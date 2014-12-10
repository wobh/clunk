#!/usr/local/bin/clisp
;; -*- mode: lisp; -*-

;;;; cl-grep.lisp

;;;; AUTHORS:
;;;; William Clifford [wc] wobh@yahoo.com

;;;; DESCRIPTION:
;;;; A basic grep. Prints lines that match pattern to standard-output.


;;; Messages

(defstruct (messages (:conc-name nil))
  "Messages object for accessing messages to user"
  (program "cl-grep")
  (usage-str "usage: cl-grep [pattern] [file]")
  (version "cl-grep: 0.0.1")
  (filename "~A:")
  (match-fmt "~A")
  (count-fmt "~D")
  (match-count 0)
  (input-stream-name nil))

(defparameter *messages*
  (make-messages)
  "Messages to user")


;;; Settings

(defstruct (settings (:conc-name nil))
  "Settings object for accessing options and parameters"
  (output-stream *standard-output*)
  (show-current-stream-name nil)
  (always-show-stream-name nil)
  (never-show-stream-name nil)
  (show-match-count nil)
  (max-match-count nil)
  (ignore-file-errors nil)
  (invert-match nil)
  (match-test #'string=))

(defparameter *settings*
  (make-settings)
  "Settings and behavior switches")


;;; Options

(defparameter *options*
  (list
   (list '("--help")
         'boolean
         (lambda ()
           "Show help and exit."
           (err-exit 2 (usage *messages*))))
   (list '("-V" "--version")
         'boolean
         (lambda ()
           "Show version and exit."
           (err-exit 0 (version *messages*))))
   (list '("-H")
         'boolean
         (lambda ()
           "Always show filenames."
           (setf (always-show-stream-name *settings*) t)))
   (list '("-h" "--no-filename")
         'boolean
         (lambda ()
           "Never show filenames."
           (setf (never-show-stream-name *settings*) t)))
   (list '("-c" "--count")
         'boolean
         (lambda ()
           "Show count of matches."
           (setf (show-match-count *settings*) t)))
   (list '("-m" "--max-count")
         '(integer 0)
         (lambda (max)
           "Stop reading file after number of matches."
           (setf (max-match-count *settings*)
                 (parse-integer max))))
   (list '("-s" "--no-messages")
         'boolean
         (lambda ()
           "Silent mode. Suppress error messages from unreadable or nonexistent files."
           (setf (ignore-file-errors *settings*) t)))
   (list '("-v" "--invert-match")
         'boolean
         (lambda ()
           "Show only lines that do not match patterns."
           (setf (invert-match *settings*) t)
           (setf (match-test *settings*)
                 (lambda (str1 str2)
                   (null (string= str1 str2)))))))
  "Options and parameters.

An option definition list is a list with the following elements:

- list of option's parameters
- option argument type
- lambda to set option value in *settings*
- default arguments to lambda, if any

(destructuring-bind
    (params typespec setoptf &optional default-val) optlist
  ...)")

(define-condition invalid-option (error)
  ((opt :reader given :initarg :given))
  (:report (lambda (condition stream)
             (format stream "invalid option -- ~A~%~A"
                     (given condition)
                     (usage-str *messages*)))))

(defun find-option (opt &optional (options *options*))
  (or (find opt options
            :key 'first
            :test (lambda (str params)
                    (find str params :test 'equal)))
      (error 'invalid-option :given opt)))

(defun getopt (arg)
  (check-type arg string)
  (let ((pos (position #\= arg))
        (idx (string< "--" arg))
        (val nil))
    (when pos
      (setf val (subseq arg (1+ pos)))
      (setf arg (subseq arg 0 pos)))
    (when idx
      (ecase idx
        (0 (values nil arg val))
        (1 (let ((opt (subseq arg idx)))
             (values
              (format nil "-~C" (char opt 0))
              (when (< 1 (length opt))
                (format nil "-~A" (subseq opt 1)))
              val)))
        (2 (values arg nil val))))))

(defun getopts (&optional (args *args*) (options *options*))
  (unless args
    (err-exit 2 (usage-str *messages*)))
  (loop
     with opt
     with optchain
     with optvalue
     do
       (unless args (return))
       (setf (values opt optchain optvalue)
             (getopt (or optchain (pop args))))
       (when optvalue (push optvalue args))
       (cond
         ((and (null opt) (null optchain))
          (return args))
         ((and (null opt) (stringp optchain))
          (push optchain args)
          (return args))
         ((stringp opt)
          (destructuring-bind (type func)
              (rest (find-option opt options))
            (if (eq type 'boolean)
                (funcall func)
                (let ((arg (if optchain (subseq optchain 1) (pop args))))
                  (funcall func arg))))))))

(defparameter *args*
  (or #+clisp EXT:*ARGS* nil)
  "Arguments passed to CL-Grep.")


;;; Status

(defparameter *status*
  nil
  "Program status")

(defun grep-exit (&optional (status *status*))
  #+clisp (EXT:exit status))

(defun err-exit (status message)
  (setf *status* status)
  (format *error-output* message)
  (grep-exit))


;;; CL-Grep

;; (when (show-match-count *settings*)
;;   (write-count (match-count *messages*)))
;; (princ (get-output-stream-string (o-stream *messages*)))

;; (format stream "~&~@[~A:~]~A~%"
;;         (and (show-current-stream-name *settings)
;;              (inpout-stream-name *settings*))
;;         (if (show-match-count *settings*)
;;             (match-count *messages*)
;;             text))

;; TODO streamline the control flow of printing.

(defun setup-output (o-format)
  (with-output-to-string (str)
    (princ "~&" str)
    (when (show-current-stream-name *settings*)
      (format str (filename *messages*)
              (input-stream-name *messages*)))
    (princ o-format str)
    (princ "~%" str)))

(defun write-match (text)
  (format (output-stream *settings*)
          (setup-output (match-fmt *messages*))
          text))

(defun write-count (count)
  (format (output-stream *settings*)
          (setup-output (count-fmt *messages*))
          count))

(defun count-match ()
  (incf (match-count *messages*)))

(defun max-matches-counted-p ()
  (when (max-match-count *settings*)
    (= (max-match-count *settings*)
       (match-count *messages*))))

(defun handle-match (pattern text)
  (when (or (show-match-count *settings*)
            (max-match-count *settings*))
      (count-match))
  (unless (show-match-count *settings*)
    (write-match text)))

(defun seek-pattern (pattern text)
  (when (search pattern text :test (match-test *settings*))
    (unless *status* (setf *status* 0))
    (handle-match pattern text)))

(defun scan-stream (pattern stream)
  (loop
    for line = (read-line stream nil)
      then (read-line stream nil)
    do (seek-pattern pattern line)
    until (or (null line)
              (max-matches-counted-p))
    finally (when (show-match-count *settings*)
              (write-count (match-count *messages*)))))

(defun main (&optional pattern &rest files)
  (unless pattern
    (err-exit 2 (usage-str *messages*)))
  (cond
    (files
     (unless (never-show-stream-name *settings*)
       (when (or (< 1 (length files))
                 (always-show-stream-name *settings*))
         (setf (show-current-stream-name *settings*) t)))
     (dolist (file files)
       (handler-bind
           ((file-error
             (lambda (err)
               (when (ignore-file-errors *settings*)
                 (return)))))
         (setf (input-stream-name *messages*) file)
         (with-open-file (stream file)
           (scan-stream pattern stream)))))
    (t
     (setf (input-stream-name *messages*) "(standard-input)"
           (output-stream *settings*) *standard-output*)
     (with-open-stream (stream *standard-input*)
       (scan-stream pattern stream))))
  (unless *status* (setf *status* 1))
  (grep-exit))

(apply #'main (getopts *args*))
