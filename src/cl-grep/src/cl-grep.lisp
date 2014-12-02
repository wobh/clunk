#!/usr/local/bin/clisp
;; -*- mode: lisp; -*-

;;;; cl-grep.lisp

;;;; AUTHORS:
;;;; William Clifford [wc] wobh@yahoo.com

;;;; DESCRIPTION:
;;;; A basic grep. Prints lines that match pattern to standard-output.


;;; Messages

(defstruct (messages (:conc-name mesg-))
  "Messages object for accessing messages to user"
  (program "cl-grep")
  (usage "usage: cl-grep [pattern] [file]")
  (version "cl-grep: 0.0.1")
  (filename "~A:")
  (match "~A")
  (count "~D")
  (i-stream-name nil)
  (o-stream (make-string-output-stream)))

(defparameter *messages*
  (make-messages)
  "Messages to user")


;;; Conditions

(define-condition no-pattern (error)
  ()
  (:report (lambda (stream)
             (mesg-usage *messages*))))

(define-condition invalid-option (error)
  ((opt :reader given :initarg :given))
  (:report (lambda (condition stream)
             (format stream "invalid option -- ~A~%~A"
                     (given condition)
                     (mesg-usage *messages*)))))


;;; Status and Settings

(defparameter *status*
  nil
  "Program status")

(defun grep-exit (&optional (status *status*))
  #+clisp (EXT:exit status))

(defun err-exit (status message)
  (setf *status* status)
  (format *error-output* message)
  (grep-exit))

(defstruct (settings (:conc-name nil))
  "Settings object for accessing options and parameters"
  (show-current-stream-name nil)
  (always-show-stream-name nil)
  (never-show-stream-name nil)
  (show-match-count nil))

(defparameter *settings* (make-settings))

(defparameter *options*
  (list
   (list '("--help")
         'boolean
         (lambda ()
           "Show help and exit."
           (err-exit 2 (mesg-usage *messages*))))
   (list '("-V" "--version")
         'boolean
         (lambda ()
           "Show version and exit."
           (err-exit 0 (mesg-version *messages*))))
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
           "Show count of matches"
           (setf (show-match-count *settings*) 0))))
  "Options and parameters.

An option definition list is a list with the following elements:

- list of option's parameters
- option argument type
- lambda to set option value in *settings*
- default arguments to lambda, if any

(destructuring-bind () ")

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
    (err-exit 2 (mesg-usage *messages*)))
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
                  (funcall func
                           (case type
                             (integer
                              (handler-bind ((parse-error
                                               (error 'invalid-option
                                                      :given arg)))
                                (parse-integer arg)))
                             (string arg))))))))))

(defparameter *args*
  (or #+clisp EXT:*ARGS* nil)
  "Arguments passed to CL-Grep.")



;;; CL-Grep

(defun setup-output (o-format)
  (with-output-to-string (str)
    (princ "~&" str)
    (when (show-current-stream-name *settings*)
      (format str (mesg-filename *messages*)
              (current-stream-name *settings*)))
    (princ o-format str)
    (princ "~%" str)))

(defun write-match (text)
  (format (mesg-o-stream *messages*)
          (setup-output (mesg-match *messages*))
          text))

(defun write-count (count)
  (format (mesg-o-stream *messages*)
          (setup-output (mesg-format *messages*))
          count))

(defun seek-pattern (pattern text)
  (when (search pattern text)
    (unless *status* (setf *status* 0))
    (if (show-match-count *settings*)
        (incf (show-match-count *settings*))
        (write-match text))))

(defun scan-stream (pattern stream)
  (do ((line (read-line stream nil) 
             (read-line stream nil)))
      ((null line))
    (seek-pattern pattern line)))

(defun main (&optional pattern &rest files)
  (unless pattern
    (err-exit 2 (mesg-usage *messages*)))
  (cond
    (files
     (unless (never-show-stream-name *settings*)
       (when (or (< 1 (length files))
                 (always-show-stream-name *settings*))
         (setf (show-current-stream-name *settings*) t)))
     (dolist (file files)
       (setf (i-stream-name *messages*) file)
       (with-open-file (stream file)
         (scan-stream pattern stream))
       (when (show-match-count)
         (princ (setup-output )))
       (princ (get-output-stream-string (o-stream *messages*)))))
    (t
     (setf (i-stream-name *messages*) "(standard-input)")
     (with-open-stream (stream *standard-input*)
       (scan-stream pattern stream))
     (princ (get-output-stream-string (o-stream *messages*)))))
  (unless *status* (setf *status* 1))
  (grep-exit))

(apply #'main (getopts *args*))
