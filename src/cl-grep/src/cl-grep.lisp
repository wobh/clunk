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
  (match-count 0)
  (null-byte (string (or #+clisp #\Null "\\0")))
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
  (only-show-stream-name nil)
  (show-null-byte-after-stream-name nil)
  (show-match-count nil)
  (max-match-count nil)
  (max-stream-match-count nil)
  (ignore-file-errors nil)
  (invert-match nil)
  (ignore-case nil)
  (match-test #'string=)
  (only-show-match nil)
  (patterns ()))

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
                   (null (string= str1 str2))))))
   (list '("-i" "--ignore-case")
         'boolean
         (lambda ()
           "Ignore case in matches."
           (setf (ignore-case *settings*) nil)
           (if (invert-match *settings*)
               (setf (match-test *settings*)
                     (lambda (str1 str2)
                       (null (string-equal str1 str2))))
               (setf (match-test *settings*) #'string-equal))))
   (list '("-o" "--only-matching")
         'boolean
         (lambda ()
           "Show only matching part of the line"
           (setf (only-show-match *settings*) t)))
   (list '("-e" "--regexp-pattern")
         'string
         (lambda (pattern)
           "Specify a pattern used for search."
           (pushnew pattern (patterns *settings*))))
   (list '("-f" "--file")
         'string
         (lambda (file)
           "Read pattern(s) from file."
           (with-open-file (stream file)
             (loop
                for pattern = (read-line stream nil)
                until (null pattern)
                do (pushnew pattern (patterns *settings*))))))
   (list '("-q" "--quiet" "--silent")
         'boolean
         (lambda ()
           "Quiet mode. Output nothing. Match once, status 0; match none, status 1."
           (setf (output-stream *settings*) (make-broadcast-stream)
                 (max-match-count *settings*) 1)))
   (list '("--null")
         'boolean
         (lambda ()
           "Print zero-byte after the file name."
           (setf (show-null-byte-after-stream-name *settings*) t)))
   (list '("-l" "--files-with-matches")
         'boolean
         (lambda ()
           "Print only names of files with matches"
           (setf (max-stream-match-count *settings*) 1
                 (show-current-stream-name *settings*) 1
                 (only-show-stream-name *settings*) t))))
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

;; (with-accessors ((snamep show-input-stream-name)
;;                  (matchp show-match-count)
;;                  (nullbp show-null-byte))
;;     *settings*
;;   (format stream "~&~@[~A~@[~A]:~]~A~%"
;;           (when snamep
;;             (input-stream-name *messages*))
;;           (when nullbp
;;             (null-char *messages*))
;;           (if matchp
;;               (match-count *messages*)
;;               text)))

;; TODO streamline the control flow of printing.

(defun setup-output ()
  (with-output-to-string (str)
    (princ "~&" str)
    (when (show-current-stream-name *settings*)
      (princ (input-stream-name *messages*) str)
      (when (show-null-byte-after-stream-name *settings*)
        (princ (null-byte *messages*) str)))
    (unless (only-show-stream-name *settings*)
      (when (show-current-stream-name *settings*)
        (unless (show-null-byte-after-stream-name *settings*)
          (princ ":" str)))
      (cond
        ((show-match-count *settings*)
         (princ "~D" str))
        (t
         ;; (when (show-line-number *settings*) (princ "~D:" str))
         ;; (when (show-byte-offset *settings*) (princ "~D:" str))
         (princ "~A" str))))
    (princ "~%" str)))

;; (defun setup-output (o-format)
;;   (with-output-to-string (str)
;;     (princ "~&" str)
;;     (when (show-current-stream-name *settings*)
;;       (format (output-stream *settings*)
;;               (filename *messages*)
;;               (input-stream-name *messages*)
;;               (show-null-byte-after-stream-name *settings*)
;;               (null-byte *messages*)))
;;     (princ o-format str)
;;     (princ "~%" str)))

(defun write-match (text)
  (format (output-stream *settings*)
          (setup-output)
          text))

(defun write-count (count)
  (format (output-stream *settings*)
          (setup-output)
          count))

(defun count-match ()
  (incf (match-count *messages*)))

(defun max-matches-counted-p ()
  (when (max-match-count *settings*)
    (= (max-match-count *settings*)
       (match-count *messages*))))

(defun write-stream ()
  (format (output-stream *settings*)
          (setup-output)))

(defun handle-match (text)
  (when (or (show-match-count *settings*)
            (max-match-count *settings*))
      (count-match))
  (unless (show-match-count *settings*)
    (if (only-show-stream-name *settings*)
        (write-stream)
        (write-match text))))

(defun seek-pattern (text)
  (loop
     for pattern in (patterns *settings*)
     with count = 0
     do
       (when (search pattern text :test (match-test *settings*))
         (unless *status* (setf *status* 0))
         (incf count)
         (handle-match
          (if (only-show-match *settings*) pattern text)))
     finally (return count)))

(defun max-stream-matches-counted-p (match-stream-count)
  (when (max-stream-match-count *settings*)
    (<= (max-stream-match-count *settings*)
        match-stream-count)))

(defun scan-stream (stream)
  (loop
     for line = (read-line stream nil)
     then (read-line stream nil)
     with count = 0
     do (incf count (seek-pattern line))
     until (or (null line)
               (max-stream-matches-counted-p count)
               (max-matches-counted-p))
     finally (when (show-match-count *settings*)
               (write-count (match-count *messages*)))))

(defun handle-file-error (err)
  (when (ignore-file-errors *settings*)
    (return)))

(defun handle-files (files)
  (unless (never-show-stream-name *settings*)
    (when (or (< 1 (length files))
              (always-show-stream-name *settings*))
      (setf (show-current-stream-name *settings*) t)))
  (dolist (file files)
    (handler-bind ((file-error #'handle-file-error))
      (setf (input-stream-name *messages*) file)
      (with-open-file (stream file)
        (scan-stream stream)))))

(defun handle-stream ()
  (setf (input-stream-name *messages*) "(standard-input)"
        (output-stream *settings*) *standard-output*)
  (with-open-stream (stream *standard-input*)
    (scan-stream stream)))

(defun main (&rest files)
  (if files
      (handle-files files)
      (handle-stream))
  (unless *status* (setf *status* 1))
  (grep-exit))

(defun handle-args (args)
  (let ((args (getopts args)))
    (unless (patterns *settings*)
      (pushnew (pop args) (patterns *settings*)))
    (unless (patterns *settings*)
      (err-exit 2 (usage-str *messages*)))
    args))

(apply #'main (handle-args *args*))
