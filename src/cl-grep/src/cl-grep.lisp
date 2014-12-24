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
  (output-format nil)
  (match-writer nil)
  (stream-name-match-separator #\:)
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
  (only-show-stream-names-without-matches nil)
  (show-null-byte-after-stream-name nil)
  (show-match-line-number nil)
  (show-match-position nil)
  (show-match-count nil)
  (max-match-count nil)
  (max-stream-match-count nil)
  (ignore-file-errors nil)
  (invert-match nil)
  (ignore-case nil)
  (line-match nil)
  (match-test nil)
  (match-handler nil)
  (stream-handler nil)
  (scan-handler nil)
  (only-show-match nil)
  (patterns ()))

(defparameter *settings*
  (make-settings)
  "Settings and behavior switches")


;;; Definitions

(defmacro make-matcher (function &rest options)
  "Returns a function of two arguments to match first argument to second."
  (let ((pattern (gensym "PATTERN"))
        (text (gensym "TEXT")))
    `(lambda (,pattern ,text)
       "Matches first argument with second."
       (,function ,pattern ,text ,@options))))

(defun setup-matcher (&key fixed-string ignore-case line-match)
  (cond ((and (not fixed-string) (not ignore-case) (not line-match))
         (make-matcher search :test #'string=))
        ((and fixed-string (not ignore-case) (not line-match))
         (identity (make-matcher search :test #'string=)))
        ((and (not fixed-string) ignore-case (not line-match))
         (make-matcher search :test #'string-equal))
        ((and fixed-string ignore-case (not line-match))
         (identity (make-matcher search :test #'string-equal)))
        ((and (not fixed-string) (not ignore-case) line-match)
         (make-matcher string=))
        ((and fixed-string (not ignore-case) line-match)
         (identity (make-matcher string=)))
        ((and (not fixed-string) ignore-case line-match)
         (make-matcher string-equal))
        ((and fixed-string ignore-case line-match)
         (identity (make-matcher string-equal)))))

(defun setup-output-format ()
  (with-output-to-string (str)
    (princ "~&" str)
    (when (show-current-stream-name *settings*)
      (princ (input-stream-name *messages*) str)
      (when (show-null-byte-after-stream-name *settings*)
        (princ (stream-name-match-separator *messages*) str)))
    (unless (only-show-stream-name *settings*)
      (when (show-current-stream-name *settings*)
        (unless (show-null-byte-after-stream-name *settings*)
          (princ (stream-name-match-separator *messages*) str)))
      (cond
        ((show-match-count *settings*)
         (princ "~D" str))
        (t
         (when (show-match-line-number *settings*)
           (princ "~D:" str))
         (when (show-match-position *settings*)
           (princ "~D:" str))
         (princ "~A" str))))
    (princ "~%" str)))

(defun make-match-writer ()
  (cond
    ((only-show-stream-name *settings*)
     (lambda ()
       (format (output-stream *settings*)
               (output-format *messages*)
               (input-stream-name *messages*))))
    ((show-match-count *settings*)
     (lambda (match-count)
       (format (output-stream *settings*)
               (output-format *messages*)
               match-count)))
    ((and (show-match-line-number *settings*)
          (show-match-position *settings*))
     (lambda (match-text line-number match-position)
       (format (output-stream *settings*)
               (output-format *messages*)
               line-number
               match-postion
               match-text)))
    ((or (show-match-line-number *settings*)
         (show-match-position *settings*))
     (lambda (match-text match-marker)
       (format (output-stream *settings*)
               (output-format *messages*)
               match-marker
               match-text)))
    (t
     (lambda (match-text)
       (format (output-stream *settings*)
               (output-format *messages*)
               match-text)))))

(defun make-match-handler ()
  (unless (only-show-stream-names-without-matches *settings*)
    (when (or (show-match-count *settings*)
              (max-match-count *settings*))
      (lambda () (count-match)))
    (unless (show-match-count *settings*)
      (cond ((only-show-stream-name *settings*)
             (lambda () (write-match)))
            ((and (show-match-line-number *settings*)
                  (show-match-position *settings*))
             (lambda (match-text match-line match-position)
               (write-match match-text match-line match-position)))
            ((show-match-line-number *settings*)
             (lambda (match-text match-line)
               (write-match match-text match-line)))
            ((show-match-position *settings*)
             (lambda (match-text match-position)
               (write-match match-text match-position)))
            (t
             (lambda (match-text)
               (write-match match-text)))))))

(defun make-scan-handler ()
  (cond ((show-match-count *settings*)
         (lambda ()
           (write-match (output-format *messages*) (match-count *messages*))))
        ((only-show-stream-names-without-matches *settings*)
         (lambda ()
           (when (zerop count)
             (write-match))))
        (t (lambda () nil))))

(defun after-getopts ()
  (setf (match-test *settings*)
        (funcall (if (invert-match *settings*)
                     #'complement
                     #'identity)
                 (setup-matcher :fixed-string nil
                                :ignore-case (ignore-case *settings*)
                                :line-match (line-match *settings*)))
        (output-format *messages*) (setup-output-format)
        (match-writer *messages*) (make-match-writer)
        (match-handler *settings*) (make-match-handler)
        (stream-handler *settings*) (make-stream-handler)))



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
   (list '("-F" "--fixed-string")
         'boolean
         (lambda ()
           "Use fixed string for matching instead of regexp. NOOP"
           ;; TODO: setup regexp patterns
           (values)))
   (list '("-v" "--invert-match")
         'boolean
         (lambda ()
           "Show only lines that do not match patterns."
           (setf (invert-match *settings*) t)))
   (list '("-i" "--ignore-case")
         'boolean
         (lambda ()
           "Ignore case in matches."
           (setf (ignore-case *settings*) t)))
   (list '("-x" "--line-regexp")
         'boolean
         (lambda ()
           "Match on whole input line."
           (setf (line-match *settings*) t)))
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
           (setf (show-null-byte-after-stream-name *settings*) t)
           (setf (stream-name-match-separator *messages*)
                 (null-byte *messages*))))
   (list '("-l" "--files-with-matches")
         'boolean
         (lambda ()
           "Print only names of files with matches."
           (setf (max-stream-match-count *settings*) 1
                 (show-current-stream-name *settings*) t
                 (only-show-stream-name *settings*) t
                 (only-show-stream-names-without-matches *settings*) nil
                 (show-match-line-number *settings*) nil)))
   (list '("-L" "--files-without-matches")
         'boolean
         (lambda ()
           "Print only names of files without matches."
           (setf (max-stream-match-count *settings*) 1
                 (show-current-stream-name *settings*) t
                 (only-show-stream-name *settings*) t
                 (only-show-stream-names-without-matches *settings*) t
                 (show-match-line-number *settings*) nil)))
   (list '("-n" "--line-number")
         'boolean
         (lambda ()
           "Show line number of match."
           (unless (or (show-match-count *settings*)
                       (only-show-stream-name *settings*)
                       (only-show-stream-names-without-matches *settings*))
             (setf (show-match-line-number *settings*) t))))
   (list '("-b" "--byte-offset")
         'boolean
         (lambda ()
           "Show byte offset of match (actually file position of match line)."
           (setf (show-match-position *settings*) t))))
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
       (unless args (loop-finish))
       (setf (values opt optchain optvalue)
             (getopt (or optchain (pop args))))
       (when optvalue (push optvalue args))
       (cond
         ((and (null opt) (null optchain))
          (loop-finish))
         ((and (null opt) (stringp optchain))
          (push optchain args)
          (loop-finish))
         ((stringp opt)
          (destructuring-bind (type func)
              (rest (find-option opt options))
            (if (eq type 'boolean)
                (funcall func)
                (let ((arg (if optchain (subseq optchain 1) (pop args))))
                  (funcall func arg))))))
     finally (progn
               (after-getopts)
               (return args))))

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

;; TODO streamline the control flow of printing.

(defun write-match (&rest args)
  (apply (match-writer *messages*) args))

(defun count-match ()
  (incf (match-count *messages*)))

(defun max-matches-counted-p ()
  (when (max-match-count *settings*)
    (= (max-match-count *settings*)
       (match-count *messages*))))

(defun handle-match (&rest args)
  (apply (match-handler *settings*) (remove-if #'null args)))

(defun seek-pattern (text &key line-number line-position)
  (loop
     for pattern in (patterns *settings*)
     with match-position = nil
     with count = 0
     while (null match-position)
     do
        (when (setf match-position (funcall (match-test *settings*) pattern text))
          (unless *status* (setf *status* 0))
          (incf count)
          (handle-match (if (only-show-match *settings*) pattern text)
                        (when (show-match-line-number *settings*) line-number)
                        (when (show-match-position *settings*)
                          (if (typep match-position 'integer)
                              line-position
                              line-position))))
     finally (return count)))

(defun max-stream-matches-counted-p (match-stream-count)
  (when (max-stream-match-count *settings*)
    (<= (max-stream-match-count *settings*)
        match-stream-count)))

(defun scan-stream (stream)
  (loop
     for line-number = 1 then (incf line-number)
     for position = (file-position stream) then (file-position stream)
     for line = (read-line stream nil)
     then (read-line stream nil)
     with count = 0
     until (or (null line)
               (max-stream-matches-counted-p count)
               (max-matches-counted-p))
     do (when (seek-pattern line
                            :line-number line-number
                            :line-position position)
          (incf count))
        finally (funcall (scan-handler *settings*))))

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

(defun handle-stdin ()
  (setf (input-stream-name *messages*) "(standard-input)"
        (output-stream *settings*) *standard-output*)
  (with-open-stream (stream *standard-input*)
    (scan-stream stream)))

(defun handle-patterns (args)
  (let ((args (getopts args)))
    (unless (patterns *settings*)
      (pushnew (pop args) (patterns *settings*)))
    (unless (patterns *settings*)
      (err-exit 2 (usage-str *messages*)))
    args))

(defun handle-args (args)
  (let ((files (handle-patterns args)))
    (if files
        (handle-files files)
        (handle-stdin))))

(defun main (args)
  (handle-args)
  (unless *status* (setf *status* 1))
  (grep-exit))

(apply #'main *args*)
