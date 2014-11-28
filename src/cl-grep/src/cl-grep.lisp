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
  (match "~A"))

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
  (current-stream-name nil)
  (show-current-stream-name nil))

(defparameter *settings* (make-settings))

(defparameter *options*
  (list
   (list '("--help") 'boolean
	 (lambda ()
	   "Show help and exit"
	   (err-exit 2 (mesg-usage *messages*))))
   (list '("-V" "--version") 'boolean
	 (lambda ()
	   "show version"
	   (err-exit 0 (mesg-version *messages*)))))
  "Options and parameters")

(defun strip-opt (optstr)
  (subseq optstr (string< "--" optstr)))

(defun find-option (opt &optional (options *options*))
  (or (find opt options :key 'first
	    :test (lambda (str params)
		    (find str params :test 'equal)))
      (error 'invalid-option :given (strip-opt opt))))

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
       (setf (values opt optchain optvalue)
	     (getopt (or optchain (pop args))))
       (when optvalue (push optvalue args))
       (cond ((and (null opt) (null optchain))
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
				 (integer (parse-integer arg))
				 (string arg))))))))))

(defparameter *args*
  (or #+clisp EXT:*ARGS* nil)
  "Arguments passed to CL-Grep.")



;;; CL-Grep

(defun setup-output ()
  (with-output-to-string (str)
    (princ "~&" str)
    (when (show-current-stream-name *settings*)
      (format str (mesg-filename *messages*)
              (current-stream-name *settings*)))
    (princ (mesg-match *messages*) str)
    (princ "~%" str)))

(defun write-match (text)
  (format *standard-output*
	  (setup-output)
	  text))

(defun seek-pattern (pattern text)
  (when (search pattern text)
    (unless *status* (setf *status* 0))
    (write-match text)))

(defun scan-stream (stream pattern)
  (do ((line (read-line stream nil) 
             (read-line stream nil)))
      ((null line))
    (seek-pattern pattern line)))

(defun main (&optional pattern &rest files)
  (unless pattern
    (err-exit 2 (mesg-usage *messages*)))
  (cond
    (files
     (when (< 1 (length files))
       (setf (show-current-stream-name *settings*) t))
     (dolist (file files)
       (setf (current-stream-name *settings*) file)
       (with-open-file (stream file)
         (scan-stream stream pattern))))
    (t
     (with-open-stream (stream *standard-input*)
       (setf (current-stream-name *settings*) "(standard-input)")
       (scan-stream stream pattern))))
  (unless *status* (setf *status* 1))
  (grep-exit))

(apply #'main (getopts *args*))

