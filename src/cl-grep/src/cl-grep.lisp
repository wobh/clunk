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
  (usage "usage: cl-grep [pattern] [file]")
  (version "cl-grep: 0.0.1")
  (match "~&~A~%"))

(defparameter *messages*
  (make-messages)
  "Messages to user")


;;; Settings and Status

(defstruct (settings (:conc-name opts-))
  "Settings object for accessing options and parameters")

(defparameter *settings*
  (make-settings)
  "Options and parameters")

(defparameter *status*
  nil
  "Program status")

(defun grep-exit (&optional (status *status*))
  #+clisp (EXT:exit status))


;;; Conditions

(define-condition no-pattern (error)
  ((message :reader message :initarg :message)))


;;; CL-Grep

(defun write-match (text)
  (format *standard-output*
	  (mesg-match *messages*)
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

(defun main (&optional pattern file)
  (unless pattern
    (format *error-output* (mesg-usage *messages*))
    (setf *status* 2)
    (grep-exit))
  (if file
      (with-open-file (stream file)
        (scan-stream stream pattern))
      (with-open-stream (stream *standard-input*)
        (scan-stream stream pattern)))
  (unless *status* (setf *status* 1))
  (grep-exit))


;;; Setup

(defun getopts(args)
  "Process arguments list"
  (let ((options
	 (list
	  :version '("-V" "--version"))))
      (labels ((optp (str)
		 (char= #\- (char str 0)))
	       ())
	(loop
	   for opt in args
	   do
	     ))))

(defparameter *args*
  (or #+clisp EXT:*ARGS* nil)
  "Arguments passed to CL-Grep.")

(apply #'main (getopts *args*))
