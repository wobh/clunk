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

(defstruct (settings (:conc-name nil))
  "Settings object for accessing options and parameters")

(defparameter *settings*
  '((("V" "-V" "--version") version boolean "show version"))
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

(defun err-exit (status message)
  (setf *status* status)
  (format *error-output* message)
  (grep-exit))

(defun main (&optional pattern files)
  (unless pattern
    (err-exit 2 (mesg-usage *messages*)))
  (if files
      (loop
	 for file in files
	 do
	   (with-open-file (stream file)
	     (scan-stream stream pattern)))
      (with-open-stream (stream *standard-input*)
        (scan-stream stream pattern)))
  (unless *status* (setf *status* 1))
  (grep-exit))


;;; Setup

(let ((*args* (or #+clisp EXT:*ARGS* nil)))
  (unless *args*
    (err-exit 2 (mesg-usage *messages*)))
  (let ((opt-stop "--")
	(opt nil)
	(optchain nil))
    (labels ((string-or-nil-p (val)
	       (or (typep val 'null)
		   (typep val 'string)))
	     (optp (str)
	       (when (< 1 (length str))
		   (and (char= #\- (char str 0))
		     (not (string= str "--")))))
	     (optchainp (str)
	       (let ((reststr (subseq str 1)))
		 (when (and (not (optp reststr))
			    (> 1 (length reststr)))
		   str)))
	     (find-setting (opt)
	       (find opt settings :key 'first
		     :test (lambda (str params)
			     (find str params :test 'equal))))
	     (end-optchain ()
	       (setq optchain nil)
	       (setq opt (pop *args*))))
      (tagbody
	 (go setup-opt)
       setup-opt
	 (cond ((string< "" optchain)
		(setf opt (format nil "-~C" (char opt 0)))
		(setq optchain (subseq optchain 1))
		(when (string= "" optchain)
		  (setq optchain nil)))
	       ((null optchain)
		(setf opt (pop *args*))
		(setq optchain (optchainp opt))
		(when optchain
		  (go setup-opt)))
	       (t
		(check-type optchain (satisfies string-or-nil-p))))
	 (go check-opt)
       check-opt
	 (if (optp opt)
	     (go proc-opt)
	     (go make-args))
       proc-opt
	 
	 (cond ((null optchain)
		(setq opt (find-setting opt))
		(go proc-setting))
	       ((string< "" optchain)
		(setq opt (find-setting ))
		
		
		(go proc-setting))
	       (t
		))
       proc-setting
	 (destructuring-bind (var type) (rest (butlast opt))
	   (setf (var *settings*
		      (if (eq type 'boolean)
			  (not (var *settings))
			  (pop *args*)))))
	 (when *args*
	   (setq opt (if optchain
			 (char 0 (format nil "-~C" (char opt 0)))
			 (pop *args*)))
	   (cond ((optp opt)
		  (setq optchain (optchainp opt))
		  (when optchain
		    (setf opt (delete ))))
		 (t
		  (go make-args))))
       make-args
	 
	 )
      (loop
	 for opt in args
	 until
	   (or (not (optp opt))
	       (string= opt opt-stop))
	 do
	   (when (optchainp opt)
	     (map 'list opt
		  (lambda ())))
	   (let ((setting (find-setting opt)))
	     )))))

(defparameter *args*
  (or #+clisp EXT:*ARGS* nil)
  "Arguments passed to CL-Grep.")



(apply #'main (getopts *args*))
