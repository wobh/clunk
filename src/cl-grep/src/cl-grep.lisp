#!/usr/local/bin/clisp
;; -*- mode: common-lisp; -*-

;;;; cl-grep.lisp

;;;; AUTHORS:
;;;; William Clifford [wc] wobh@yahoo.com

;;;; DESCRIPTION:
;;;; A basic grep. Prints lines that match pattern to standard-output.

(defparameter *usage*
  "usage: cl-grep [pattern] [file]"
  "Messages")

(defparameter *args* (or #+clisp EXT:*ARGS* nil)
  "TODO: strive for implementation independence.")

(defparameter *pattern* (first  *args*))
(defparameter *file*    (second *args*))

(defun scan-stream (stream pattern)
  (do ((line (read-line stream nil) 
             (read-line stream nil)))
      ((null line))
      (when (search pattern line)
        (format t "~&~A~%" line))))

(defun main ()
  (unless *args*
    (format t *usage*)
    (quit))
  (if *file*
      (with-open-file (stream *file*)
        (scan-stream stream *pattern*))
      (with-open-stream (stream *standard-input*)
        (scan-stream stream *pattern*))))

(main)
