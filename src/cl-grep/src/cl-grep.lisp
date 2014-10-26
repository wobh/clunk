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

(defun main ()
  (unless *args*
    (format t *usage*)
    (quit))
  (with-open-file (stream *file*)
    (do ((line (read-line stream nil) 
               (read-line stream nil)))
        ((null line))
        (when (search *pattern* line)
          (format t "~&~A~%" line)))))

(main)
