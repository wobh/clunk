#!/usr/local/bin/clisp
;;;; hello.lisp

;;;; AUTHORS
;;;; William Clifford [wc] wobh@yahoo.com

;;;; DESCRIPTION

;;;; Hello.

(let ((name (first EXT:*ARGS*)))
  (format t "Hello~@[ ~A~]!" name))
