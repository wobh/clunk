;;;; clrt.cl --- Common Lisp Regression Testing

;;;; Copyright (C) 2014 William Clifford

;;;; Author: William Clifford <wobh@yahoo.com>

(in-package "CL-USER")

(defpackage "ORG.WOBH.CLRT"
  ((:nicknames "CLRT")
   (:export "DEFTEST" 
	    "RUN-TESTS-BATCH" "RUN-TESTS-BATCH-AND-EXIT"
	    "RUN-TESTS-INTERACTIVELY")))

(in-package "CLRT")
