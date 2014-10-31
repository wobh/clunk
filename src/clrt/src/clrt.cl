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


;;; Defining and locating tests.

(defstruct (test (:conc-name nil))
  "Data structure representing a test case"
  (name nil)
  (documentation nil)
  (body (assert nil))
  (most-recent-result nil)
  (expected-result-type :passed)
  (tags '()))

(defun test-boundp (symbol)
  "Return non-nil if SYMBOL names a test."
  (and (get symbol 'test) t))

(defun get-test (symbol)
  "If SYMBOL names a test, return that.  Signal an error otherwise."
  (unless (test-boundp symbol) (error "No test named ~S" symbol))
  (get symbol 'test))

(defun set-test (symbol definition)
  "Make SYMBOL name the test DEFINITION, and return DEFINITION."
  (when (eq symbol 'nil)
    ;; We disallow nil since `test-at-point' and related functions
    ;; want to return a test name, but also need an out-of-band value
    ;; on failure.  Nil is the most natural out-of-band value; using 0
    ;; or "" or signaling an error would be too awkward.
    ;;
    ;; Note that nil is still a valid value for the `name' slot in
    ;; ert-test objects.  It designates an anonymous test.
    (error "Attempt to define a test named nil"))
  (put symbol 'test definition)
  definition)

(defun make-test-unbound (symbol)
  "Make SYMBOL name no test.  Return SYMBOL."
  (remprop symbol 'test)
  symbol)

;;; TODO: are TEST-BOUNDP, GET-TEST, SET-TEST, MAKE-TEST-UNBOUND
;;; necessary? I don't think so, but it's somewhat vague to me right
;;; now. Remove or refactor as needed. [wc 2014-10-29]


(defun parse-keys-and-body (keys-and-body)
    "Split KEYS-AND-BODY into keyword-and-value pairs and the remaining body.

KEYS-AND-BODY should have the form of a property list, with the
exception that only keywords are permitted as keys and that the
tail -- the body -- is a list of forms that does not start with a
keyword.

Returns a two-element list containing the keys-and-values plist
and the body."
  (let ((extracted-key-accu '())
        (remaining keys-and-body))
    (loop
       while (and (consp remaining) (keywordp (first remaining)))
       do
	 (let ((keyword (pop remaining)))
	   (unless (consp remaining)
	     (error "Value expected after keyword ~S in ~S"
		    keyword keys-and-body))
	   (when (assoc keyword extracted-key-accu)
	     (warn "Keyword ~S appears more than once in ~S" keyword
		   keys-and-body))
	   (push (cons keyword (pop remaining)) extracted-key-accu)))
    (setq extracted-key-accu (nreverse extracted-key-accu))
    (list (loop for (key . value) in extracted-key-accu
                collect key
                collect value)
          remaining)))
;; TODO: I have a feeling DESTRUCTURING-BIND could simplify this.
;; Investigate, refactor. [wc 2014-10-29]

;; TODO: Would it be worth defining conditions for any of the above or
;; following errors and warnings? Probably. Investigate, refactor. [wc
;; 2014-1029]

(defmacro ert-deftest (name () &body docstring-keys-and-body)
  "Define NAME (a symbol) as a test.

BODY is evaluated as a `progn' when the test is run.  It should
signal a condition on failure or just return if the test passes.

`should', `should-not' and `should-error' are useful for
assertions in BODY.

Use `ert' to run tests interactively.

Tests that are expected to fail can be marked as such
using :expected-result.  See `ert-test-result-type-p' for a
description of valid values for RESULT-TYPE.

\(fn NAME () [DOCSTRING] [:expected-result RESULT-TYPE] \
\[:tags '(TAG...)] BODY...)"
  (let ((documentation nil)
        (documentation-supplied-p nil))
    (when (stringp (first docstring-keys-and-body))
      (setq documentation (pop docstring-keys-and-body)
            documentation-supplied-p t))
    (destructuring-bind ((&key (expected-result nil expected-result-supplied-p)
                               (tags nil tags-supplied-p))
                         body)
        (parse-keys-and-body docstring-keys-and-body)
      `(progn
         (set-test ',name
		   (make-test
		    :name ',name
		    ,@(when documentation-supplied-p
                            `(:documentation ,documentation))
		    ,@(when expected-result-supplied-p
                            `(:expected-result-type ,expected-result))
		    ,@(when tags-supplied-p
                            `(:tags ,tags))
		    :body (lambda () ,@body)))
         ;; This hack allows `symbol-file' to associate `ert-deftest'
         ;; forms with files, and therefore enables `find-function' to
         ;; work with tests.  However, it leads to warnings in
         ;; `unload-feature', which doesn't know how to undefine tests
         ;; and has no mechanism for extension.
         ;; (push '(ert-deftest . ,name) current-load-list)
         ;; ',name))))

	 ;; TODO: I'm not sure we're going to be able to do the part
	 ;; commented out. [wc 2014-10-29]
	 ))))

(defun pass ()
  "Terminate the current test and mark it passed.  Does not return."
  (throw 'pass nil))

(defun fail (data)
  "Terminate the current test and mark it failed.  Does not return.
DATA is displayed to the user and should state the reason of the failure."
  (signal 'test-failed (list data)))
