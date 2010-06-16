(defpackage #:ntest
  (:use :cl :nutils)
  (:documentation "Test suite using CLOS and AMOP."))

(in-package :ntest)

(defvar *test-timeout* 5
  "Default max time a test can run before timing out.")
(declaim ((integer 0 3600) *test-timeout*))

(defvar *result-equality-test* #'equalp
  "Default function to compare result equality.")
(declaim (function-designator *result-equality-test*))


(defstruct test
  "Main unittest structure.

NAME is a symbol or string to use to look up the test

DOCUMENTATION is some description of what is being tested

CATEGORY is some arbitrary grouping, may or may not be useful.

DEPENDENCIES is a list of other tests this test depends on.

CASES are the actual forms to test."
  (name (error "name required") :type symbol)
  (documentation "" :type string)
  (category nil :type symbol)
  (depends-on () :type list)
  (cases () :type list))

(defun add-test-case (unit-test case)
  "Add a test case to UNIT-TEST possibly replacing existing CASE."
  (declare (unit-test unit-test))
  (pushnew case (unit-test-cases unit-test)))

(deftype valid-test-status ()
  "Tests have 5 possible status values.

Tests are set at never-run at initialization, then when run they get set
  to one of pass failure or unexpected-failure. When a test is re-run
  before running the batch of tests, they are all cleared."
  '(member :never-run :cleared :pass :failure :unexpected-failure))

(defstruct (test-case
             (:constructor
              make-test-case
              (expression-form expected-form &optional result-predicate
                               &aux
                               (expression-function
                                (compile nil `(lambda () ,expression-form)))
                               (expected-function
                                (compile nil `(lambda () ,expected-form))))))
  (result-predicate *result-equality-test* :type function)
  expression-form
  (expression-function nil :type (or null function))
  expression-result

  expected-form
  (expected-function nil :type (or null function))
  expected-result                       ; Expect what?
  (status nil :type (or null valid-test-status)))

(defun run-test-case-expression (test-case-function)
  (declare (function test-case-function))
  (handler-case (funcall test-case-function)
    (error (condition) condition)))

(defun run-test-case (test-case)
  "Run a testcase, not paying attention to dependencies."
  (declare (test-case test-case))
  (setf (test-case-expression-result test-case)
        (run-test-case-expression (test-case-expression-function test-case)))
  (setf (test-case-expected-result test-case)
        (run-test-case-expression (test-case-expected-function test-case)))
  test-case)

(defun test-case-unexpected-failure-p (test-case)
  "True if TEST-CASE contains an unexpected failure.

A failure is considered unexpected when the `test-case-expected-result'
does not contain a `condition' and the expression signals. What this means
is a condition cannot be considered _unexpected_ if the expected-result of
the test is itself a condition."
  (declare (test-case test-case))
  (and (typep (test-case-expression-result test-case) 'condition)
       (not (typep (test-case-expected-result test-case) 'condition))))

(defun run-test-case-result-predicate (test-case)
  "Compares TEST-CASE's results with the test's defined predicate."
  (declare (test-case test-case))
  (not (not (funcall (test-case-result-predicate test-case)
                 (test-case-expression-result test-case)
                 (test-case-expected-result test-case)))))

(defun test-case-passed-p (test-case)
  "True if the TEST-CASE passed."
  (declare (test-case test-case))
  (if (test-case-unexpected-failure-p test-case)
      (values nil :unexpected-failure)
      (let ((passedp (run-test-case-result-predicate test-case)))
        (values passedp
                (if passedp :pass :failure)))))

;;; END
