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

