;;;; tests/package.lisp

(fiasco:define-test-package #:cmu-infix-tests
  (:use #:cmu-infix)
  
  ;; tests.lisp
  (:export
   #:run-tests))

