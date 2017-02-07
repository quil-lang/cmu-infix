;;;; cmu-infix-tests.asd

(asdf:defsystem #:cmu-infix-tests
  :description "Tests for the system CMU-INFIX."
  :author "Mark Kantrowitz"
  :maintainer "Robert Smith <robert@rigetti.com>"
  :license "Custom (See LICENSE.txt)"
  :depends-on (#:cmu-infix
               #:fiasco
               #:uiop)
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :cmu-infix-tests
                                           '#:run-tests))
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "tests")))

