;;;; cmu-infix.asd

(asdf:defsystem #:cmu-infix
  :description "Mathematical infix notation for Common Lisp."
  :author "Mark Kantrowitz"
  :maintainer "Robert Smith <robert@rigetti.com>"
  :license "Custom (See LICENSE.txt)"
  :in-order-to ((asdf:test-op (asdf:test-op #:cmu-infix-tests)))
  :depends-on (#:named-readtables)
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:file "cmu-infix")))
