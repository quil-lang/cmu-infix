;;;; package.lisp

(defpackage #:cmu-infix
  (:use #:cl #:named-readtables)

  (:export
   #:syntax                             ; Readtable designator
   #:string->prefix                     ; FUNCTION
   )
  (:documentation
     "Package holding the readtable designator for mathematical infix notation.

The following two tables enumerate the supported operators along with
their precedence.

Operators:

   NOTE: == is equality, = is assignment (C-style).

    \\                   quoting character:  x\\-y  -->  x-y
    !                   lisp escape    !(foo bar) -->  (foo bar)
    ;                   comment
    x = y               assignment                     (setf x y)
    x += y              increment                      (incf x y)
    x -= y              decrement                      (decf x y)
    x *= y              multiply and store             (setf x (* x y))
    x /= y              divide and store               (setf x (/ x y))
    x|y                 bitwise logical inclusive or   (logior x y)
    x^y                 bitwise logical exclusive or   (logxor x y)
    x&y                 bitwise logical and            (logand x y)
    x<<y                left shift                     (ash x y)
    x>>y                right shift                    (ash x (- y))
    ~x                  ones complement (unary)        (lognot x)
    x and y             conjunction                    (and x y)
    x && y              conjunction                    (and x y)
    x or y              disjunction                    (or x y)
    x || y              disjunction                    (or x y)
    not x               negation                       (not x)
    x^^y                exponentiation                 (expt x y)
    x,y                 sequence                       (progn x y)
    (x,y)               sequence                       (progn x y)
                        also parenthesis (x+y)/z -->   (/ (+ x y) z)
    f(x,y)              functions                      (f x y)
    a[i,j]              array reference                (aref a i j)
    x+y x*y             arithmetic                     (+ x y) (* x y)
    x-y x/y             arithmetic                     (- x y) (/ x y)
    -y                  value negation                 (- y)
    x % y               remainder                      (mod x y)
    x<y x>y             inequalities                   (< x y) (> x y)
    x <= y  x >= y      inequalities                   (<= x y) (>= x y)
    x == y              equality                       (= x y)
    x != y              equality                       (not (= x y))
    if p then q         conditional                    (when p q)
    if p then q else r  conditional                    (if p q r)


Precedence:

   The following precedence conventions are obeyed by the infix operators:
     [ ( !
     ^^
     ~
     * / %
     + -
     << >>
     < == > <= != >=
     &
     ^
     |
     not
     and
     or
     = += -= *= /=
     ,
     if
     then else
     ] )

   Note that logical negation has lower precedence than numeric comparison
   so that \"not a<b\" becomes (not (< a b)), which is different from the
   C precedence conventions. You can change the precedence conventions by
   modifying the value of the variable *operator-ordering*.
"))
