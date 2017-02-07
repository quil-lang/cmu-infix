;;;; tests/tests.lisp

(in-package #:cmu-infix-tests)

(defun run-tests (&key (headless nil))
  "Run all of the unit tests for CMU-INFIX."
  (cond
    ((null headless)
     (run-package-tests :package ':cmu-infix-tests
                        :verbose nil
                        :describe-failures t
                        :interactive t))
    (t
     (let ((successp (run-package-tests :package ':cmu-infix-tests
                                        :verbose t
                                        :describe-failures t
                                        :interactive nil)))
       (uiop:quit (if successp 0 1))))))

(defparameter *test-cases*
    ;; Note that in strings, we have to slashify \ as \\.
    '(("1 * +2"         (* 1 2))
      ("1 * -2"         (* 1 (- 2)))
      ("1 * /2"         (* 1 (/ 2)))
      ("/2"             (/ 2))
      ("not true"       (not true))
      ("foo\\-bar"     foo-bar)
      ("a + b-c"       (+ a b (- c)))
      ("a + b\\-c"      (+ a b-c))
      ("f\\oo"          |FoO|)
      ("!foo-bar * 2"  (* foo-bar 2))
      ("!(foo bar baz)" (foo bar baz))
      ("!foo-bar "     foo-bar)
      ;; The following now longer gives an eof error, since the close
      ;; parenthesis terminates the token.
      ("!foo-bar"      foo-bar)         ; eof error -- ! eats the close $
      ("a+-b"          (+ a (- b)))
      ("a+b"           (+ a b))
      ("a+b*c"         (+ a (* b c)))
      ("a+b+c"         (+ a b c))
      ("a+b-c"         (+ a b (- c)))
      ("a+b-c+d"       (+ a b (- c) d))
      ("a+b-c-d"       (+ a b (- c) (- d)))
      ("a-b"           (- a b))
      ("a*b"           (* a b))
      ("a*b*c"         (* a b c))
      ("a*b+c"         (+ (* a b) c))
      ("a/b"           (/ a b))
      ("a^^b"          (expt a b))
      ("foo/-bar"      (/ foo (- bar)))
      ("1+2*3^^4"       (+ 1 (* 2 (expt 3 4))))
      ("1+2*3^^4+5"     (+ 1 (* 2 (expt 3 4)) 5))
      ("2*3^^4+1"       (+ (* 2 (expt 3 4)) 1))
      ("2+3^^4*5"       (+ 2 (* (expt 3 4) 5)))
      ("2^^3^^4"        (expt 2 (expt 3 4)))
      ("x^^2 + y^^2"    (+ (expt x 2) (expt y 2)))
      ("(1+2)/3"       (/ (+ 1 2) 3))
      ("(a=b)"         (setq a b))
      ("(a=b,b=c)"     (progn (setq a b) (setq b c)))
      ("1*(2+3)"       (* 1 (+ 2 3)))
      ("1+2/3"         (+ 1 (/ 2 3)))
      ("a,b"           (progn a b))
      ("a,b,c"         (progn a b c))
      ("foo(a,b,(c,d))" (foo a b (progn c d)))
      ("foo(a,b,c)"    (foo a b c))
      ("(a+b,c)"       (progn (+ a b) c))
      ("1"             1)
      ("-1"            (- 1))
      ("+1"            1)
      ("1."            1)
      ("1.1"           1.1)
      ("1e3"           1000.0)
      ("1e-3"          0.001)
      ("1f-3"          1f-3)
      ("1e-3e"         (- 1e 3e))
      ("!1e-3 "        0.001)
      ("a and b and c" (and a b c))
      ("a and b or c"  (or (and a b) c))
      ("a and b"       (and a b))
      ("a or b and c"  (or a (and b c)))
      ("a or b"        (or a b))
      ("a<b and b<c"   (and (< a b) (< b c)))
      ("if (if a then b else c) then e" (when (if a b c) e))
      ("if 1 then 2 else 3+4"  (if 1 2 (+ 3 4)))
      ("(if 1 then 2 else 3)+4"  (+ (if 1 2 3) 4))
      ("if a < b then b else a"   (if (< a b) b a))
      ("if a and b then c and d else e and f"
       (if (and a b) (and c d) (and e f)))
      ("if a or b then c or d else e or f" (if (or a b) (or c d) (or e f)))
      ("if a then (if b then c else d) else e"  (if a (if b c d) e))
      ("if a then (if b then c) else d"  (if a (when b c) d))
      ("if a then b else c"       (if a b c))
      ("if a then b"              (when a b))
      ("if a then if b then c else d else e" (if a (if b c d) e))
      ("if a then if b then c else d"  (when a (if b c d)))
      ("if if a then b else c then e" (when (if a b c) e))
      ("if not a and not b then c" (when (and (not a) (not b)) c))
      ("if not a then not b else not c and d"
       (if (not a) (not b) (and (not c) d)))
      ("not a and not b" (and (not a) (not b)))
      ("not a or not b" (or (not a) (not b)))
      ("not a<b and not b<c"   (and (not (< a b)) (not (< b c))))
      ("not a<b"       (not (< a b)))
      ("a[i,k]*b[j,k]"          (* (aref a i k) (aref b j k)))
      ("foo(bar)=foo[bar,baz]"  (setf (foo bar) (aref foo bar baz)))
      ("foo(bar,baz)"           (foo bar baz))
      ("foo[bar,baz]"           (aref foo bar baz))
      ("foo[bar,baz]=barf"      (setf (aref foo bar baz) barf))
      ("max = if a < b then b else a"   (setq max (if (< a b) b a)))
      ("a < b < c"     (< A B C))
      ("a < b <= c"    (and (< a b) (<= b c)))
      ("a <= b <= c"   (<= A B C))
      ("a <= b <= c"   (<= A B C))
      ("a!=b and b<c"  (and (not (= a b)) (< b c)))
      ("a!=b"          (not (= a b)))
      ("a<b"           (< a b))
      ("a==b"          (= a b))
      ("a*b(c)+d"      (+ (* a (b c)) d))
      ("a+b(c)*d"      (+ a (* (b c) d)))
      ("a+b(c)+d"      (+ a (b c) d))
      ("d+a*b(c)"      (+ d (* a (b c))))
      ("+a+b"          (+ a b))
      ("-a+b"          (+ (- a) b))
      ("-a-b"          (+ (- a) (- b)))
      ("-a-b-c"        (+ (- a) (- b) (- c)))
      ("a*b/c"         (/ (* a b) c))
      ("a+b-c"         (+ a b (- c)))
      ("a-b-c"         (- a b c))
      ("a/b*c"         (* (/ a b) c))
      ("a/b/c"         (/ a b c))
      ("/a/b"          (/ (* a b)))
      ("a^^b^^c"         (expt a (expt b c)))
      ("a(d)^^b^^c"      (expt (a d) (expt b c)))
      ("a<b+c<d"       (< a (+ b c) d))
      ("1*~2+3"        (+ (* 1 (lognot 2)) 3)) 
      ("1+~2*3"        (+ 1 (* (lognot 2) 3))) 
      ("1+~2+3"        (+ 1 (lognot 2) 3)) 
      ("f(a)*=g(b)"    (setf (f a) (* (f a) (g b))))
      ("f(a)+=g(b)"    (incf (f a) (g b)))
      ("f(a)-=g(b)"    (decf (f a) (g b)))
      ("f(a)/=g(b)"    (setf (f a) (/ (f a) (g b))))
      ("a&b"           (logand a b))
      ("a^b"           (logxor a b))
      ("a|b"           (logior a b))
      ("a<<b"          (ash a b))
      ("a>>b"          (ash a (- b)))
      ("~a"            (lognot a))
      ("a&&b"          (and a b))
      ("a||b"          (or a b))
      ("a%b"           (mod a b))

      ;; Comment character -- must have carriage return after semicolon.
      ("x^^2   ; the x coordinate
        + y^^2 ; the y coordinate" :error)
      ("x^^2   ; the x coordinate
        + y^^2 ; the y coordinate
        "              (+ (expt x 2) (expt y 2)))

      ;; Errors
      ("foo(bar,baz"   :error)          ; premature termination
      ;; The following no longer gives an error
      ("foo(bar,baz))" (foo bar baz))   ; extra close parenthesis
      ("foo[bar,baz]]" :error)          ; extra close bracket
      ("[foo,bar]"     :error)          ; AREF is not a prefix operator
      ("and a"         :error)          ; AND is not a prefix operator
      ("< a"           :error)          ; < is not a prefix operator
      ("=bar"          :error)          ; SETF is not a prefix operator
      ("*bar"          :error)          ; * is not a prefix operator
      ("a not b"       :error)          ; NOT is not an infix operator
      ("a if b then c" :error)          ; IF is not an infix operator
      (""              :error)          ; premature termination (empty clause)
      (")a"            :error)          ; left parent is not a prefix operator
      ("]a"            :error)          ; left bracket is not a prefix operator
      ))

(deftest test-infix (&optional (tests *test-cases*))
  (let ((count 0))
    (dolist (test tests)
      (destructuring-bind (string result) test
        (unless (test-infix-case string result)
          (incf count))))
    (format t "~&~:(~R~) test~p failed." count count)
    (values)))

(defun test-infix-case (string result)
  (let ((specimen (concatenate 'string "#I(" string ")"))
        (*readtable* (named-readtables:find-readtable 'cmu-infix:syntax)))
    (if (eql result ':error)
        (signals error (read-from-string specimen))
        (is (equal result (read-from-string specimen))))))  
