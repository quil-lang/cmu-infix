;;; src/cmu-infix.lisp

(in-package #:cmu-infix)

;;; This is an implementation of an infix reader macro. It should run
;;; in any valid Common Lisp. It allows the user to type arithmetic
;;; expressions in the traditional way (e.g., 1+2) when writing Lisp
;;; programs instead of using the normal Lisp syntax (e.g., (+ 1 2)).
;;; It is not intended to be a full replacement for the normal Lisp
;;; syntax. If you want a more complete alternate syntax for Lisp, get
;;; a copy Apple's MLisp or Pratt's CGOL.
;;;
;;; Although similar in concept to the Symbolics infix reader
;;; (#<DIAMOND>), no real effort has been made to ensure compatibility
;;; beyond coverage of at least the same set of basic arithmetic
;;; operators. There are several differences in the syntax beyond just
;;; the choice of #I as the macro character. (Our syntax is a little
;;; bit more C-like than the Symbolics macro in addition to some more
;;; subtle differences.)
;;;
;;; We initially chose $ as a macro character because of its
;;; association with mathematics in LaTeX, but unfortunately that
;;; character is already used in MCL. We switched to #I() because it
;;; was one of the few options remaining.


;;; ********************************
;;; Documentation ******************
;;; ********************************
;;;
;;; Syntax:
;;;
;;;   Begin the reader macro with #I( and end it with ). For example,
;;;      #I( x^^2 + y^^2 )
;;;   is equivalent to the Lisp form
;;;      (+ (expt x 2) (expt y 2))
;;;   but much easier to read according to some folks.
;;;
;;;   If you want to see the expansion, type a quote before the #I form
;;;   at the Lisp prompt:
;;;     > '#I(if x<y<=z then f(x)=x^^2+y^^2 else f(x)=x^^2-y^^2)
;;;     (IF (AND (< X Y) (<= Y Z))
;;;         (SETF (F X) (+ (EXPT X 2) (EXPT Y 2)))
;;;         (SETF (F X) (- (EXPT X 2) (EXPT Y 2))))
;;;
;;;

;;; ********************************
;;; To Do **************************
;;; ********************************
;;;
;;;    Write some more test cases.
;;;    Write some more syntactic optimizations.
;;;    Would really like ~x to be (not x), but need it for (lognot x).
;;;    Support for multiple languages, such as a Prolog parser, a
;;;    strictly C compatible parser, etc.

;;; Create a more declarative format, where there is one big table of
;;; operators with all the info on them, and also NOT have the list of
;;; operators in the comment, where they are likely to become wrong when
;;; changes are made to the code. For example, something like:

;; (define-infix-operators
;;   ([  30                           :matchfix aref :end ])
;;   (*  20 :infix *                                       )
;;   (+  10 :infix +        :prefix +                      )
;;   (&  10 :infix and                                     )
;;   (+= 10 :infix #'+=-operator                           )
;;   ...)


;;; ********************************
;;; Implementation Notes ***********
;;; ********************************
;;;
;;; Initially we tried implementing everything within the Lisp reader,
;;; but found this to not be workable. Parameters had to be passed in
;;; global variables, and some of the processing turned out to be
;;; indelible, so it wasn't possible to use any kind of lookahead.
;;; Center-embedded constructions were also a problem, due to the lack
;;; of an explicit stack.
;;;
;;; So we took another tack, that used below. The #I macro binds the
;;; *readtable* to a special readtable, which is used solely for tokenization
;;; of the input. Then the problem is how to correctly parenthesize the input.
;;; We do that with what is essentially a recursive-descent parser. An
;;; expression is either a prefix operator followed by an expression, or an
;;; expression followed by an infix operator followed by an expression. When
;;; the latter expression is complex, the problem becomes a little tricky.
;;; For example, suppose we have
;;;      exp1 op1 exp2 op2
;;; We need to know whether to parenthesize it as
;;;      (exp1 op1 exp2) op2
;;; or as
;;;      exp1 op1 (exp2 op2 ...)
;;; The second case occurs either when op2 has precedence over op1 (e.g.,
;;; * has precedence over +) or op2 and op1 are the same right-associative
;;; operator (e.g., exponentiation). Thus the algorithm is as follows:
;;; When we see op1, we want to gobble up exp2 op2 exp3 op3 ... opn expn+1
;;; into an expression where op2 through opn all have higher precedence
;;; than op1 (or are the same right-associative operator), and opn+1 doesn't.
;;; This algorithm is implemented by the GATHER-SUPERIORS function.
;;;
;;; Because + and - are implemented in the infix readtable as terminating
;;; macro cahracters, the exponentiation version of Lisp number syntax
;;;    1e-3 == 0.001
;;; doesn't work correctly -- it parses it as (- 1e 3). So we add a little
;;; cleverness to GATHER-SUPERIORS to detect when the tokenizer goofed.
;;; Since this requires the ability to lookahead two tokens, we use a
;;; stack to implement the lookahead in PEEK-TOKEN and READ-TOKEN.
;;;
;;; Finally, the expression returned by GATHER-SUPERIORS sometimes needs to
;;; be cleaned up a bit. For example, parsing a<b<c would normally return
;;; (< (< a b) c), which obviously isn't correct. So POST-PROCESS-EXPRESSION
;;; detects this and similar cases, replacing the expression with (< a b c).
;;; For cases like a<b<=c, it replaces it with (and (< a b) (<= b c)).
;;;

;;; ********************************
;;; Readtable **********************
;;; ********************************

(defreadtable cmu-infix:syntax
  (:merge :standard)
  (:dispatch-macro-char #\# #\I 'infix-reader))

;;; The *INFIX-READTABLE* is *not* the same as the named readtable
;;; CMU-INFIX:SYNTAX. Instead, it's a special readtable used in the
;;; internals of the INFIX-READER function.
;;;
;;;                                -- Robert Smith
(defparameter *infix-readtable* (copy-readtable nil))
(defparameter *normal-readtable* (copy-readtable nil))

(defmacro infix-error (format-string &rest args)
  `(let ((*readtable* *normal-readtable*))
     (error ,format-string ,@args)))

(defun infix-reader (stream subchar arg)
  ;; Read either #I(...) or #I"..."
  (declare (ignore arg subchar))
  (let ((first-char (peek-char nil stream t nil t)))
    (cond ((char= first-char #\space)
           (read-char stream)           ; skip over whitespace
           (infix-reader stream nil nil))
          ((char= first-char #\")
           ;; Read double-quote-delimited infix expressions.
           (string->prefix (read stream t nil t)))
          ((char= first-char #\()
           (read-char stream)           ; get rid of opening left parenthesis
           (let ((*readtable* *infix-readtable*)
                 (*normal-readtable* *readtable*))
             (read-infix stream)))
          (t
           (infix-error "Infix expression starts with ~A" first-char)))))

(defun string->prefix (string)
  "Convert a string to a prefix s-expression using the infix reader.
  If the argument is not a string, just return it as is."
  (if (stringp string)
      (with-input-from-string (stream (concatenate 'string "#I(" string ")"))
        (read stream))
      string))

(defun read-infix (stream)
  (let* ((result (gather-superiors '\) stream)) ; %infix-end-token%
         (next-token (read-token stream)))
    (unless (same-token-p next-token '\)) ; %infix-end-token%
      (infix-error "Infix expression ends with ~A." next-token))
    result))

(defun read-regular (stream)
  (let ((*readtable* *normal-readtable*))
    (read stream t nil t)))


;;; ********************************
;;; Reader Code ********************
;;; ********************************

(defun same-operator-p (x y)
  (same-token-p x y))

(defun same-token-p (x y)
  (and (symbolp x)
       (symbolp y)
       (string-equal (symbol-name x) (symbol-name y))))

;;; Peeking Token Reader

(defvar *peeked-token* nil)
(defun read-token (stream)
  (if *peeked-token*
      (pop *peeked-token*)
      (read stream t nil t)))
(defun peek-token (stream)
  (unless *peeked-token*
    (push (read stream t nil t) *peeked-token*))
  (car *peeked-token*))

;;; Hack to work around + and - being terminating macro characters,
;;; so 1e-3 doesn't normally work correctly.

(defun fancy-number-format-p (left operator stream)
  (when (and (symbolp left)
             (find operator '(+ -) :test #'same-operator-p))
    (let* ((name (symbol-name left))
           (length (length name)))
      (when (and (valid-numberp (subseq name 0 (1- length)))
                 ;; Exponent, Single, Double, Float, or Long
                 (find (subseq name (1- length))
                       '("e" "s" "d" "f" "l")
                       :test #'string-equal))
        (read-token stream)
        (let ((right (peek-token stream)))
          (cond ((integerp right)
                 ;; it is one of the fancy numbers, so return it
                 (read-token stream)
                 (let ((*readtable* *normal-readtable*))
                   (read-from-string (format nil "~A~A~A"
                                             left operator right))))
                (t
                 ;; it isn't one of the fancy numbers, so unread the token
                 (push operator *peeked-token*)
                 ;; and return nil
                 nil)))))))

(defun valid-numberp (string)
  (let ((saw-dot nil))
    (dolist (char (coerce string 'list) t)
      (cond ((char= char #\.)
             (if saw-dot
                 (return nil)
                 (setq saw-dot t)))
            ((not (find char "01234567890" :test #'char=))
             (return nil))))))

;;; Gobbles an expression from the stream.

(defun gather-superiors (previous-operator stream)
  "Gathers an expression whose operators all exceed the precedence of
   the operator to the left."
  (let ((left (get-first-token stream)))
    (loop
      (setq left (post-process-expression left))
      (let ((peeked-token (peek-token stream)))
        (let ((fancy-p (fancy-number-format-p left peeked-token stream)))
          (when fancy-p
            ;; i.e., we've got a number like 1e-3 or 1e+3 or 1f-1
            (setq left fancy-p
                  peeked-token (peek-token stream))))
        (unless (or (operator-lessp previous-operator peeked-token)
                    (and (same-operator-p peeked-token previous-operator)
                         (operator-right-associative-p previous-operator)))
          ;; The loop should continue when the peeked operator is
          ;; either superior in precedence to the previous operator,
          ;; or the same operator and right-associative.
          (return left)))
      (setq left (get-next-token stream left)))))

(defun get-first-token (stream)
  (let ((token (read-token stream)))
    (if (token-operator-p token)
        ;; It's an operator in a prefix context.
        (apply-token-prefix-operator token stream)
        ;; It's a regular token
        token)))

(defun apply-token-prefix-operator (token stream)
  (let ((operator (get-token-prefix-operator token)))
    (if operator
        (funcall operator stream)
        (infix-error "~A is not a prefix operator" token))))

(defun get-next-token (stream left)
  (let ((token (read-token stream)))
    (apply-token-infix-operator token left stream)))

(defun apply-token-infix-operator (token left stream)
  (let ((operator (get-token-infix-operator token)))
    (if operator
        (funcall operator stream left)
        (infix-error "~A is not an infix operator" token))))

;;; Fix to read-delimited-list so that it works with tokens, not
;;; characters.

(defun infix-read-delimited-list (end-token delimiter-token stream)
  (do ((next-token (peek-token stream) (peek-token stream))
       (list nil))
      ((same-token-p next-token end-token)
       ;; We've hit the end. Remove the end-token from the stream.
       (read-token stream)
       ;; and return the list of tokens.
       ;; Note that this does the right thing with [] and ().
       (nreverse list))
    ;; Ignore the delimiters.
    (when (same-token-p next-token delimiter-token)
      (read-token stream))
    ;; Gather the expression until the next delimiter.
    (push (gather-superiors delimiter-token stream) list)))


;;; ********************************
;;; Precedence *********************
;;; ********************************

(defparameter *operator-ordering*
    '(( \[ \( \! )                      ; \[ is array reference
      ( ^^ )                            ; exponentiation
      ( ~ )                             ; lognot
      ( * /  % )                        ; % is mod
      ( + - )
      ( << >> )
      ( < == > <= != >= )
      ( & )                             ; logand
      ( ^ )                             ; logxor
      ( \| )                            ; logior
      ( not )
      ( and )
      ( or )
      ;; Where should setf and friends go in the precedence?
      ( = |:=| += -= *= /= )
      ( \, )                            ; progn (statement delimiter)
      ( if )
      ( then else )
      ( \] \) )
      ( %infix-end-token% ))            ; end of infix expression
  "Ordered list of operators of equal precedence.")

(defun operator-lessp (op1 op2)
  (dolist (ops *operator-ordering* nil)
    (cond ((find op1 ops :test #'same-token-p)
           (return nil))
          ((find op2 ops :test #'same-token-p)
           (return t)))))

(defparameter *right-associative-operators* '(^^ =))
(defun operator-right-associative-p (operator)
  (find operator *right-associative-operators*))


;;; ********************************
;;; Define Operators ***************
;;; ********************************

(defvar *token-operators* nil)
(defvar *token-prefix-operator-table* (make-hash-table))
(defvar *token-infix-operator-table* (make-hash-table))
(defun token-operator-p (token)
  (find token *token-operators*))
(defun get-token-prefix-operator (token)
  (gethash token *token-prefix-operator-table*))
(defun get-token-infix-operator (token)
  (gethash token *token-infix-operator-table*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-token-operator (operator-name &key
                                                 (prefix nil prefix-p)
                                                 (infix nil infix-p))
    `(progn
       (pushnew ',operator-name *token-operators*)
       ,(when prefix-p
          `(setf (gethash ',operator-name *token-prefix-operator-table*)
                 #'(lambda (stream)
                     ,@(cond ((and (consp prefix)
                                   (eq (car prefix) 'infix-error))
                              ;; To avoid ugly compiler warnings.
                              `((declare (ignore stream))
                                ,prefix))
                             (t
                              (list prefix))))))
       ,(when infix-p
          `(setf (gethash ',operator-name *token-infix-operator-table*)
                 #'(lambda (stream left)
                     ,@(cond ((and (consp infix)
                                   (eq (car infix) 'infix-error))
                              ;; To avoid ugly compiler warnings.
                              `((declare (ignore stream left))
                                ,infix))
                             (t
                              (list infix)))))))))

;;; Readtable definitions for characters, so that the right token is returned.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro define-character-tokenization (char function)
    `(set-macro-character ,char ,function nil *infix-readtable*)))


;;; ********************************
;;; Operator Definitions ***********
;;; ********************************

(define-token-operator and
    :infix  `(and ,left ,(gather-superiors 'and stream)))
(define-token-operator or
    :infix  `(or ,left ,(gather-superiors 'or stream)))
(define-token-operator not
    :prefix `(not ,(gather-superiors 'not stream)))

(define-token-operator if
    :prefix (let* ((test (gather-superiors 'if stream))
                   (then (cond ((same-token-p (peek-token stream) 'then)
                                (read-token stream)
                                (gather-superiors 'then stream))
                               (t
                                (infix-error "Missing THEN clause."))))
                   (else (when (same-token-p (peek-token stream) 'else)
                           (read-token stream)
                           (gather-superiors 'else stream))))
              (cond ((and test then else)
                     `(if ,test ,then ,else))
                    ((and test then)
                     ;; no else clause
                     `(when ,test ,then))
                    ((and test else)
                     ;; no then clause
                     `(unless ,test ,else))
                    (t
                     ;; no then and else clauses --> always NIL
                     nil))))

(define-token-operator then
    :prefix (infix-error "THEN clause without an IF."))
(define-token-operator else
    :prefix (infix-error "ELSE clause without an IF."))

(define-character-tokenization #\+
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '+=)
              (t
               '+))))
(define-token-operator +
    :infix `(+ ,left ,(gather-superiors '+ stream))
    :prefix (gather-superiors '+ stream))
(define-token-operator +=
    :infix `(incf ,left ,(gather-superiors '+= stream)))

(define-character-tokenization #\-
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '-=)
              (t
               '-))))
(define-token-operator -
    :infix `(- ,left ,(gather-superiors '- stream))
    :prefix `(- ,(gather-superiors '- stream)))
(define-token-operator -=
    :infix `(decf ,left ,(gather-superiors '-= stream)))

(define-character-tokenization #\*
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '*=)
              (t
               '*))))
(define-token-operator *
    :infix `(* ,left ,(gather-superiors '* stream)))
(define-token-operator *=
    :infix `(,(if (symbolp left)
                  'setq
                  'setf)
              ,left
              (* ,left ,(gather-superiors '*= stream))))

(define-character-tokenization #\/
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '/=)
              (t
               '/))))
(define-token-operator /
    :infix `(/ ,left ,(gather-superiors '/ stream))
    :prefix `(/ ,(gather-superiors '/ stream)))
(define-token-operator /=
    :infix `(,(if (symbolp left)
                  'setq
                  'setf)
              ,left
              (/ ,left ,(gather-superiors '/= stream))))

(define-character-tokenization #\^
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\^)
               (read-char stream t nil t)
               '^^)
              (t
               '^))))
(define-token-operator ^^
    :infix `(expt ,left ,(gather-superiors '^^ stream)))
(define-token-operator ^
    :infix `(logxor ,left ,(gather-superiors '^ stream)))

(define-character-tokenization #\|
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\|)
               (read-char stream t nil t)
               'or)
              (t
               '\|))))
(define-token-operator \|
    :infix `(logior ,left ,(gather-superiors '\| stream)))

(define-character-tokenization #\&
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\&)
               (read-char stream t nil t)
               'and)
              (t
               '\&))))
(define-token-operator \&
    :infix `(logand ,left ,(gather-superiors '\& stream)))

(define-character-tokenization #\%
    #'(lambda (stream char)
        (declare (ignore stream char))
        '\%))
(define-token-operator \%
    :infix `(mod ,left ,(gather-superiors '\% stream)))

(define-character-tokenization #\~
    #'(lambda (stream char)
        (declare (ignore stream char))
        '\~))
(define-token-operator \~
    :prefix `(lognot ,(gather-superiors '\~ stream)))

(define-character-tokenization #\,
    #'(lambda (stream char)
        (declare (ignore stream char))
        '\,))
(define-token-operator \,
    :infix `(progn ,left ,(gather-superiors '\, stream)))

(define-character-tokenization #\=
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '==)
              (t
               '=))))
(define-token-operator ==
    :infix `(= ,left ,(gather-superiors '== stream)))
(define-token-operator =
    :infix `(,(if (symbolp left)
                  'setq
                  'setf)
              ,left
              ,(gather-superiors '= stream)))

(define-character-tokenization #\:
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '|:=|)
              (t
               '|:|))))
(define-token-operator |:=|
    :infix `(,(if (symbolp left)
                  'setq
                  'setf)
              ,left
              ,(gather-superiors '|:=| stream)))

(define-character-tokenization #\<
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '<=)
              ((char= (peek-char nil stream t nil t) #\<)
               (read-char stream t nil t)
               '<<)
              (t
               '<))))
(define-token-operator <
    :infix `(< ,left ,(gather-superiors '< stream)))
(define-token-operator <=
    :infix `(<= ,left ,(gather-superiors '<= stream)))
(define-token-operator <<
    :infix `(ash ,left ,(gather-superiors '<< stream)))

(define-character-tokenization #\>
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '>=)
              ((char= (peek-char nil stream t nil t) #\>)
               (read-char stream t nil t)
               '>>)
              (t
               '>))))
(define-token-operator >
    :infix `(> ,left ,(gather-superiors '> stream)))
(define-token-operator >=
    :infix `(>= ,left ,(gather-superiors '>= stream)))
(define-token-operator >>
    :infix `(ash ,left (- ,(gather-superiors '>> stream))))

(define-character-tokenization #\!
    #'(lambda (stream char)
        (declare (ignore char))
        (cond ((char= (peek-char nil stream t nil t) #\=)
               (read-char stream t nil t)
               '!=)
              (t
               '!))))
(define-token-operator !=
    :infix `(not (= ,left ,(gather-superiors '!= stream))))
(define-token-operator !
    :prefix (read-regular stream))

(define-character-tokenization #\[
    #'(lambda (stream char)
        (declare (ignore stream char))
        '\[))
(define-token-operator \[
    :infix (let ((indices (infix-read-delimited-list '\] '\, stream)))
             (if (null indices)
                 (infix-error "No indices found in array reference.")
                 `(aref ,left ,@indices))))

(define-character-tokenization #\(
    #'(lambda (stream char)
        (declare (ignore stream char))
        '\())
(define-token-operator \(
    :infix `(,left ,@(infix-read-delimited-list '\) '\, stream))
    :prefix (let ((list (infix-read-delimited-list '\) '\, stream)))
              (if (null (rest list))
                  ;; only one element in list. works correctly if list is NIL
                  (first list)
                  ;; several elements in list
                  `(progn ,@list))))

(define-character-tokenization #\]
    #'(lambda (stream char)
        (declare (ignore stream char))
        '\]))
(define-token-operator \]
    :infix (infix-error "Extra close brace \"]\" in infix expression"))

(define-character-tokenization #\)
    #'(lambda (stream char)
        (declare (ignore stream char))
        '\)))
(define-token-operator \)
    :infix (infix-error "Extra close paren \")\" in infix expression"))

#|
;;; Commented out because no longer using $ as the macro character.
(define-character-tokenization #\$
    #'(lambda (stream char)
        (declare (ignore stream char))
        '%infix-end-token%))
(define-token-operator %infix-end-token%
    :infix (infix-error "Prematurely terminated infix expression")
    :prefix (infix-error "Prematurely terminated infix expression"))
|#

(define-character-tokenization #\;
    #'(lambda (stream char)
        (declare (ignore char))
        (do ((char (peek-char nil stream t nil t)
                   (peek-char nil stream t nil t)))
            ((or (char= char #\newline) (char= char #\return)
                 ;; was #\$
                 ;; (char= char #\))
                 )
             ;; Gobble characters until the end of the line or the
             ;; end of the input.
             (cond ((or (char= char #\newline) (char= char #\return))
                    (read-char stream)
                    (read stream t nil t))
                   (t
                    ;; i.e., return %infix-end-token%
                    (read stream t nil t))))
          (read-char stream))))


;;; ********************************
;;; Syntactic Modifications ********
;;; ********************************

;;; Post processes the expression to remove some unsightliness caused
;;; by the way infix processes the input. Note that it is also required
;;; for correctness in the a<b<=c case.

(defun post-process-expression (expression)
  (if (and (consp expression)
           (= (length expression) 3))
      (destructuring-bind (operator left right) expression
        (cond ((and (consp left)
                    (same-operator-p (first left) operator)
                    (find operator '(+ * / - and or < > <= >= progn)
                          :test #'same-operator-p))
               ;; Flatten the expression if possible
               (cond ((and (eq operator '-)
                           (= (length left) 2))
                      ;; -a-b --> (+ (- a) (- b)).
                      `(+ ,left (- ,right)))
                     ((and (eq operator '/)
                           (= (length left) 2))
                      ;; ditto with /
                      `(/ (* ,(second left) ,right)))
                     (t
                      ;; merges a+b+c as (+ a b c).
                      (append left (list right)))))
              ((and (consp left)
                    (eq operator '-)
                    (eq (first left) '+))
               ;; merges a+b-c as (+ a b (- c)).
               (append left (list `(- ,right))))
              ((and (consp left)
                    (find operator '(< > <= >=))
                    (find (first left) '(< > <= >=)))
               ;; a<b<c --> a<b and b<c
               `(and ,left
                     (,operator ,(first (last left))
                                ,right)))
              (t
               expression)))
      expression))
