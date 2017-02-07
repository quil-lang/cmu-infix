# CMU-INFIX

A library for writing infix mathematical notation in Common Lisp.

## Origin

This library was originally written by Mark Kantrowitz in 1993 with updates the following few years. The code in this repository was derived from the original [`infix.cl`](https://www.cs.cmu.edu/Groups/AI/lang/lisp/code/syntax/infix/infix.cl) library provided by the CMU AI Repository. For posterity, a copy of this original file—otherwise unused by this library—can be found in [`attic/infix.cl`](attic/infix.cl).

With minimal changes to the core functionality, the library was modernized by Robert Smith to be in-line with contemporary Common Lisp usage.

## Example Use

This package uses [`named-readtables`](https://common-lisp.net/project/named-readtables) to manage the readtables. If you've loaded `CMU-INFIX` successfully, then you'll have this package loaded as well.

To use `CMU-INFIX`, simply use the readtable named `cmu-infix:syntax`:

```lisp
(named-readtables:in-readtable cmu-infix:syntax)
```

Once you have this, you can use the `#I` syntax for infix syntax. Here are some examples.

**Example**: Pythagorean Theorem

```lisp
(defun hypot (a b)
  "Compute the length of the hypotenuse of a right triangle
   with sides A and B."
  #I( sqrt(a^^2 + b^^2) ))
```

**Example**: Power-of-Two Check

```lisp
(defun power-of-two-p (n)
  "Check if N is a power of 2."
  #I( n != 0 and (n & (n - 1)) == 0 ))
```



**Example**: Euclidean Algorithm

```lisp
(defun euclid (a b)
  "Compute the GCD of A and B using Euclid's algorithm."
  (let (temp)
    (loop :until #I( b == 0 ) :do
      #I( temp := b,
          b := a % b,
          a := temp
        ))
    a))
```

**Example**: Matrix Multiplication

```lisp
(defun matmul (A B)
  "Compute C = A * B for matrices A and B."
  (let* ((m (array-dimension A 0))
         (n (array-dimension A 1))
         (q (array-dimension B 1))
         (C (make-array (list m q) :initial-element 0)))
    (loop :for i :below m :do
      (loop :for k :below q :do
        (loop :for j :below n :do
          #I( C[i, k] += A[i, j] * B[j, k] ))))
    C))

;; Example:
(let ((A (make-array '(2 2) :initial-contents '((0 1) (1 0))))
      (B (make-array '(2 1) :initial-contents '((2) (3)))))
  #I( matmul(A, B) ))
```

A full description of the supported operators is in the package documentation for `CMU-INFIX`:

```lisp
(format t "~A" (documentation (find-package :cmu-infix) t))
```

## Modernization Updates

The library has been updated in the following ways:

* The package of this library has been renamed `CMU-INFIX` so as to not conflict with existing Quicklisp libraries.

* A system of the same name has been made so it is loadable by ASDF.

* The tests have been lifted and put into a separate system called `CMU-INFIX-TESTS`. You can run them by doing

  ```lisp
  (asdf:test-system :cmu-infix)
  ```

* The library was modified to use `NAMED-READTABLES` to not eagerly pollute your readtable.

* Some out-of-date comments have been deleted.

## Contributing

After receiving permission from Mark Kantrowitz, [Rigetti Computing](http://rigetti.com/) has taken stewardship of the library. Questions and issues should be filed on GitHub [here](https://github.com/rigetticomputing/cmu-infix), and pull requests are welcome. The licensing terms are described in [`LICENSE.txt`](LICENSE.txt).