## Simple LISP interpreter

Interpreter for LISP-like language written on Scala 3. Main goal is simplicity of interpreter and interpreted language.

Features:

* integers
* booleans
* lists, pairs
* lambda functions
* quotes
* GC: reuse GC of JVM

example of code:
```
(define (lst-size lst) (if (null? lst) 0 (+ 1 (lst-size (cdr lst)))))
(lst-size '(1, 2, 3))
```