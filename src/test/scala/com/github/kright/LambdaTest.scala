package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class LambdaTest extends AnyFunSuite with LispTestSuite:
  TEST_F("_SimpleLambda") {
    ExpectEq("((lambda (x) (+ 1 x)) 5)", "6")
  }

  TEST_F("_LambdaBodyHasImplicitBegin") {
    ExpectNoError("(define test (lambda (x) (set! x (* x 2)) (+ 1 x)))");
    ExpectEq("(test 20)", "41");
  }

  TEST_F("_SlowSum") {
    ExpectNoError("(define slow-add (lambda (x y) (if (= x 0) y (slow-add (- x 1) (+ y 1)))))");
    ExpectEq("(slow-add 3 3)", "6");
    ExpectEq("(slow-add 100 100)", "200");
  }

  TEST_F("_ClosureTest") {
    ExpectNoError("(define y 2)");
    ExpectNoError("(define range (lambda () y))");
    ExpectNoError("(lambda () y)");
    ExpectEq("(range)", "2");
  }

  TEST_F("_ParsingTest") {
    ExpectNoError("(lambda () y)");
  }

  TEST_F("_SimpleLambdaClosure") {
    ExpectNoError("(define x 1)");

    ExpectNoError("(define range (lambda (x)(lambda ()(set! x (+ x 1)) x))))");

    ExpectNoError("(define my-range (range 10))");
    ExpectEq("(my-range)", "11");
    ExpectEq("(my-range)", "12");
    ExpectEq("(my-range)", "13");

    ExpectEq("x", "1");
  }

  TEST_F("LambdaClosure") {
    ExpectNoError("(define x 1)");

    ExpectNoError(
    """
      (define range
        (lambda (x)
          (lambda ()
            (set! x (+ x 1))
             x)))
      """);

    ExpectNoError("(define my-range (range 10))");
    ExpectEq("(my-range)", "11");
    ExpectEq("(my-range)", "12");
    ExpectEq("(my-range)", "13");

    ExpectEq("x", "1");
  }

  TEST_F("LambdaSyntax") {
    ExpectSyntaxError("(lambda)");
    ExpectSyntaxError("(lambda x)");
    ExpectSyntaxError("(lambda (x))");
  }

  TEST_F("DefineLambdaSugar") {
    ExpectNoError("(define (inc x) (+ x 1))");
    ExpectEq("(inc -1)", "0");

    ExpectNoError("(define (add x y) (+ x y 1))");
    ExpectEq("(add -10 10)", "1");

    ExpectNoError("(define (zero) 0)");
    ExpectEq("(zero)", "0");
  }
