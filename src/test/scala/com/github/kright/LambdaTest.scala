package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class LambdaTest extends AnyFunSuite with LispTestSuite:
  testLisp("_SimpleLambda") {
    expectEq("((lambda (x) (+ 1 x)) 5)", "6")
  }

  testLisp("_LambdaBodyHasImplicitBegin") {
    expectNoError("(define test (lambda (x) (set! x (* x 2)) (+ 1 x)))");
    expectEq("(test 20)", "41");
  }

  testLisp("_SlowSum") {
    expectNoError("(define slow-add (lambda (x y) (if (= x 0) y (slow-add (- x 1) (+ y 1)))))");
    expectEq("(slow-add 3 3)", "6");
    expectEq("(slow-add 100 100)", "200");
  }

  testLisp("_ClosureTest") {
    expectNoError("(define y 2)");
    expectNoError("(define range (lambda () y))");
    expectNoError("(lambda () y)");
    expectEq("(range)", "2");
  }

  testLisp("_ParsingTest") {
    expectNoError("(lambda () y)");
  }

  testLisp("_SimpleLambdaClosure") {
    expectNoError("(define x 1)");

    expectNoError("(define range (lambda (x)(lambda ()(set! x (+ x 1)) x))))");

    expectNoError("(define my-range (range 10))");
    expectEq("(my-range)", "11");
    expectEq("(my-range)", "12");
    expectEq("(my-range)", "13");

    expectEq("x", "1");
  }

  testLisp("LambdaClosure") {
    expectNoError("(define x 1)");

    expectNoError(
    """
      (define range
        (lambda (x)
          (lambda ()
            (set! x (+ x 1))
             x)))
      """);

    expectNoError("(define my-range (range 10))");
    expectEq("(my-range)", "11");
    expectEq("(my-range)", "12");
    expectEq("(my-range)", "13");

    expectEq("x", "1");
  }

  testLisp("LambdaSyntax") {
    expectSyntaxError("(lambda)");
    expectSyntaxError("(lambda x)");
    expectSyntaxError("(lambda (x))");
  }

  testLisp("DefineLambdaSugar") {
    expectNoError("(define (inc x) (+ x 1))");
    expectEq("(inc -1)", "0");

    expectNoError("(define (add x y) (+ x y 1))");
    expectEq("(add -10 10)", "1");

    expectNoError("(define (zero) 0)");
    expectEq("(zero)", "0");
  }
