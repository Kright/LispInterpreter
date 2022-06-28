package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class ControlFlowTest extends AnyFunSuite with LispTestSuite:
  testLisp("IfReturnValueSimple") {
    expectEq("(if #t 1 2)", "1");
    expectEq("(if #f 1 2)", "2");
    expectEq("(if #t 1)", "1");
    expectEq("(if #f 1)", "()");
  }

  testLisp("IfReturnValue") {
    expectEq("(if #t 0)", "0");
    expectEq("(if #f 0)", "()");
    expectEq("(if (= 2 2) (+ 1 10))", "11");
    expectEq("(if (= 2 3) (+ 1 10) 5)", "5");
  }

  testLisp("IfEvaluation") {
    expectNoError("(define x 1)");

    expectNoError("(if #f (set! x 2))");
    expectEq("x", "1");

    expectNoError("(if #t (set! x 4) (set! x 3))");
    expectEq("x", "4");
  }

  testLisp("IfSyntax") {
    expectSyntaxError("(if)");
    expectSyntaxError("(if 1 2 3 4)");
  }
