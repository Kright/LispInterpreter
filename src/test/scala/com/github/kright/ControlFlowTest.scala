package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class ControlFlowTest extends AnyFunSuite with LispTestSuite:
  TEST_F("IfReturnValueSimple") {
    ExpectEq("(if #t 1 2)", "1");
    ExpectEq("(if #f 1 2)", "2");
    ExpectEq("(if #t 1)", "1");
    ExpectEq("(if #f 1)", "()");
  }

  TEST_F("IfReturnValue") {
    ExpectEq("(if #t 0)", "0");
    ExpectEq("(if #f 0)", "()");
    ExpectEq("(if (= 2 2) (+ 1 10))", "11");
    ExpectEq("(if (= 2 3) (+ 1 10) 5)", "5");
  }

  TEST_F("IfEvaluation") {
    ExpectNoError("(define x 1)");

    ExpectNoError("(if #f (set! x 2))");
    ExpectEq("x", "1");

    ExpectNoError("(if #t (set! x 4) (set! x 3))");
    ExpectEq("x", "4");
  }

  TEST_F("IfSyntax") {
    ExpectSyntaxError("(if)");
    ExpectSyntaxError("(if 1 2 3 4)");
  }
