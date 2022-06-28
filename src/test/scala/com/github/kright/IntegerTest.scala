package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class IntegerTest extends AnyFunSuite with LispTestSuite {
  testLisp("IntegersInQuotes") {
    expectEq("\'1", "1")
  }

  testLisp("IntegersAreSelfEvaluating") {
    expectEq("4", "4")
    expectEq("-14", "-14")
    expectEq("+14", "14")
  }

  testLisp("IntegerPredicate") {
    expectEq("(number? -1)", "#t");
    expectEq("(number? 1)", "#t");
    expectEq("(number? #t)", "#f");
  }

  testLisp("IntegerComparison") {
    // =, <, >, <=, >=
    // These procedures return #t if their arguments are (respectively): equal,
    // monotonically increasing, monotonically decreasing, monotonically nondecreasing,
    // or monotonically nonincreasing.

//    ExpectEq("(=)", "#t");
//    ExpectEq("(>)", "#t");
//    ExpectEq("(<)", "#t");
//    ExpectEq("(>=)", "#t");
//    ExpectEq("(<=)", "#t");

    expectEq("(= 1 2)", "#f");
    expectEq("(= 1 1)", "#t");
//    ExpectEq("(= 1 1 1)", "#t");
//    ExpectEq("(= 1 1 2)", "#f");

    expectEq("(> 2 1)", "#t");
    expectEq("(> 1 1)", "#f");
//    ExpectEq("(> 3 2 1)", "#t");
//    ExpectEq("(> 3 2 3)", "#f");

    expectEq("(< 1 2)", "#t");
    expectEq("(< 1 1)", "#f");
//    ExpectEq("(< 1 2 3)", "#t");
//    ExpectEq("(< 1 2 1)", "#f");

    expectEq("(>= 2 1)", "#t");
    expectEq("(>= 1 2)", "#f");
//    ExpectEq("(>= 3 3 2)", "#t");
//    ExpectEq("(>= 3 3 4)", "#f");

    expectEq("(<= 2 1)", "#f");
    expectEq("(<= 1 2)", "#t");
//    ExpectEq("(<= 3 3 4)", "#t");
//    ExpectEq("(<= 3 3 2)", "#f");
  }

  testLisp("IntegerComparisonEdgeCases") {
    expectRuntimeError("(= 1 #t)");
    expectRuntimeError("(< 1 #t)");
    expectRuntimeError("(> 1 #t)");
    expectRuntimeError("(<= 1 #t)");
    expectRuntimeError("(>= 1 #t)");
  }

  testLisp("IntegerArithmetics") {
    expectEq("(+ 1 2)", "3");
//    ExpectEq("(+ 1)", "1");
    expectEq("(+ 1 (+ 3 4 5))", "13");
    expectEq("(- 1 2)", "-1");
    expectEq("(- 2 1)", "1");
    expectEq("(* 5 6)", "30");
    expectEq("(/ 4 2)", "2");
//    ExpectEq("(/ 4 2 2)", "1");
  }

  testLisp("IntegerArithmeticsEdgeCases") {
    expectRuntimeError("(+ 1 #t)");
    expectRuntimeError("(- 1 #t)");
    expectRuntimeError("(* 1 #t)");
    expectRuntimeError("(/ 1 #t)");

//    ExpectEq("(+)", "0");
//    ExpectEq("(*)", "1");
    expectRuntimeError("(/)");
    expectRuntimeError("(-)");
  }

  testLisp("IntegerMaxMin") {
    expectEq("(max 0)", "0");
    expectEq("(min 0)", "0");

    expectEq("(max 1 2)", "2");
    expectEq("(min 1 2)", "1");

    expectEq("(max 1 2 3 4 5)", "5");
    expectEq("(min 1 2 3 4 5)", "1");
  }

  testLisp("IntegerMaxMinEdgeCases") {
    expectRuntimeError("(max)");
    expectRuntimeError("(min)");

    expectRuntimeError("(max #t)");
    expectRuntimeError("(min #t)");
  }

  testLisp("IntegerAbs") {
    expectEq("(abs 10)", "10")
    expectEq("(abs -10)", "10")
  }

  testLisp("IntegerAbsEdgeCases") {
    expectRuntimeError("(abs)");
    expectRuntimeError("(abs #t)");
    expectRuntimeError("(abs 1 2)");
  }
}
