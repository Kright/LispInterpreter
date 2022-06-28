package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class BooleanTest extends AnyFunSuite with LispTestSuite:
  testLisp("BooleansAreSelfEvaluating") {
    expectEq("#t", "#t")
    expectEq("#f", "#f")
  }

  testLisp("BooleanDefineVariable") {
    expectNoError("(define x #t)")
    expectEq("x", "#t")
  }

  testLisp("BooleanPredicate") {
    expectEq("(boolean? #t)", "#t")
    expectEq("(boolean? #f)", "#t")
    expectEq("(boolean? 1)", "#f")
    expectEq("(boolean? '())", "#f")
  }

  testLisp("NotFunction") {
    expectEq("(not #f)", "#t")
    expectEq("(not #t)", "#f")
//    ExpectEq("(not 1)", "#f")
//    ExpectEq("(not 0)", "#f")
//    ExpectEq("(not '())", "#f")
  }

  testLisp("NotFunctionInvalidCall") {
    expectRuntimeError("(not)")
    expectRuntimeError("(not #t #t)")
  }

  testLisp("AndSyntax") {
    // (and <test>)
    // The <test> expressions are evaluated from left to right, and the value of the first expression
    // that evaluates to a false value is returned. Any remaining expressions are not evaluated.
    // If all the expressions evaluate to true values, the value of the last expression is returned.
    // If there are no expressions then #t is returned.

//    ExpectEq("(and)", "#t")
    expectEq("(and (= 2 2) (> 2 1))", "#t")
    expectEq("(and (= 2 2) (< 2 1))", "#f")
//    ExpectEq("(and 1 2 'c '(f g))", "(f g)")
  }

  testLisp("AndOptimizesArgumentEvaluation") {
    expectNoError("(define x 1)")
    expectNoError("(and #f (set! x 2))")
    expectEq("x", "1")
  }

  testLisp("OrSyntax") {
    // (or <test>)
    // The <test> expressions are evaluated from left to right, and the value of the first expression
    // that evaluates to a true value is returned. Any remaining expressions are
    // not evaluated. If all expressions evaluate to false values, the value of the last expression is returned.
    // If there are no expressions then #f is returned.

//    ExpectEq("(or)", "#f")
    expectEq("(or (not (= 2 2)) (> 2 1))", "#t")
    expectEq("(or #f (< 2 1))", "#f")
//    ExpectEq("(or #f 1)", "1")
  }

  testLisp("OrOptimizesArgumentEvaluation") {
    expectNoError("(define x 1)")
    expectNoError("(or #t (set! x 2))")
    expectEq("x", "1")
  }
