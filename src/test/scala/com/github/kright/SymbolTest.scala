package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class SymbolTest extends AnyFunSuite with LispTestSuite:
  testLisp("_SymbolsAreNotSelfEvaluating") {
    expectUndefinedVariable("x");

    expectEq("'x", "x");
    expectEq("(quote x)", "x");
  }

  testLisp("_SymbolPredicate") {
    expectEq("(symbol? 'x)", "#t");
    expectEq("(symbol? 1)", "#f");
  }

  testLisp("_SymbolsAreUsedAsVariableNames") {
    expectNoError("(define x (+ 1 2))");
    expectEq("x", "3");

    expectNoError("(define x (+ 2 2))");
    expectEq("x", "4");
  }

  testLisp("_DefineInvalidSyntax") {
    expectSyntaxError("(define)");
    expectSyntaxError("(define 1)");
    expectSyntaxError("(define x 1 2)");
  }

  testLisp("_SetOverrideVariables") {
    expectUndefinedVariable("(set! x 2)");
    expectUndefinedVariable("x");

    expectNoError("(define x 1)");
    expectEq("x", "1");

    expectNoError("(set! x (+ 2 4))");
    expectEq("x", "6");
  }

  testLisp("_SetInvalidSyntax") {
    expectSyntaxError("(set!)");
    expectSyntaxError("(set! 1)");
    expectSyntaxError("(set! x 1 2)");
  }
