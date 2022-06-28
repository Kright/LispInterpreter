package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class ListTest extends AnyFunSuite with LispTestSuite :

  testLisp("Quote") {
    expectEq("(quote (1 2))", "(1 2)");
  }

  testLisp("_ListsAreNotSelfEvaliating") {
    expectRuntimeError("()")
    expectRuntimeError("(1)")
    expectRuntimeError("(1 2 3)")

    expectEq("'()", "()")
    expectEq("'(1)", "(1)")
    expectEq("'(1 2)", "(1 2)")
  }

  testLisp("_ListSyntax") {
    expectEq("'(1 . 2)", "(1 . 2)")
    expectSyntaxError("(1 . 2 3)")

    expectEq("'(1 2 . 3)", "(1 2 . 3)")
    expectEq("'(1 2 . ())", "(1 2)")
    expectEq("'(1 . (2 . ()))", "(1 2)")
  }

  testLisp("ListInvalidSyntax") {
    expectSyntaxError("((1)")
    expectSyntaxError(")(1)")

    expectSyntaxError("(.)")
    expectSyntaxError("(1 .)")
    expectSyntaxError("(. 2)")
  }

  testLisp("PairPredicate") {
    expectEq("(pair? '(1 . 2))", "#t")
    expectEq("(pair? '(1 2))", "#t")
    expectEq("(pair? '())", "#f")
  }

  testLisp("NullPredicate") {
    expectEq("(null? '())", "#t")
    expectEq("(null? '(1 2))", "#f")
    expectEq("(null? '(1 . 2))", "#f")
  }

  testLisp("ListPredicate") {
    expectEq("(list? '())", "#t")
    expectEq("(list? '(1 2))", "#t")
    expectEq("(list? '(1 . 2))", "#f")
    expectEq("(list? '(1 2 3 4 . 5))", "#f")
  }

  testLisp("PairOperations") {
    expectEq("(cons 1 2)", "(1 . 2)")
    expectEq("(car '(1 . 2))", "1")
    expectEq("(cdr '(1 . 2))", "2")
  }

  testLisp("PairMutations") {
    expectNoError("(define x '(1 . 2))")

    expectNoError("(set-car! x 5)")
    expectEq("(car x)", "5")

    expectNoError("(set-cdr! x 6)")
    expectEq("(cdr x)", "6")
  }

  testLisp("ListDebug") {
    expectNoError("(define x '(1 2 3))");

    expectEq("(list-ref x 0)", "1");
    expectEq("(list-ref x 1)", "2");
    expectEq("(list-ref x 2)", "3");
    expectRuntimeError("(list-ref x 3)");
  }

  testLisp("ListOperations") {
    expectEq("(list)", "()")
    expectEq("(list 1)", "(1)")
    expectEq("(list 1 2 3)", "(1 2 3)")

    expectEq("(list-ref '(1 2 3) 1)", "2")
    expectEq("(list-tail '(1 2 3) 1)", "(2 3)")
    expectEq("(list-tail '(1 2 3) 3)", "()")

    expectRuntimeError("(list-ref '(1 2 3) 1 2 3)")
    expectRuntimeError("(list-ref '(1 2 3) 3)")
    expectRuntimeError("(list-ref '(1 2 3) 10)")
    expectRuntimeError("(list-tail '(1 2 3) 10)")
  }
