package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class WalkerTest extends AnyFunSuite:
  test("tokenize") {
    assert(tokenize("(a)") == Seq("(", "a", ")"))
    assert(tokenize("(+ 1 2)") == Seq("(", "+", "1", "2", ")"))
    assert(tokenize("'()") == Seq("'", "(", ")"))
    assert(tokenize("(pair? '())") == Seq("(", "pair?", "'", "(", ")", ")"))
  }

  def testTokenizer(code: String, tokens: String*): Unit =
    assert(tokenize(code) == tokens)

  test("ParseWord") {
    testTokenizer("abc", "abc")
  }

  test("ParseWords") {
    testTokenizer("a b c", "a", "b", "c")
  }

  test("ParseBrackets") {
    testTokenizer("()", "(", ")")
    testTokenizer("(1 2)", "(", "1", "2", ")")
    testTokenizer("'(1 2)", "'", "(", "1", "2", ")")
    testTokenizer("((1)", "(", "(", "1", ")")
  }

  test("ParseList") {
    testTokenizer("(list-ref '(1 2 3) 1)", "(", "list-ref", "'", "(", "1", "2", "3", ")", "1", ")")
  }

