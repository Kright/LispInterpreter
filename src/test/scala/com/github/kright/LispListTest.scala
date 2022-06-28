package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

class LispListTest extends AnyFunSuite:
  test("take 0") {
    val result = LispList.take(LispList(LispNumber(1), EmptyObject), 0)
    assert(result.get == LispNumber(1))
  }

  test("tail 0") {
    for(lst <- Seq(EmptyObject, LispList(LispNumber(1), EmptyObject))) {
      val result = LispList.tail(lst, 0)
      assert(result.get == lst)
    }
  }

  test("tail 1 for 1-element list") {
    val result = LispList.tail(LispList(LispNumber(1), EmptyObject), 1)
    assert(result.get == EmptyObject)
  }
