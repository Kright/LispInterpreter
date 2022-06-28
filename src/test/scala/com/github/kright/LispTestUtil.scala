package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

private def lispSession(f: LispVm ?=> Unit): Unit = {
  given vm: LispVm = new LispVm(debug = false)
  f
}

def expectRuntimeError(code: String)(using vm: LispVm): Unit =
  assert(vm.run(code).isEmpty)

def expectSyntaxError(code: String)(using vm: LispVm): Unit =
  assert(vm.run(code).isEmpty)

def expectNoError(code: String)(using vm: LispVm): Unit =
  assert(vm.run(code).isDefined)

def expectUndefinedVariable(code: String)(using vm: LispVm): Unit =
  assert(vm.run(code).isEmpty)

def expectEq(code: String, expected: String)(using vm: LispVm): Unit =
  val result = vm.run(code)
  assert(result.isDefined, "don't get any result!")
  assert(result.get == expected, s"expected '$expected', got '${result.get}' for code ${code}")

trait LispTestSuite:
  self: AnyFunSuite =>
  inline def testLisp(name: String)(testFun: LispVm ?=> Any /* Assertion */): Unit =
    test(name) {
      lispSession{
        testFun
      }
    }
