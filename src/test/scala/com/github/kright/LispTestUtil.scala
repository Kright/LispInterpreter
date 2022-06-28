package com.github.kright

import org.scalatest.funsuite.AnyFunSuite

private def lispSession(f: LispVm ?=> Unit): Unit = {
  given vm: LispVm = new LispVm(debug = false)
  f
}

def ExpectRuntimeError(code: String)(using vm: LispVm): Unit =
  assert(vm.run(code).isEmpty)

def ExpectSyntaxError(code: String)(using vm: LispVm): Unit =
  assert(vm.run(code).isEmpty)

def ExpectNoError(code: String)(using vm: LispVm): Unit =
  assert(vm.run(code).isDefined)

def ExpectUndefinedVariable(code: String)(using vm: LispVm): Unit =
  assert(vm.run(code).isEmpty)

def ExpectEq(code: String, expected: String)(using vm: LispVm): Unit =
  val result = vm.run(code)
  assert(result.isDefined, "don't get any result!")
  assert(result.get == expected, s"expected '$expected', got '${result.get}' for code ${code}")

trait LispTestSuite:
  self: AnyFunSuite =>

  inline def TEST_F(name: String)(testFun: LispVm ?=> Any /* Assertion */): Unit =
    test(name) {
      lispSession{ 
        testFun 
      }
    }
