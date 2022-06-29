package com.github.kright

import com.github.kright.benchmarking.Bench
import com.github.kright.benchmarking.Benches

@main
def main(): Unit = {
//  perfFibonacci()
//  return

  val vm = LispVm(debug = true)

  println(vm.run("((lambda (x) (* x x)) 2)"))

  println(vm.run(MyCodeExamples.reverseLst))
  println(vm.run("(reverse-lst '(1 2 3))"))

  vm.run(MyCodeExamples.naiveFib)
  println(vm.run("(naive-fib 8)"))
}

def perfFibonacci(): Unit = {
  val results = Benches.naiveFib.run(1000)
  println(s"lisp fib: ${results}")

  val results2 = Bench(1000) { t =>
    (t, scalaNaiveFib(18))
  }

  println(s"scala fib: ${results2}")
}

def scalaNaiveFib(n: Int): Int =
  if (n < 2)
    1
  else
    scalaNaiveFib(n - 1) + scalaNaiveFib(n - 2)