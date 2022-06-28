package com.github.kright

@main
def main(): Unit = {
  perfFibonacci()
  return

  println("Hello world!")

  val vm = LispVm(debug = true)

//  println(vm.run("((lambda (x) (* x x)) 2)"))

//  println(vm.run("""(define (f x)
//                   |  (
//                   |    do
//                   |    (define (sqr b) (* b b))
//                   |    (sqr x)
//                   |  )
//                   |)""".stripMargin))
//  println(vm.run("(f 2)"))

//  println(vm.run(MyCodeExamples.reverseLst))

//  vm.run(MyCodeExamples.naiveFib)
//  println(vm.run("(naive-fib 8)"))

//  println(vm.run("(reverse-lst '(1 2 3))"))
}

def perfFibonacci(): Unit = {
  val vm = new LispVm(debug = false)
  vm.run(MyCodeExamples.naiveFib)

//  vm.run("(naive-fib 18)")
//  println(ComboEval.stats)
//  return

  val results = myBench(1000) { t =>
    (t, vm.run("(naive-fib 18)"))
  }

  println(s"lisp fib: ${results}")

  val results2 = myBench(1000) { t =>
    (t, scalaNaiveFib(18))
  }

  println(s"scala fib: ${results2}")
}

def scalaNaiveFib(n: Int): Int =
  if (n < 2)
    1
  else
    scalaNaiveFib(n - 1) + scalaNaiveFib(n - 2)

object MyCodeExamples:
  val lstSize = "(define (lst-size lst) (if (null? lst) 0 (+ 1 (lst-size (cdr lst)))) )"
  val reverseLst =
    """(define (reverse-lst lst)
      |  (do
      |    (
      |      define (rev l t)
      |      (
      |        if (null? l)
      |        t
      |        (rev (cdr l) (cons (car l) t))
      |      )
      |    )
      |    (rev lst '())
      |  )
      |)""".stripMargin

  val naiveFib =
    """
      |(define (naive-fib n)
      |  (
      |    if (< n 2)
      |    1
      |    (+
      |      (naive-fib (- n 1))
      |      (naive-fib (- n 2))
      |    )
      |  )
      |)
      |""".stripMargin
