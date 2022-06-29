package com.github.kright

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

  val naiveFib = """
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
