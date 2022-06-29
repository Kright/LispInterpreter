package com.github.kright.benchmarking

import com.github.kright.MyCodeExamples

object Benches:
  val naiveFib = new Bench(MyCodeExamples.naiveFib, "(naive-fib 18)")

  val all: Map[String, Bench] = Map(
    "naiveFib" -> naiveFib
  )