package com.github.kright.benchmarking

import com.github.kright.LispVm

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


class Bench(val prelude: String,
            val code: String):
  def run(runsCount: Int = 1000) =
    val vm = new LispVm(debug = false)
    vm.run(prelude)

    Bench(runsCount) { i =>
      (i, vm.run(code))
    }

object Bench:
  inline def apply[T: ClassTag](count: Int)(inline code: (Int) => T): BenchResult[T] =
    val benchResult = new BenchResult(new ArrayBuffer[Long](count), new ArrayBuffer[T](count))

    for(i <- 0 until count) {
      val start = System.nanoTime()
      val result = code(i)
      val end = System.nanoTime()
      benchResult.timesNs += end - start
      benchResult.results += result
    }

    benchResult
