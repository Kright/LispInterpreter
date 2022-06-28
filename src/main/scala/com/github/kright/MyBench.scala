package com.github.kright

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag


inline def myBench[T: ClassTag](count: Int)(inline code: (Int) => T): BenchResult[T] = {
  val benchResult = new BenchResult(new ArrayBuffer[Long](count), new ArrayBuffer[T](count))

  for(i <- 0 until count) {
    val start = System.nanoTime()
    val result = code(i)
    val end = System.nanoTime()
    benchResult.timesNs += end - start
    benchResult.results += result
  }

  benchResult
}

class BenchResult[T](val timesNs: ArrayBuffer[Long], val results: ArrayBuffer[T]) {
  def firstTime = timesNs.head
  def bestTime = timesNs.min
  def averageTime = timesNs.sum.toDouble / timesNs.size

  override def toString: String = s"best = ${bestTime}ns, av = ${averageTime}ns, first = ${firstTime}ns"
}
