package com.github.kright.benchmarking

import scala.collection.mutable.ArrayBuffer

class BenchResult[T](val timesNs: ArrayBuffer[Long], val results: ArrayBuffer[T]):
  def firstTime = timesNs.head
  def bestTime = timesNs.min
  def averageTime: Long = timesNs.sum / timesNs.size

  private def formatTime(t: Long): String = t.toString.reverse.grouped(3).mkString("_").reverse + "ns"
  override def toString: String =
    s"best = ${formatTime(bestTime)}, av = ${formatTime(averageTime)}, first = ${formatTime(firstTime)}"
