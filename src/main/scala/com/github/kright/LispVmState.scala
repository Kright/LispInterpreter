package com.github.kright

import scala.collection.mutable.ArrayBuffer


class LispVmState(val evaluator: Evaluator,
                  val closures: ArrayBuffer[Closure]):

  def currentClosure: Closure =
    closures.last

  def addClosure(closure: Closure): Unit =
    closures += closure

  def pop(): Unit =
    closures.remove(closures.size - 1)

  def eval(form: Tree): Tree =
    evaluator.eval(this, form).getOrElse(form)
