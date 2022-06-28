package com.github.kright

import scala.collection.mutable.ArrayBuffer
import scala.util.Try


class LispVm(val debug: Boolean):
  val parentClosure = new Closure(None)
  parentClosure.variables.addAll(Builtins.all)
  val state = new LispVmState(ComboEval, ArrayBuffer(parentClosure))

  inline def log(inline s: => String): Unit =
    if (debug)
      println(s)

  def run(code: String): Option[String] =
    log(code + "\n")
    val tokens = tokenize(code)
    log(s"tokens = ${tokens.mkString(" ")}")
    val walker = new Walker(tokens)
    val parsed: Try[Option[Tree]] = Try {
      ExpressionParser.parse(walker)
    }
    if (parsed.isFailure) {
      log(s"cannot parse: ${parsed.failed.get.toString}")
      return None
    }
    if (parsed.get.isEmpty) {
      log("parse nothing")
      return None
    }
    val parsedTree = parsed.get.get
    log(s"parsed tree = ${parsedTree}")

    val result = Try {
      state.eval(parsedTree)
    }

    if (result.isFailure) {
      log(s"cannot eval: ${result.failed.get}")
      return None
    }

    Option(result.get.asCode)
