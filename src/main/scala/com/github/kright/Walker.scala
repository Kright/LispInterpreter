package com.github.kright

import java.util
import scala.collection.mutable.ArrayBuffer

class Walker(val tokens: Seq[String]):
  var pos: Int = 0

  def token(offset: Int = 0): Option[String] =
    tokens.lift(pos + offset)

  def shift(s: Int): Unit =
    pos += s

  def consume(f: String => Boolean): Option[String] =
    val r = token().filter(f)
    r.foreach{_ => shift(1)}
    r
    
  def strictlyConsume(s: String): Unit =
    if (consume(_ == s).isEmpty) {
      throw LispSyntaxError(s"expected '$s', got '${token().getOrElse("")}' instead!")
    }


def tokenize(code: String): Seq[String] =
  val tokens = new ArrayBuffer[String]()

  def add(start: Int, end: Int): Unit =
    val len = end - start
    if (len > 0) tokens += code.substring(start, end)

  var start = 0
  for((c, pos) <- code.zipWithIndex) {
    if (c == ' ' || c == '\n') {
      add(start, pos)
      start = pos + 1
    } else if (c == '(' || c == ')' || c == '\'') {
      add(start, pos)
      add(pos, pos + 1)
      start = pos + 1
    }
  }

  add(start, code.length)
  tokens.toSeq
