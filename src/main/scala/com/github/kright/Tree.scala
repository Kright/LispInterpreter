package com.github.kright

import scala.annotation.tailrec

sealed trait Tree:
  def asCode: String

object EmptyObject extends Tree:
  override def asCode: String = "()"
  override def toString: String = "()"

case class LispBoolean(b: Boolean) extends Tree:
  override def asCode: String = if (b) "#t" else "#f"

case class LispNumber(n: Long) extends Tree:
  override def asCode: String = n.toString
  override def toString: String = n.toString

case class LispIdentifier(name: String) extends Tree:
  override def asCode: String = name
  override def toString: String = s"id($name)"

case class LispList(var left: Tree, var right: Tree) extends Tree:
  override def asCode: String = s"(${elements})"

  private def elements: String =
    right match
      case lst: LispList => s"${left.asCode} ${lst.elements}"
      case EmptyObject => left.asCode
      case _ => s"${left.asCode} . ${right.asCode}"

object LispList:
  @tailrec
  def take(lst: Tree, number: Long): Option[Tree] =
    if (number < 0) return None
    lst match
      case LispList(left, right) => if (number == 0) Option(left) else take(right, number - 1)
      case _ => None

  @tailrec
  def tail(lst: Tree, number: Long): Option[Tree] =
    if (number < 0) return None
    lst match
      case a: LispList => if (number == 0) Option(lst) else tail(a.right, number - 1)
      case EmptyObject if number == 0 => Option(lst)
      case _ => None

case class LispVector(elements: Seq[Tree]) extends Tree:
  override def asCode: String = elements.map(_.asCode).mkString("LispVector(", ", ", ")")

case class LispVariableDefinition(variable: Tree, expression: Tree) extends Tree:
  override def asCode: String = ???

abstract class LispFunction extends Tree:
  def call(state: LispVmState, args: Seq[Tree]): Tree

case class LispBuiltinFunction(name: String, f: (LispVmState, Seq[Tree]) => Tree) extends LispFunction:
  override def call(state: LispVmState, args: Seq[Tree]): Tree =
    f(state, args)

  override def asCode: String = name

case class LispLambdaDefinition(args: Seq[Tree], body: Seq[Tree]) extends Tree:
  override def asCode: String = ???
