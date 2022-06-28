package com.github.kright

import com.github.kright

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap.from

trait Evaluator:
  def eval(state: LispVmState, form: Tree): Option[Tree]

case class FunctionLambda private (variables: Closure,
                                   func: LispLambdaDefinition) extends LispFunction:
  def this(funcDef: Tree, currentClosure: Closure) =
    this(new Closure(parent = Option(currentClosure)), funcDef.asInstanceOf[LispLambdaDefinition])

  override def call(state: LispVmState, args: Seq[Tree]): Tree =
    if (func.args.size != args.size) throw new LispRuntimeError("wrong args count")
    val readyArgs = args.map(state.eval)

    val localClosure = new Closure(parent = Option(variables))
    state.addClosure(localClosure)
    for((argName, value) <- func.args.zip(readyArgs)) {
      localClosure.define(argName, value)
    }
    val result = func.body.map(state.eval).last
    state.pop()
    result

  override def asCode: String = "lambda"


object ComboEval extends Evaluator:
  override def eval(state: LispVmState, form: Tree): Option[Tree] =
    form match
      case n: LispNumber => Option(n)
      case b: LispBoolean => Option(b)
      case LispIdentifier(name) => Option(state.currentClosure(name))
      case LispVector(commands) => {
        if (commands.isEmpty) throw new LispRuntimeError("cant eval empty list")
        state.eval(commands.head) match
          case f: LispFunction => Option(f.call(state, commands.tail))
          case _ =>  throw new LispRuntimeError("function expected")
      }
      case LispVariableDefinition(name, expr) => {
        val value = state.eval(expr)
        state.currentClosure.define(name, value)
        Option(EmptyObject)
      }
      case lld: LispLambdaDefinition => Option(new FunctionLambda(lld, state.currentClosure))
      case LispList(_, _) => throw new LispRuntimeError("")
      case _ => None


class TypeStats:
  val map = new mutable.HashMap[String, Int]().withDefault(_ => 0)

  def add(t: Tree): Unit =
    val name = t match
      case _ : LispNumber => "number"
      case _: LispBoolean => "boolean"
      case _: LispVariableDefinition => "define"
      case _: LispIdentifier => "id"
      case _: LispList => "list"
      case _: LispVector => "vector"
      case _: LispLambdaDefinition => "lambdadef"

    map(name) = map(name) + 1

  override def toString: String =
    map.mkString("stats:\n", "\n", "")
