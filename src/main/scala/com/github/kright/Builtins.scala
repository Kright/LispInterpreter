package com.github.kright

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

object Builtins:
  private val functions: ArrayBuffer[LispBuiltinFunction] = new ArrayBuffer()

  functions += LispBuiltinFunction("quote", { (state, lazyArgs) => lazyArgs.head })

  functions += LispBuiltinFunction("if", { (state, lazyArgs) =>
    if (lazyArgs.size < 2) throw new LispRuntimeError("useless if")
    state.eval(lazyArgs.head) match
      case LispBoolean(b) =>
        val pos = if (b) 1 else 2
        if (pos < lazyArgs.size) state.eval(lazyArgs(pos))
        else EmptyObject
      case _ => throw new LispRuntimeError("boolean expected!")
  })

  functions += LispBuiltinFunction("set!", { (state, lazyArgs) =>
    if (lazyArgs.size != 2) throw new LispRuntimeError("set require 2 args")
    val result = state.eval(lazyArgs(1))
    state.currentClosure.set(lazyArgs(0), result)
    EmptyObject
  })

  functions+= LispBuiltinFunction("and", { (state, lazyArgs) =>
    val allTrue = lazyArgs.view.map(state.eval).forall(a => castToBoolean(a).b)
    LispBoolean(allTrue)
  })

  functions += LispBuiltinFunction("or", { (state, lazyArgs) =>
    val hasTrue = lazyArgs.view.map(state.eval).exists(a => castToBoolean(a).b)
    LispBoolean(hasTrue)
  })

  functions += unaryStrictPure("not", t => LispBoolean(!castToBoolean(t).b))

  functions += unaryStrictPure("pair?", {
    case _: LispList => LispBoolean(true)
    case _ => LispBoolean(false)
  })

  functions += unaryStrictPure("null?", {
    case EmptyObject => LispBoolean(true)
    case _ => LispBoolean(false)
  })

  functions += unaryStrictPure("list?", arg => { LispBoolean(isList(arg))})

  functions += unaryStrictPure("car", {
    case LispList(head, _) => head
    case _ => throw new LispRuntimeError("can't get head of list")
  })
  functions += unaryStrictPure("cdr", {
    case LispList(_, tail) => tail
    case _ => throw new LispRuntimeError("can't get tail of list")
  })
  functions += binaryStrict("cons", (_, left, right) => LispList(left, right))

  functions += binaryStrict("set-car!", (state, first, second) => {
    first match
      case p: LispList => p.left = second; p
      case _ => throw new LispRuntimeError("first argument should be list")
  })
  functions += binaryStrict("set-cdr!", (state, first, second) => {
    first match
      case p: LispList => p.right = second; p
      case _ => throw new LispRuntimeError("first argument should be list")
  })

  functions += strictFunction("list", {(state, args) =>
    args.foldRight(EmptyObject: Tree)((elem, tree) => LispList(elem, tree))
  })

  functions += binaryStrict("list-ref", { (state, lst, num) =>
    lst match
      case lst: LispList =>
        num match
          case LispNumber(n) =>
            LispList.take(lst, n) match
              case Some(tree) => tree
              case None => throw new LispRuntimeError("list-ref can't get result")
          case _ => throw new LispRuntimeError("list-ref second argument should be a number")
      case _ => throw new LispRuntimeError("list-ref first argument should be a list")
  })

  functions += binaryStrict("list-tail", { (state, lst, num) =>
    num match
      case LispNumber(n) =>
        LispList.tail(lst, n) match
          case Some(tree) => tree
          case None => throw new LispRuntimeError("list-tail can't get result")
      case _ => throw new LispRuntimeError("list-ref second argument should be a number")
  })

  functions += unaryStrictPure("boolean?", {
    case LispBoolean(_) => LispBoolean(true)
    case _ => LispBoolean(false)
  })

  functions += unaryStrictPure("number?", {
    case LispNumber(_) => LispBoolean(true)
    case _ => LispBoolean(false)
  })

  functions += unaryStrictPure("symbol?", {
    case LispIdentifier(_) => LispBoolean(true)
    case _ => LispBoolean(false)
  })

  functions += strictFunction("+", (state, args) => {
    if (args.size < 2) throw new LispRuntimeError("+ expected at least two args")
    LispNumber(args.map(a => castToNumber(a).n).sum)
  })

  functions += strictFunction("*", (state, args) => {
    if (args.size < 2) throw new LispRuntimeError("* expected at least two args")
    LispNumber(args.map(a => castToNumber(a).n).product)
  })

  functions += binaryIntPure("-", (a, b) => LispNumber(a - b))
  functions += binaryIntPure("/", (a, b) => LispNumber(a / b))

  functions += strictFunction("max", (state, args) => {
    if (args.isEmpty) throw new LispRuntimeError("should have at least one argument")
    LispNumber(args.map(a => castToNumber(a).n).max)
  })

  functions += strictFunction("min", (state, args) => {
    if (args.isEmpty) throw new LispRuntimeError("should have at least one argument")
    LispNumber(args.map(a => castToNumber(a).n).min)
  })

  functions += unaryStrictPure("abs", {
    case LispNumber(n) => LispNumber(math.abs(n))
    case _ => throw new LispRuntimeError("expected number!")
  })

  functions += strictFunction("do", (state, args) => {args.last})

  functions += binaryIntPure("<", (a, b) => LispBoolean(a < b))
  functions += binaryIntPure("<=", (a, b) => LispBoolean(a <= b))
  functions += binaryIntPure(">", (a, b) => LispBoolean(a > b))
  functions += binaryIntPure(">=", (a, b) => LispBoolean(a >= b))
  functions += binaryIntPure("=", (a, b) => LispBoolean(a == b))

  val all: Map[String, LispBuiltinFunction] = functions.map(f => f.name -> f).toMap

// todo make inline and compare performance
private inline def strictFunction(name: String, inline f: (LispVmState, Seq[Tree]) => Tree): LispBuiltinFunction =
  LispBuiltinFunction(name, (state, lazyArgs) => {
    val args = lazyArgs.map(state.eval)
    f(state, args)
  })

private def unaryStrictPure(name: String, f: Tree => Tree): LispBuiltinFunction =
  strictFunction(name, (state, args) => {
    if (args.size != 1) throw new LispRuntimeError(s"function ${name} expects only one argument")
    f(args(0))
  })

private def binaryStrict(name: String, f: (LispVmState, Tree, Tree) => Tree): LispBuiltinFunction =
  strictFunction(name, (state, args) => {
    if (args.size != 2) throw new LispRuntimeError(s"function ${name} expects 2 arguments")
    f(state, args(0), args(1))
  })

private inline def binaryIntPure(name: String, inline f: (Long, Long) => Tree): LispBuiltinFunction =
  LispBuiltinFunction(name, (state, lazyArgs) => {
    if (lazyArgs.size != 2) throw new LispRuntimeError("expected 2 integers")

    val nums = lazyArgs.map{ t =>
      state.eval(t) match
        case LispNumber(n) => n
        case _ => throw new LispRuntimeError("expected 2 integers")
    }

    f(nums(0), nums(1))
  })

@tailrec
private def isList(t: Tree): Boolean =
  t match
    case EmptyObject => true
    case LispList(_, tail) => isList(tail)
    case _ => false

private def castToNumber(tree: Tree): LispNumber =
  tree match
    case n: LispNumber => n
    case a => throw new LispRuntimeError(s"should be int, get ${a}")

private def castToBoolean(tree: Tree): LispBoolean =
  tree match
    case n: LispBoolean => n
    case a => throw new LispRuntimeError(s"should be boolean, get ${a}")