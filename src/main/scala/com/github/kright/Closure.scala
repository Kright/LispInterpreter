package com.github.kright

import scala.collection.mutable

final class Closure(val parent: Option[Closure],
                    val variables: mutable.HashMap[String, Tree] = new mutable.HashMap()):

  def define(name: String, value: Tree): Unit =
    variables(name) = value

  def set(name: String, value: Tree): Unit =
    if (variables.contains(name)) {
      variables(name) = value
    } else {
      parent match
        case Some(p) => p.set(name, value)
        case None => throw new LispNameError(name)
    }

  def apply(name: String): Tree =
    variables.getOrElse(name, parent.getOrElse(throw new LispNameError(s"cannot find identifier ${name}"))(name))

  def define(name: Tree, value: Tree): Unit =
    define(idName(name), value)

  def set(name: Tree, value: Tree): Unit =
    set(idName(name), value)

  def apply(name: Tree): Tree =
    apply(idName(name))

def idName(id: Tree): String =
  id match
    case LispIdentifier(name) => name
    case _ => throw new LispRuntimeError(s"can't get name of ${id}")