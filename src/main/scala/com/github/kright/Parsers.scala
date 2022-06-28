package com.github.kright

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import com.github.kright.Walker

trait AbstractParser:
  def parse(walker: Walker): Option[Tree]

trait AbstractSafeParser extends AbstractParser:
  protected def wrappedParse(walker: Walker): Option[Tree]

  override final def parse(walker: Walker): Option[Tree] =
    val backup = walker.pos
    val result = wrappedParse(walker)
    if (result.isEmpty) {
      walker.pos = backup
    }
    result


class CompoundParser(parsers: Seq[AbstractParser]) extends AbstractParser:
  override def parse(walker: Walker): Option[Tree] =
    for(parser <- parsers) {
      val result = parser.parse(walker)
      if (result.isDefined) return result
    }
    None

object BooleanParser extends AbstractParser:
  override def parse(walker: Walker): Option[Tree] =
    walker.token() match
      case Some("#t") => walker.shift(1); Option(LispBoolean(true))
      case Some("#f") => walker.shift(1); Option(LispBoolean(false))
      case _ => None

object NumberParser extends AbstractParser:
  private def isNumber(str: String): Boolean =
    str.head match
      case '+' | '-' => str.length >= 2 && str.tail.forall(_.isDigit)
      case c if c.isDigit => str.forall(_.isDigit)
      case _ => false

  override def parse(walker: Walker): Option[Tree] =
    walker.consume(isNumber).map(t => LispNumber(t.toLong))

object IdentifierParser extends AbstractParser:
  def isInitial(c: Char): Boolean =
    c.isLetter || "!$%&*/:<=>?~_^".contains(c)

  def isSubsequent(c: Char): Boolean =
    isInitial(c) || c.isDigit || "+-.".contains(c)

  private def isIdentifier(str: String): Boolean =
    str == "+" || str == "-" ||
    (isInitial(str.head) && str.tail.forall(isSubsequent))

  override def parse(walker: Walker): Option[Tree] =
    walker.consume(isIdentifier).map{ t =>
      LispIdentifier(t)
    }

object NodeParser extends CompoundParser(Seq(BooleanParser, NumberParser, IdentifierParser, ListParser))
object ConstantParser extends CompoundParser(Seq(BooleanParser, NumberParser))
object DatumParser extends CompoundParser(Seq(BooleanParser, NumberParser, IdentifierParser, ListParser))

object ListParser extends AbstractSafeParser:
  private def parseEndOfList(walker: Walker): Option[Tree] =
    for {
      node <- NodeParser.parse(walker)
    } yield {
      walker.strictlyConsume(")")
      node
    }

  override def wrappedParse(walker: Walker): Option[Tree] =
    if (walker.consume(_ == "(").isEmpty) return None

    val elements = ArrayBuffer[Tree]()

    var finished = false
    var hasDot = false

    while(!finished) {
      if (walker.consume(_ == ".").isDefined) {
        hasDot = true
        parseEndOfList(walker) match
          case Some(end) => elements += end; finished = true
          case None => return None
      } else if (walker.consume(_ == ")").isDefined) {
        finished = true
      } else {
        NodeParser.parse(walker) match
          case Some(value) => elements += value
          case None => return None
      }
    }

    if (elements.isEmpty) return Option(EmptyObject)
    if (hasDot && elements.size < 2) return None
    if (!hasDot) elements += EmptyObject

    Option(elements.reduceRight{ (l, r) => LispList(l, r)})

object QuoteParser extends AbstractSafeParser:
  override def wrappedParse(walker: Walker): Option[Tree] =
    if (walker.token().contains("(") && walker.token(1).contains("quote")) {
      walker.shift(2)
      return for {
        datum <- DatumParser.parse(walker)
      } yield {
        walker.strictlyConsume(")")
        LispVector(Seq(LispIdentifier("quote"), datum))
      }
    }

    if (walker.consume(_ == "'").isDefined) {
      val datum = DatumParser.parse(walker).getOrElse(throw new LispSyntaxError("cannot parse after '"))
      return Option(LispVector(Seq(LispIdentifier("quote"), datum)))
    }

    None

object ExpressionParser extends CompoundParser(Seq(
  FunctionDefinitionParser,
  VariableDefinitionParser,

  ConstantParser,
  IdentifierParser,
  QuoteParser,
  LambdaParser,
  SetParser,
  AndOrParser,
  IfParser,
  ApplicationParser
))

object ApplicationParser extends AbstractSafeParser:
  override protected def wrappedParse(walker: Walker): Option[Tree] =
    for {
      _ <- walker.consume(_ == "(")
    } yield {
      val commands = parseMany(ExpressionParser, walker)
      walker.strictlyConsume(")")
      commands
    }


object IfParser extends AbstractSafeParser:
  override def wrappedParse(walker: Walker): Option[Tree] =
    for {
      _ <- walker.consume(_ == "(")
      _ <- walker.consume(_ == "if")
    } yield {
      val vec = parseAsVector(ExpressionParser, walker)
      walker.strictlyConsume(")")
      LispVector(Seq(LispIdentifier("if")) ++ vec)
    }

object SetParser extends AbstractSafeParser:
  override protected def wrappedParse(walker: Walker): Option[Tree] =
    for {
      _ <- walker.consume(_ == "(")
      _ <- walker.consume(_ == "set!")
    } yield {
      val variable = IdentifierParser.parse(walker).getOrElse(throw LispSyntaxError("expected varialbe"))
      val expression = ExpressionParser.parse(walker).getOrElse(throw LispSyntaxError("expected expression"))
      walker.strictlyConsume(")")
      LispVector(Seq(LispIdentifier("set!"), variable, expression))
    }

object AndOrParser extends AbstractSafeParser:
  override protected def wrappedParse(walker: Walker): Option[Tree] =
    for {
      _ <- walker.consume(_ == "(")
      t <- walker.consume(t => t == "or" || t == "and")
    } yield {
      val vec = parseAsVector(ExpressionParser, walker)
      walker.strictlyConsume(")")
      LispVector(Seq(LispIdentifier(t)) ++ vec)
    }


object LambdaParser extends AbstractSafeParser:
  override protected def wrappedParse(walker: Walker): Option[Tree] =
    for {
      _ <- walker.consume(_ == "(")
      _ <- walker.consume(_ == "lambda")
    } yield {
      walker.strictlyConsume("(")
      val ids = parseAsVector(IdentifierParser, walker)
      walker.strictlyConsume(")")
      val body = parseAsVector(ExpressionParser, walker)
      if (body.isEmpty) throw new LispSyntaxError("lambda should contain body")
      walker.strictlyConsume(")")
      LispLambdaDefinition(ids, body)
    }


object VariableDefinitionParser extends AbstractSafeParser:
  override protected def wrappedParse(walker: Walker): Option[Tree] =
    for {
      _ <- walker.consume(_ == "(")
      _ <- walker.consume(_ == "define")
    } yield {
      val variable = IdentifierParser.parse(walker).getOrElse(throw LispSyntaxError("no variable"))
      val expression = ExpressionParser.parse(walker).getOrElse(throw LispSyntaxError("no expression"))
      walker.strictlyConsume(")")
      LispVariableDefinition(variable, expression)
    }


object FunctionDefinitionParser extends AbstractSafeParser:
  override protected def wrappedParse(walker: Walker): Option[Tree] =
    for {
      _ <- walker.consume(_ == "(")
      _ <- walker.consume(_ == "define")
      _ <- walker.consume(_ == "(")
    } yield {
      val name = IdentifierParser.parse(walker).getOrElse(throw LispSyntaxError("expected id"))
      val args = parseAsVector(IdentifierParser, walker)
      walker.strictlyConsume(")")
      val body = parseAsVector(ExpressionParser, walker)
      if (body.isEmpty) throw LispSyntaxError("empty body")
      walker.strictlyConsume(")")
      LispVariableDefinition(name, LispLambdaDefinition(args, body))
    }

private def parseAsVector(parser: AbstractParser, walker: Walker): Seq[Tree] =
  val result = new ArrayBuffer[Tree]()
  var finished = false
  while(!finished) {
    parser.parse(walker) match
      case Some(tree) => result += tree
      case None => finished = true
  }
  result.toSeq

private def parseMany(parser: AbstractParser, walker: Walker) = LispVector(parseAsVector(parser, walker))
