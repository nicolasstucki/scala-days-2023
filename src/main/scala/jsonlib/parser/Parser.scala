package jsonlib.parser

import jsonlib.util.*
import jsonlib.Pattern


private[jsonlib] object Parser:
  def apply(source: Seq[String]): Parser =
    new Parser(new Tokens(new InterpolatedChars(source)))

private[jsonlib] class Parser private(tokens: Tokens):

  private type ![T] = Result.Continuation[Pattern, ParseError] ?=> T

  def parse(): Result[Pattern, ParseError] =
    Result.withContinuation {
      val ast = parseValue()
      accept(Token.End)
      ast
    }

  private def parseValue(): ![Pattern] =
    tokens.next() match
      case Token.Null => Pattern.Null
      case Token.True => Pattern.Bool(true)
      case Token.False => Pattern.Bool(false)
      case Token.Str(value) => Pattern.Str(value)
      case Token.Num(value) => Pattern.Num(value.toDouble)
      case Token.OpenBrace =>
        val nameValues =
          if tokens.peek() == Token.CloseBrace then Vector.empty
          else commaSeparate(parseNameValue)
        accept(Token.CloseBrace)
        nameValues.map(_._1).groupBy(x => x).filter(_._2.length > 1).foreach { x =>
          error(s"Duplicate name: ${x._1}", tokens.location)
        }
        Pattern.Obj(nameValues*)
      case Token.OpenBracket =>
        val values =
          if tokens.peek() == Token.CloseBracket then Vector.empty
          else commaSeparate(parseValue)
        accept(Token.CloseBracket)
        Pattern.Arr(values*)
      case Token.InterpolatedValue =>
        Pattern.InterpolatedValue
      case Token.Error(msg, location) =>
        error(msg, location)
      case token =>
        error(s"unexpected start of value: $token", tokens.location)

  private def commaSeparate[T](parseItem: () => ![T]): ![Vector[T]] =
    def parseNext(values: Vector[T]): Vector[T] =
      tokens.peek() match
        case Token.Comma =>
          accept(Token.Comma)
          parseNext(values :+ parseItem())
        case _ => values
    parseNext(Vector(parseItem()))

  private def parseNameValue(): ![(String, Pattern)] =
    tokens.next() match
      case Token.Str(value) =>
        accept(Token.Colon)
        value -> parseValue()
      case Token.Error(msg, location) =>
        error(msg, location)
      case _ =>
        error("expected string literal", tokens.location)

  private def accept(token: Token): ![Unit] =
    val nextToken = tokens.next()
    if token != nextToken then error(s"expected token $token but got $nextToken", tokens.location)

  private def error(msg: String, location: Location): ![Nothing] =
    Result.continuation.error(ParseError(msg, location))
