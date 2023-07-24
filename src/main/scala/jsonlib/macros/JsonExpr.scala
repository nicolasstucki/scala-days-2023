package jsonlib.macros

import scala.quoted.*

import jsonlib.*
import jsonlib.parser.*
import jsonlib.util.*
import scala.quoted.runtime.Patterns.patternType

private[jsonlib] object JsonExpr:
  def jsonExpr(jsonStringContext: Expr[JsonStringContext], argsExpr: Expr[Seq[Json]])(using Quotes): Expr[Json] =
    val json: Pattern = parsed(jsonStringContext)
    val argExprs: Seq[Expr[Json]] = argsExpr match
      case Varargs(argExprs) => argExprs
      case _ => quotes.reflect.report.errorAndAbort("Unpacking StringContext.json args* is not supported")
    val jsonExpr: Expr[Json] = toJsonExpr(json, argExprs.iterator)
    Schema.refinedType(json, argExprs) match
      case '[t] => '{ $jsonExpr.asInstanceOf[t] }.asExprOf[Json]

  def jsonUnapplySeqExpr(jsonStringContext: Expr[JsonStringContext], scrutinee: Expr[Json])(using Quotes): Expr[Option[Seq[Json]]] =
    val jsonPattern: Pattern = parsed(jsonStringContext)
    // Exercise: partially evaluate the pattern matching
    '{ ${Expr(jsonPattern)}.unapplySeq($scrutinee) }

  private def parsed(jsonStringContext: Expr[JsonStringContext])(using Quotes): Pattern =
    jsonStringContext match
      case '{ jsonlib.json($sc) } =>
        val jsonString = sc.valueOrAbort.parts.map(scala.StringContext.processEscapes)
        Parser(jsonString).parse() match
          case Success(json) => json
          case Error(ParseError(msg, location)) =>
            def error(args: Seq[Expr[String]]) =
              import quotes.reflect.*
              val baseOffset = args(location.partIndex).asTerm.pos.start
              val pos = Position(jsonStringContext.asTerm.pos.sourceFile, baseOffset + location.offset, baseOffset + location.offset)
              report.errorAndAbort(msg, pos)
            sc match
              case '{ new scala.StringContext(${Varargs(args)}: _*) } => error(args)
              case '{     scala.StringContext(${Varargs(args)}: _*) } => error(args)
              case _ =>
                quotes.reflect.report.errorAndAbort("string context is not known statically")

  private def toJsonExpr(ast: Pattern, args: Iterator[Expr[Json]])(using Quotes): Expr[Json] =
    ast match
      case Pattern.Null => '{ null }
      case Pattern.Bool(value) => Expr(value)
      case Pattern.Num(value) => Expr(value)
      case Pattern.Str(value) => Expr(value)
      case Pattern.Arr(values*) =>
        val valueExprs = values.map(value => toJsonExpr(value, args))
        '{ JsonArray(${Varargs(valueExprs)}*) }
      case Pattern.Obj(nameValues*) =>
        val nameValueExprs =
          for (name, value) <- nameValues
          yield Expr.ofTuple(Expr(name), toJsonExpr(value, args))
        '{ JsonObject(${Varargs(nameValueExprs)}*) }
      case Pattern.InterpolatedValue => args.next()

  private given ToExpr[Pattern] with
    def apply(pattern: Pattern)(using Quotes): Expr[Pattern] =
      pattern match
        case Pattern.Null => '{ Pattern.Null }
        case Pattern.Bool(value) => '{ Pattern.Bool(${Expr(value)}) }
        case Pattern.Num(value) => '{ Pattern.Num(${Expr(value)}) }
        case Pattern.Str(value) => '{ Pattern.Str(${Expr(value)}) }
        case Pattern.Arr(values*) => '{ Pattern.Arr(${Expr(values)}*) }
        case Pattern.Obj(nameValue*) => '{ Pattern.Obj(${Expr(nameValue)}*) }
        case Pattern.InterpolatedValue => '{ Pattern.InterpolatedValue }
