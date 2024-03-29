package jsonlib.macros

import scala.quoted.*

import jsonlib.*
import jsonlib.parser.*
import jsonlib.util.*
import scala.quoted.runtime.Patterns.patternType

private[jsonlib] object JsonExpr:

  def jsonExpr(jsonStringContext: Expr[JsonStringContext], argsExpr: Expr[Seq[Json]])(using Quotes): Expr[Json] =
    val json: Pattern =
      jsonStringContext match
        case '{ ($stringContext: StringContext).json } => parsed(stringContext)
        case _ => quotes.reflect.report.errorAndAbort("Expected call to extension method `json(StringContext): JsonStringContext`")
    val argExprs: Seq[Expr[Json]] = argsExpr match
      case Varargs(argExprs) => argExprs
      case _ => quotes.reflect.report.errorAndAbort("Unpacking StringContext.json args* is not supported")
    val jsonExpr: Expr[Json] = toJsonExpr(json, argExprs)
    // val refinedJsonType: Type[? <: Json] = Schema.refinedType(json, argExprs) // With SIP-53
    val refinedJsonType: Type[?] = Schema.refinedType(json, argExprs)
    refinedJsonType match
      case '[t] => '{ $jsonExpr.asInstanceOf[t] }.asExprOf[Json]

  def jsonUnapplySeqExpr(jsonStringContext: Expr[JsonStringContext], scrutinee: Expr[Json])(using Quotes): Expr[Option[Seq[Json]]] =
    jsonStringContext match
      case '{ ($stringContext: StringContext).json } =>
        val jsonPattern: Pattern = parsed(stringContext)
        // Exercise: inline the scrutinee, analyze its type and check if it could match the pattern. Warn if not.
        // Exercise: partially evaluate the pattern matching
        '{ ${Expr(jsonPattern)}.unapplySeq($scrutinee) }
      case _ => quotes.reflect.report.errorAndAbort("Expected call to extension method `json(StringContext): JsonStringContext`")

  private def parsed(stringContextExpr: Expr[StringContext])(using Quotes): Pattern =
    val stringContext: StringContext = stringContextExpr.valueOrAbort
    val jsonString: Seq[String] = stringContext.parts.map(scala.StringContext.processEscapes)
    Parser(jsonString).parse() match
      case Success(json) => json
      case Error(ParseError(msg, location)) =>
        def error(args: Seq[Expr[String]]) =
          import quotes.reflect.*
          val baseOffset = args(location.partIndex).asTerm.pos.start
          val pos = Position(stringContextExpr.asTerm.pos.sourceFile, baseOffset + location.offset, baseOffset + location.offset)
          report.errorAndAbort(msg, pos)
        stringContextExpr match
          case '{ new scala.StringContext(${Varargs(args)}: _*) } => error(args)
          case '{     scala.StringContext(${Varargs(args)}: _*) } => error(args)
          case _ =>
            quotes.reflect.report.errorAndAbort("string context is not known statically")

  private def toJsonExpr(ast: Pattern, args: Seq[Expr[Json]])(using Quotes): Expr[Json] =
    val argsIterator = args.iterator
    def rec(ast: Pattern): Expr[Json] =
      ast match
        case Pattern.Null => '{ null }
        case Pattern.Bool(value) => Expr(value)
        case Pattern.Num(value) => Expr(value)
        case Pattern.Str(value) => Expr(value)
        case Pattern.Arr(values*) =>
          val valueExprs: Seq[Expr[Json]] = values.map(rec)
          val valuesExpr: Expr[Seq[Json]] = Varargs(valueExprs)
          '{ JsonArray($valuesExpr*) }
        case Pattern.Obj(nameValues*) =>
          val nameExprValueExprs: Seq[(Expr[String], Expr[Json])]  =
            for (name, value) <- nameValues yield (Expr(name), rec(value))
          val nameValueExprs: Seq[Expr[(String, Json)]] = nameExprValueExprs.map(Expr.ofTuple)
          val nameValuesExpr: Expr[Seq[(String, Json)]] = Varargs(nameValueExprs)
          '{ JsonObject($nameValuesExpr*) }
        case Pattern.InterpolatedValue =>
          argsIterator.next()
    rec(ast)

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
