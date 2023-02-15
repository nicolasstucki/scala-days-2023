package jsonlib
package macros

import scala.quoted.*

import jsonlib.parser.*
import jsonlib.util.*

private enum Schema:
  case Value
  case Obj(nameSchemas: (String, Schema)*)
  case Arr
  case Str
  case Num
  case Bool
  case Null

private object Schema:

  def refinedType(pattern: Pattern, args: Seq[Expr[Json]])(using Quotes): Type[?] =
    refinedType(schema(pattern, args.iterator))

  private def refinedType(schema: Schema)(using Quotes): Type[?] =
    schema match
      case Schema.Value => Type.of[Json]
      case Schema.Obj(nameSchemas*) =>
        import quotes.reflect.*
        nameSchemas.foldLeft(TypeRepr.of[JsonObject]) { case (acc, (name, schema)) =>
          Refinement(acc, name, TypeRepr.of(using refinedType(schema)))
        }.asType
      case Schema.Arr => Type.of[JsonArray]
      case Schema.Str => Type.of[String]
      case Schema.Num => Type.of[Double]
      case Schema.Bool => Type.of[Boolean]
      case Schema.Null => Type.of[Null]

  private def schema(pattern: Pattern, args: Iterator[Expr[Json]])(using Quotes): Schema =
    pattern match
      case Pattern.Obj(nameValues*) =>
        val nameSchemas = for (name, value) <- nameValues yield (name, schema(value, args))
        Schema.Obj(nameSchemas*)
      case Pattern.Arr(_*) => Schema.Arr
      case Pattern.Str(_) => Schema.Str
      case Pattern.Num(_) => Schema.Num
      case Pattern.Bool(_) => Schema.Bool
      case Pattern.Null => Schema.Null
      case Pattern.InterpolatedValue =>
        args.next() match
          case '{ $x : t } => schemaOf[t]

  private def schemaOf[T : Type](using Quotes): Schema =
    Type.of[T] match
      case '[Null] => Schema.Null
      case '[Boolean] => Schema.Bool
      case '[String] => Schema.Str
      case '[Double] => Schema.Num
      case '[JsonArray] => Schema.Arr
      case '[JsonObject] =>
        import quotes.reflect.*
        def refinements(tpe: TypeRepr): Vector[(String, Schema)] =
          tpe match
            case Refinement(parent, name, info) =>
              val  refinedSchema = info.asType match
                case '[t] => schemaOf[t]
              refinements(parent) :+ (name, refinedSchema)
            case _ => Vector()
        Schema.Obj(refinements(TypeRepr.of[T].widenTermRefByName)*)
      case _ => Schema.Value
