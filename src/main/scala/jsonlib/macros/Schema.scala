package jsonlib
package macros

import scala.quoted.*

import jsonlib.parser.*
import jsonlib.util.*

private enum Schema:
  case Value
  case Obj(nameSchemas: (String, Schema)*)
  case Arr(elemSchema: Schema)
  case Str
  case Num
  case Bool
  case Null
  // exercise: add Schema.Or to make union more precise
    //   case Or(schema1: Schema, schema2: Schema)

private object Schema:

  def refinedType(pattern: Pattern, args: Seq[Expr[Json]])(using Quotes): Type[? <: Json] =
    val jsonSchema: Schema = schema(pattern, args)
    refinedType(jsonSchema)

  private def refinedType(schema: Schema)(using Quotes): Type[? <: Json] =
    schema match
      case Schema.Value => Type.of[Json]
      case Schema.Obj(nameSchemas*) =>
        import quotes.reflect.*
        val refined = nameSchemas.foldLeft(TypeRepr.of[JsonObject]) { case (acc, (name, schema)) =>
          refinedType(schema) match
            case '[t] => Refinement(acc, name, TypeRepr.of[t])
        }
        refined.asType.asInstanceOf[Type[? <: JsonObject]]
        // With SIP-53:
        // refined.asType match
        //   case '[type t <: JsonObject; t] => Type.of[t]
      case Schema.Arr(elemSchema) =>
        refinedType(elemSchema) match
          case '[t] => Type.of[ JsonArray { def apply(idx: Int): t } ]
      case Schema.Str => Type.of[String]
      case Schema.Num => Type.of[Double]
      case Schema.Bool => Type.of[Boolean]
      case Schema.Null => Type.of[Null]

  private def schema(pattern: Pattern, args: Seq[Expr[Json]])(using Quotes): Schema =
    val argsIterator = args.iterator
    def rec(pattern: Pattern): Schema =
      pattern match
        case Pattern.Obj(nameValues*) =>
          val nameSchemas: Seq[(String, Schema)] =
            for (name, value) <- nameValues
            yield (name, rec(value))
          Schema.Obj(nameSchemas*)
        case Pattern.Arr() => Schema.Arr(Schema.Value)
        case Pattern.Arr(patterns*) =>
          val elementSchema: Schema =
            patterns
              .map(pattern => rec(pattern))
              .reduce(union)
          Schema.Arr(elementSchema)
        case Pattern.Str(_) => Schema.Str
        case Pattern.Num(_) => Schema.Num
        case Pattern.Bool(_) => Schema.Bool
        case Pattern.Null => Schema.Null
        // Version 1: Only take into account the pattern
        //  case Pattern.InterpolatedValue => Schema.Value
        // Version 2: Take into account the statically known type of the interpolated value
        case Pattern.InterpolatedValue =>
          argsIterator.next() match
            // case '{ type t <: Json; $x : t } => schemaOf[t] // With SIP-53
            case '{ $x : t } => schemaOf[t]
    rec(pattern)

  // private def schemaOf[T <: Json](using Type[T])(using Quotes): Schema = // With SIP-53
  private def schemaOf[T](using Type[T])(using Quotes): Schema =
    Type.of[T] match
      case '[Null] => Schema.Null
      case '[Boolean] => Schema.Bool
      case '[String] => Schema.Str
      case '[Double] => Schema.Num
      case '[JsonArray] =>
        import quotes.reflect.*
        val  refinedSchema =
          TypeRepr.of[T].widen match
            case Refinement(parent, "apply", MethodType(_, _, resType)) =>
              resType.asType match
                // case '[type t <: Json; t] => schemaOf[t] // With SIP-53
                case '[t] => schemaOf[t]
            case _ => Schema.Value
        Schema.Arr(refinedSchema)
      case '[JsonObject] =>
        import quotes.reflect.*
        def refinements(tpe: TypeRepr): Vector[(String, Schema)] =
          tpe match
            case Refinement(parent, name, info) =>
              val  refinedSchema = info.asType match
                // case '[type t <: Json; t] => schemaOf[t] // With SIP-53
                case '[t] => schemaOf[t]
              refinements(parent) :+ (name, refinedSchema)
            case _ => Vector()
        Schema.Obj(refinements(TypeRepr.of[T].widen)*)
      case _ => Schema.Value

  def union(schema1: Schema, schema2: Schema): Schema =
    (schema1, schema2) match
      case (Schema.Num, Schema.Num) => Schema.Num
      case (Schema.Str, Schema.Str) => Schema.Str
      case (Schema.Bool, Schema.Bool) => Schema.Bool
      case (Schema.Null, Schema.Null) => Schema.Null
      case (Schema.Arr(elemSchema1), Schema.Arr(elemSchema2)) =>
        Schema.Arr(union(elemSchema1, elemSchema2))
      case (Schema.Obj(nameSchemas1*), Schema.Obj(nameSchemas2*)) =>
        val nameSchemas =
          for
            (name1, valueSchema1) <- nameSchemas1
            (name2, valueSchema2) <- nameSchemas2
            if name1 == name2
          yield (name1, union(valueSchema1, valueSchema2))
        Schema.Obj(nameSchemas*)
      case _ => Schema.Value
