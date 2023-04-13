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
      case Schema.Arr(elemSchema) =>
        refinedType(elemSchema) match
          case '[t] => Type.of[JsonArray { def apply(idx: Int): t } ]
      case Schema.Str => Type.of[String]
      case Schema.Num => Type.of[Double]
      case Schema.Bool => Type.of[Boolean]
      case Schema.Null => Type.of[Null]

  private def schema(pattern: Pattern, args: Iterator[Expr[Json]])(using Quotes): Schema =
    pattern match
      case Pattern.Obj(nameValues*) =>
        val nameSchemas: Seq[(String, Schema)] =
          for (name, value) <- nameValues
          yield (name, schema(value, args))
        Schema.Obj(nameSchemas*)
      case Pattern.Arr() => Schema.Arr(Schema.Value)
      case Pattern.Arr(patterns*) =>
        val elementSchema: Schema =
          patterns
            .map(pattern => schema(pattern, args))
            .reduce(intersection)
        Schema.Arr(elementSchema)
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
      case '[JsonArray] => Schema.Arr(Schema.Value) // TODO refine element type
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

  def intersection(schema1: Schema, schema2: Schema): Schema =
    (schema1, schema2) match
      case (Schema.Num, Schema.Num) => Schema.Num
      case (Schema.Str, Schema.Str) => Schema.Str
      case (Schema.Bool, Schema.Bool) => Schema.Bool
      case (Schema.Null, Schema.Null) => Schema.Null
      case (Schema.Arr(elemSchema1), Schema.Arr(elemSchema2)) =>
        Schema.Arr(intersection(elemSchema1, elemSchema2))
      case (Schema.Obj(nameSchemas1*), Schema.Obj(nameSchemas2*)) =>
        val nameSchemas =
          for
            (name1, valueSchema1) <- nameSchemas1
            (name2, valueSchema2) <- nameSchemas2
            if name1 == name2
          yield (name1, intersection(valueSchema1, valueSchema2))
        Schema.Obj(nameSchemas*)
      case _ => Schema.Value
