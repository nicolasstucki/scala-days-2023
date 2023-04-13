package jsonlib

import scala.language.dynamics

import parser.*
import util.*

type Json = JsonObject | JsonArray | Double | String | Boolean | Null

object Json:
  def apply(json: String): Json =
    Parser(Seq(json)).parse() match
      case Success(ast) => fromPattern(ast)
      case Error(ParseError(msg, location)) =>
        ???

  private def fromPattern(ast: Pattern): Json =
    ast match
      case Pattern.Null => null
      case Pattern.Bool(value) => value
      case Pattern.Num(value) => value
      case Pattern.Str(value) => value
      case Pattern.Arr(values*) => JsonArray(values.map(fromPattern)*)
      case Pattern.Obj(nameValues*) =>
        val nameJsons = for (name, value) <- nameValues yield (name, fromPattern(value))
        JsonObject(nameJsons*)
      case Pattern.InterpolatedValue =>
        assert(false, "unexpected Pattern containing interpolated values: " + ast)
