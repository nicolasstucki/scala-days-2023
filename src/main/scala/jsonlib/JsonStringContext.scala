package jsonlib

import jsonlib.macros.JsonExpr.{jsonExpr, jsonUnapplySeqExpr}

extension (stringContext: scala.StringContext)
  @annotation.compileTimeOnly("Json.json should no be called directly. Use json\"...\" string interpolation.")
  def json: JsonStringContext = ???

type JsonStringContext

object JsonStringContext:

  extension (inline jsonStringContext: JsonStringContext)
    transparent inline def apply(inline args: Json*): Json =
      ${ jsonExpr('jsonStringContext, 'args) }

    inline def unapplySeq(scrutinee: Json): Option[Seq[Json]] =
      ${ jsonUnapplySeqExpr('jsonStringContext, 'scrutinee) }

    // Exercise: replace `unapplySeq` with transparent `unapply` that return a tuple.
    //           The macro should return a tuple of known size. Can start with
    //           `(Json, ..., Json)`). The the idea is to refine the types of the tuple
    //           based on the type of the scrutinee and the shape of the pattern.
    // transparent inline def unapply(inline scrutinee: Json): Option[Tuple] =