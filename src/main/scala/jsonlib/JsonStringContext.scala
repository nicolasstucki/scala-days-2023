package jsonlib

import jsonlib.macros.JsonExpr.{jsonExpr, jsonUnapplySeqExpr}

extension (stringContext: scala.StringContext)
  @annotation.compileTimeOnly("Json.json should have been removed by macro")
  def json: JsonStringContext = ???

type JsonStringContext

object JsonStringContext:

  extension (inline jsonStringContext: JsonStringContext)
    transparent inline def apply(inline args: Json*): Json =
      ${ jsonExpr('jsonStringContext, 'args) }

    inline def unapplySeq(scrutinee: Json): Option[Seq[Json]] =
      ${ jsonUnapplySeqExpr('jsonStringContext, 'scrutinee) }
