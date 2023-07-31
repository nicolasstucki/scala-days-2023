package jsonlib

import scala.language.dynamics

final class JsonObject(private val nameValues: Map[String, Json]) extends scala.Selectable:
  def selectDynamic(name: String): Json | JsonObject.Undefined.type =
    nameValues.getOrElse(name, JsonObject.Undefined)

  def asMap: Map[String, Json] = nameValues

  override def toString(): String = nameValues.iterator.map {
    (name, value) => s"\"$name\" -> $value" // TODO escape characters of name
  }.mkString("{", ", ", "}")

  override def equals(x: Any): Boolean = x match
    case x: JsonObject => this.nameValues == x.nameValues
    case _ => false

  override def hashCode(): Int =
    nameValues.hashCode()

object JsonObject:

  object Undefined

  def apply(keyValues: (String, Json)*): JsonObject =
    // TODO check that keys are unique?
    new JsonObject(Map(keyValues*))
