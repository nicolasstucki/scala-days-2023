package jsonlib

final class JsonArray(private[jsonlib] val values: Json*):
  def apply(idx: Int): Json = values(idx)
  def length: Int = values.length

  override def toString(): String = values.mkString("[", ", ", "]")

  override def equals(x: Any): Boolean = x match
    case x: JsonArray => this.values == x.values
    case _ => false

  override def hashCode(): Int =
    values.hashCode()
