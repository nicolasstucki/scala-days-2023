package jsonlib.parser

private[jsonlib] final case class ParseError(msg: String, location: Location)
