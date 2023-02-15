package jsonlib.parser

private enum Token:
  case Null
  case False
  case True
  case Num(value: String)
  case OpenBrace
  case CloseBrace
  case OpenBracket
  case CloseBracket
  case Comma
  case Colon
  case Str(value: String)
  case InterpolatedValue
  case End
  case Error(msg: String, location: Location)
