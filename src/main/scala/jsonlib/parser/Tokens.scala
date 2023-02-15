package jsonlib.parser

import scala.util.boundary

import jsonlib.util.*

private class Tokens(chars: InterpolatedChars):

  private var nextToken: Token = null

  def peek(): Token =
    if nextToken eq null then
      nextToken = readToken()
    nextToken

  def next(): Token =
    val res = peek()
    if nextToken ne Token.End then
      nextToken = null
    res

  def location: Location = chars.location

  private def readToken(): Token =
    boundary:
      chars.skipWhiteSpaces()
      if chars.atEnd then
        Token.End
      else if chars.atInterpolation then
        chars.nextPart()
        Token.InterpolatedValue
      else chars.peekChar() match
        case '{' => accept('{', Token.OpenBrace)
        case '}' => accept('}', Token.CloseBrace)
        case '[' => accept('[', Token.OpenBracket)
        case ']' => accept(']', Token.CloseBracket)
        case 'n' => accept("null", Token.Null)
        case 'f' => accept("false", Token.False)
        case 't' => accept("true", Token.True)
        case ',' => accept(',', Token.Comma)
        case ':' => accept(':', Token.Colon)
        case '"' => readString()
        case '-' => readNum()
        case c if '0' <= c && c <= '9' => readNum()
        case _ =>
          Token.Error(s"unexpected start of token ${chars.nextChar()}", chars.location)

  private def readString()(using boundary.Label[Token]): Token =
    assert(nextCharOrError() == '"')
    val stringBuffer = new collection.mutable.StringBuilder()
    def parseChars(): String =
      nextCharOrError() match
        case '"' => stringBuffer.result()
        case '\\' =>
          nextCharOrError() match
            case '\\' => stringBuffer += '\\'
            case '"' => stringBuffer += '"'
            case '/' => stringBuffer += '/'
            case 'b' => stringBuffer += '\b'
            case 'f' => stringBuffer += '\f'
            case 'n' => stringBuffer += '\n'
            case 'r' => stringBuffer += '\r'
            case 't' => stringBuffer += '\t'
            case 'u' =>
              def hex =
                val c = nextCharOrError()
                if ('0' <= c && c <= '9') || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f') then c
                else error("Expected hex digit [0-9a-fA-F]")
              val num = Integer.parseInt(hex.toString + hex + hex + hex, 16)
              stringBuffer += num.toChar
            case _ =>
              error("unexpected escaped character")
          parseChars()
        case char if char.isControl =>
          error("unexpected control character")
        case char =>
          stringBuffer += char
          parseChars()
    Token.Str(parseChars())

  private def readNum()(using boundary.Label[Token]): Token =
    val num = new StringBuilder

    def readDigits() =
      def peekIsDigit() =
        if atEndOfToken then false
        else
          val char = chars.peekChar()
          '0' <= char && char <= '9'
      while peekIsDigit() do num += nextCharOrError()

    def readExponent() =
      if atEndOfToken then ()
      else chars.peekChar() match
        case 'e' | 'E' =>
          num += nextCharOrError()
          chars.peekChar() match
            case '+' => num += nextCharOrError()
            case '-' => num += nextCharOrError()
            case _ =>
          readDigits()
          if !atEndOfToken then
            error(s"Expected end of number")
        case c =>
          error(s"Unexpected character in number exponent: $c")

    def readDecimal() =
      if atEndOfToken then ()
      else
        chars.peekChar() match
          case '.' =>
            num += nextCharOrError()
            readDigits()
          case _ =>
        readExponent()

    def readPosNum() =
      val char = nextCharOrError()
      num += char
      if '1' <= char && char <= '9' then
        readDigits()
      else if char != '0' then
        error(s"Unexpected character in number: $char")
      readDecimal()

    if !chars.atPartEnd && chars.peekChar() == '-' then
      num += nextCharOrError()
    readPosNum()

    Token.Num(num.toString)

  private def nextCharOrError()(using boundary.Label[Token]): Char =
    if chars.atPartEnd then error("unexpected end")
    else chars.nextChar()

  private def accept(char: Char, token: Token)(using boundary.Label[Token]): Token =
    nextCharOrError() match
      case `char` => token
      case next =>
        error(s"unexpected character: got $char but got $next")

  private def atEndOfToken: Boolean =
    if chars.atPartEnd then true
    else
      val char = chars.peekChar()
      char.isWhitespace || char == '}' || char == ']' || char == ','

  private def accept(str: String, token: Token)(using boundary.Label[Token]): Token =
    for char <- str if char != nextCharOrError() do //
      error(s"expected `$char` (of $str)")
    if !chars.atPartEnd && !atEndOfToken then
      error("expected end of token")
    else
      token

  private def error(msg: String)(using boundary.Label[Token]): Nothing =
    boundary.break(Token.Error(msg, chars.location))
