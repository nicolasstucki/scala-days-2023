package jsonlib.parser

private class InterpolatedChars(val source: Seq[String]):

  private var part: Int = 0
  private var offset: Int = 0

  def atPartEnd: Boolean =
    offset == source(part).length

  def atEnd: Boolean =
    part == source.length - 1 && atPartEnd

  def atInterpolation: Boolean =
    part < source.length - 1 && atPartEnd

  def nextPart(): Unit =
    part += 1
    offset = 0

  def peekChar(): Char =
    source(part)(offset)

  def nextChar(): Char =
    val char = peekChar()
    offset += 1
    char

  def skipWhiteSpaces(): Unit =
    while part < source.length && offset < source(part).length && (peekChar().isWhitespace || peekChar() == '\n') do
      offset += 1

  def location: Location = Location(part, offset)
