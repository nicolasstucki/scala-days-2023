package jsonlib

import scala.compiletime.testing.*

class ParseFailures extends munit.FunSuite {

  // TODO improve error messages
  assertCompileErrorEquals("""json"n u l l"""", "[7] expected `u` (of null)")
  assertCompileErrorEquals("json\"\"\"n u l l\"\"\"", "[9] expected `u` (of null)")
  assertCompileErrorEquals("""json"a"""", "[6] unexpected start of token a")
  assertCompileErrorEquals("""json"a${n}"""", "[6] unexpected start of token a")
  assertCompileErrorEquals("""json"[${n}a ${n}"""", "[11] expected token CloseBracket but got Error(unexpected start of token a,Location(1, 1))")
  assertCompileErrorEquals("""json"[${n},a ${n}"""", "[12] unexpected start of token a")
  assertCompileErrorEquals("""json"[${n} ${n}"""", "[15] expected token CloseBracket but got InterpolatedValue")
  assertCompileErrorEquals("""json"[${n} ${n},"""", "[15] expected token CloseBracket but got InterpolatedValue")
  assertCompileErrorEquals("""json"[true, """", "[12] unexpected start of value: End")
  assertCompileErrorEquals("""json"[true, ,"""", "[13] unexpected start of value: Comma")
  assertCompileErrorEquals("""json"{"""", "[6] expected string literal")
  assertCompileErrorEquals("""json"{"ab":"""", "[7] end of statement expected but string interpolator found")
  assertCompileErrorEquals("""json"{}}"""", "[8] expected token End but got CloseBrace")
  assertCompileErrorEquals("""json"}"""", "[6] unexpected start of value: CloseBrace")
  assertCompileErrorEquals("""json"--1"""", "[7] Unexpected character in number: -")
  assertCompileErrorEquals("""json"-"""", "[6] unexpected end")

  private inline def assertCompileErrorEquals(inline code: String, inline expected: String): Unit =
    test(code) {
      val errors = typeCheckErrors(code)
      assertEquals( s"[${errors.head.column}] ${errors.head.message}", expected)
    }
}
