package jsonlib

class TestsComplexValue extends munit.FunSuite {

  val user = json"""{
    "firstName": "John",
    "lastName": "Doe"
  }"""
  val bool = json"true"
  val account = json"""{
    "user": $user,
    "active": $bool
  }"""
  test("selection") {
    assertEquals(account.active: Boolean, true)
    assertEquals(account.user.firstName: String, "John")
  }

  test("pattern matching") {
    (account: Json) match
      case json"""{ "user": $x, "a": true }""" => ???
      case json"""{ "user": $x, "active": $y }""" =>
        assertEquals(x: Json, user) // TODO refine the type of x if possible
        assertEquals(y: Json, true) // TODO refine the type of y if possible
      case _ =>
  }
  test("slides example") {
    val firstTalk: JsonObject { val name: String; val speaker: String } =
      json""" { "name": "Resource Management Made Easy", "speaker": "Julien Truffaut" } """
    val secondTalk: JsonObject { val name: String; val speaker: String } =
      json""" { "name": "Implementing A Macro", "speaker": "Nicolas Stucki" } """
    val talks: JsonArray { def apply(idx: Int): JsonObject { val name: String; val speaker: String } }  =
      json" [ $firstTalk, $secondTalk ] "

    val name: String = talks(0).name
  }
}
