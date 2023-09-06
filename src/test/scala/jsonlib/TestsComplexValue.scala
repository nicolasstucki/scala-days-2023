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
  val user2 = json"""{
    "firstName": "John"
  }"""

  val stringArray = json"""[ "name1", "name2" ]"""
  val objectWithArray = json"""{ "arr": $stringArray }"""

  test("selection") {
    assertEquals(account.active: Boolean, true)
    assertEquals(account.user.firstName: String, "John")
  }

  test("selection1") {
    assertEquals(objectWithArray.arr(0): String, "name1")
    assertEquals(objectWithArray.arr(1): String, "name2")
  }

  test("pattern matching") {
    (account: Json) match
      case json"""{ "user": $x, "a": true }""" => ???
      case json"""{ "user": $x, "active": $y }""" =>
        assertEquals(x: Json, user) // TODO refine the type of x if possible
        assertEquals(y: Json, true) // TODO refine the type of y if possible
      case _ =>

    json"[1, 2, 3]" match
      case json"[1, 2]" => ???
      case json"[1, 2, 3]" => // OK

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
