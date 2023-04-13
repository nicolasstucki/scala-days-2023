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
}
