package jsonlib

class TestsValues extends munit.FunSuite {

  test("null values") {
    assertEquals(json"null" : Null, null)
    assertEquals(json" null " : Null, null)

    assertEquals(json"${ json"null" }" : Null, null)
  }

  test("boolean values") {
    assertEquals(json"true" : Boolean, true)
    assertEquals(json"false" : Boolean, false)
    assertEquals(json" false " : Boolean, false)

    assertEquals(json"${ json"true" }" : Boolean, true)
  }

  test("number values") {
    assertEquals(json"0" : Double, 0.0)
    assertEquals(json"-0" : Double, -0.0)
    assertEquals(json"1" : Double, 1.0)
    assertEquals(json"-1" : Double, -1.0)
    assertEquals(json"-1.1" : Double, -1.1)
    assertEquals(json"15345.15445" : Double, 15345.15445)
    assertEquals(json"-15345.15445" : Double, -15345.15445)
    assertEquals(json"-1.1e33" : Double, -1.1e33)
    assertEquals(json"-1.1e+34" : Double, -1.1e+34)
    assertEquals(json"-1.1e-35" : Double, -1.1e-35)
    assertEquals(json"-1e-35" : Double, -1e-35)
    assertEquals(json"-1e+35" : Double, -1e+35)
    assertEquals(json"-1E-35" : Double, -1E-35)
    assertEquals(json"-1E+35" : Double, -1E+35)

    assertEquals(json"${ 1.0 }" : Double, 1.0)
    assertEquals(json"${ json"1" }" : Double, 1.0)
  }

  test("string values") {
    assertEquals(json"\"\"" : String, "")
    assertEquals(json"""""""" : String, "")
    assertEquals(json"\"abc\"" : String, "abc")
    assertEquals(json""""abc"""" : String, "abc")
    assertEquals(json"\"\\n\"" : String, "\n")
    assertEquals(json"\"\\t\"" : String, "\t")
    assertEquals(json""""\\\\"""" : String, "\\")
    assertEquals(json"\"\\u0046\"" : String, "\u0046")
    assertEquals(json"\"\\u1A3F\"" : String, "\u1A3F")
    assertEquals(json"\"\\u1a3f\"" : String, "\u1a3f")
    assertEquals(json"\"\\u1a3f\"" : String, "\u1a3f")

    assertEquals(json"${ "abc" }" : String, "abc")
    assertEquals(json"${ json"\"abc\"" }" : String, "abc")
  }

  test("array values") {
    assertEquals(json"[]", JsonArray())
    assertEquals(json"[true]": JsonArray, JsonArray(true))
    assertEquals(json"[true, null]": JsonArray, JsonArray(true, null))
    assertEquals(json"[[[]]]": JsonArray, JsonArray(JsonArray(JsonArray())))
    assertEquals(json"[true]"(0): Boolean, true)

    assertEquals(json"${json"[]"}", JsonArray())
    assertEquals(json"[${json"[]"}]": JsonArray, JsonArray(JsonArray()))
  }

  test("object values") {
    assertEquals(json"{}", JsonObject())
    assertEquals(json"""{ "name": true }""" : JsonObject, JsonObject("name" -> true))
    assertEquals(json"""{ "name": true, "name2": false  }""": JsonObject, JsonObject("name" -> true, "name2" -> false))

    assertEquals(json"""{ "name": true }""".name, true)
    assertEquals(json"""{ "name": true, "name2": false }""".name, true)
    assertEquals(json"""{ "name": true, "name2": false }""".name2, false)

    assertEquals(json"""{ "name": ${ true } }""" : JsonObject, JsonObject("name" -> true))
    assertEquals(json"""{ "name": ${ json"true" } }""" : JsonObject, JsonObject("name" -> true))
    assertEquals(json"""{ "name": ${ json"true" } }""" : JsonObject, JsonObject("name" -> true))
  }

}
