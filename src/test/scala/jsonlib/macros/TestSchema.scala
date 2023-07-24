package jsonlib
package macros

import Schema.*

class TestSchema extends munit.FunSuite {

  test("union") {
    assertEquals(union(Null, Null), Null)
    assertEquals(union(Null, Num), Value)
    assertEquals(union(Null, Str), Value)
    assertEquals(union(Null, Bool), Value)
    assertEquals(union(Null, Value), Value)
    assertEquals(union(Null, Obj("a" -> Num)), Value)
    assertEquals(union(Null, Arr(Num)), Value)

    assertEquals(union(Num, Num), Num)
    assertEquals(union(Num, Null), Value)
    assertEquals(union(Num, Str), Value)
    assertEquals(union(Num, Bool), Value)
    assertEquals(union(Num, Value), Value)
    assertEquals(union(Num, Obj("a" -> Num)), Value)
    assertEquals(union(Num, Arr(Num)), Value)

    assertEquals(union(Str, Str), Str)
    assertEquals(union(Str, Num), Value)
    assertEquals(union(Str, Null), Value)
    assertEquals(union(Str, Bool), Value)
    assertEquals(union(Str, Value), Value)
    assertEquals(union(Str, Obj("a" -> Num)), Value)
    assertEquals(union(Str, Arr(Num)), Value)

    assertEquals(union(Bool, Bool), Bool)
    assertEquals(union(Bool, Num), Value)
    assertEquals(union(Bool, Null), Value)
    assertEquals(union(Bool, Str), Value)
    assertEquals(union(Bool, Value), Value)
    assertEquals(union(Bool, Obj("a" -> Num)), Value)
    assertEquals(union(Bool, Arr(Num)), Value)

    assertEquals(union(Obj("a" -> Num), Obj("a" -> Num)), Obj("a" -> Num))
    assertEquals(union(Obj("a" -> Num), Obj("a" -> Str)), Obj("a" -> Value))
    assertEquals(union(Obj("a" -> Num), Obj("a" -> Null)), Obj("a" -> Value))
    assertEquals(union(Obj("a" -> Num), Obj("a" -> Bool)), Obj("a" -> Value))
    assertEquals(union(Obj("a" -> Num, "b" -> Bool), Obj("a" -> Num, "c" -> Str)), Obj("a" -> Num))

    assertEquals(union(Arr(Num), Arr(Num)), Arr(Num))
    assertEquals(union(Arr(Bool), Arr(Bool)), Arr(Bool))
    assertEquals(union(Arr(Str), Arr(Str)), Arr(Str))
    assertEquals(union(Arr(Null), Arr(Null)), Arr(Null))
    assertEquals(union(Arr(Num), Arr(Str)), Arr(Value))
    assertEquals(union(Arr(Num), Arr(Null)), Arr(Value))
    assertEquals(union(Arr(Num), Arr(Bool)), Arr(Value))
    assertEquals(union(Arr(Num), Arr(Obj("a" -> Num))), Arr(Value))

  }
}
