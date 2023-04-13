package jsonlib
package macros

import Schema.*

class TestSchema extends munit.FunSuite {

  test("intersection") {
    assertEquals(intersection(Null, Null), Null)
    assertEquals(intersection(Null, Num), Value)
    assertEquals(intersection(Null, Str), Value)
    assertEquals(intersection(Null, Bool), Value)
    assertEquals(intersection(Null, Value), Value)
    assertEquals(intersection(Null, Obj("a" -> Num)), Value)
    assertEquals(intersection(Null, Arr(Num)), Value)

    assertEquals(intersection(Num, Num), Num)
    assertEquals(intersection(Num, Null), Value)
    assertEquals(intersection(Num, Str), Value)
    assertEquals(intersection(Num, Bool), Value)
    assertEquals(intersection(Num, Value), Value)
    assertEquals(intersection(Num, Obj("a" -> Num)), Value)
    assertEquals(intersection(Num, Arr(Num)), Value)

    assertEquals(intersection(Str, Str), Str)
    assertEquals(intersection(Str, Num), Value)
    assertEquals(intersection(Str, Null), Value)
    assertEquals(intersection(Str, Bool), Value)
    assertEquals(intersection(Str, Value), Value)
    assertEquals(intersection(Str, Obj("a" -> Num)), Value)
    assertEquals(intersection(Str, Arr(Num)), Value)

    assertEquals(intersection(Bool, Bool), Bool)
    assertEquals(intersection(Bool, Num), Value)
    assertEquals(intersection(Bool, Null), Value)
    assertEquals(intersection(Bool, Str), Value)
    assertEquals(intersection(Bool, Value), Value)
    assertEquals(intersection(Bool, Obj("a" -> Num)), Value)
    assertEquals(intersection(Bool, Arr(Num)), Value)

    assertEquals(intersection(Obj("a" -> Num), Obj("a" -> Num)), Obj("a" -> Num))
    assertEquals(intersection(Obj("a" -> Num), Obj("a" -> Str)), Obj("a" -> Value))
    assertEquals(intersection(Obj("a" -> Num), Obj("a" -> Null)), Obj("a" -> Value))
    assertEquals(intersection(Obj("a" -> Num), Obj("a" -> Bool)), Obj("a" -> Value))
    assertEquals(intersection(Obj("a" -> Num, "b" -> Bool), Obj("a" -> Num, "c" -> Str)), Obj("a" -> Num))

    assertEquals(intersection(Arr(Num), Arr(Num)), Arr(Num))
    assertEquals(intersection(Arr(Bool), Arr(Bool)), Arr(Bool))
    assertEquals(intersection(Arr(Str), Arr(Str)), Arr(Str))
    assertEquals(intersection(Arr(Null), Arr(Null)), Arr(Null))
    assertEquals(intersection(Arr(Num), Arr(Str)), Arr(Value))
    assertEquals(intersection(Arr(Num), Arr(Null)), Arr(Value))
    assertEquals(intersection(Arr(Num), Arr(Bool)), Arr(Value))
    assertEquals(intersection(Arr(Num), Arr(Obj("a" -> Num))), Arr(Value))

  }
}
