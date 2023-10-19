## Scala Days 2023 - Implementing a Macro in Scala 3


### Scala Days Presentation

* Talk: https://www.youtube.com/watch?v=dKblZynnhgo
* Slides: https://nicolasstucki.github.io/scala-days-2023/

### Usage

This is a normal sbt project. You can compile code with `sbt compile`, test it with `sbt test`, and `sbt console` will start a Scala 3 REPL.

### JSON representation


```scala
type Json = JsonObject | JsonArray | Double | String | Boolean | Null
```

### Macro JSON string interpolation

The string interpolator `json` defined in [JsonStringContext.scala](src/main/scala/jsonlib/JsonStringContext.scala) provides shows how to implement the interpolator `apply` and `unapply` with macros.

#### Crate refined JSON with an interpolator
```scala
val user = json"""{
    "firstName": "John",
    "lastName": "Doe"
  }"""
val bool = json"true"
val account = json"""{
    "user": $user,
    "active": $bool
}"""
```
where the type of `account` is refined to
```scala
account: JsonObject {
  val user: JsonObject {
    val firstName: String
    val lastName:String
  }
  val active: Boolean
}
```

#### Extracting refined JSON with an interpolator

```scala
(account: Json) match
  case json"""{ "user": $x, "active": true }""" => println(x + " is active")
  case _ =>
```
