val scala3Version = "3.3.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Implementing a Macro in Scala 3",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,

    testFrameworks += new TestFramework("munit.Framework"),
  )
