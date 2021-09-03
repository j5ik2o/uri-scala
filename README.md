# uri-scala

[![CI](https://github.com/j5ik2o/uri-scala/workflows/CI/badge.svg)](https://github.com/j5ik2o/uri-scala/actions?query=workflow%3ACI)
[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.j5ik2o/uri-scala_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.j5ik2o/uri-scala_2.13)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)

This library provides a simple URI parsing and model.

## Installation

Add the following to your sbt build (2.13.x):

```scala
val version = "..."

libraryDependencies += Seq(
  "com.github.j5ik2o" %% "uri-scala" % version,
)
```

## Usage

This library provides a parser for URI and a model for URI.

```scala
object Example extends App {

  val uriText = "http://example.com/abc?key1=abc"

  val uri = Uri.parseWithException(uriText)

  println(s"uri = ${uri.asString}")
  println(s"uri.scheme = ${uri.scheme.asString}")
  println(s"uri.authority = ${uri.authority.asString}")
  println(s"uri.path = ${uri.path.asString}")
  println(s"uri.query = ${uri.query.map(_.asString)}")
  println(s"uri.fragment = ${uri.fragment}")

  assert(uri.asString == uriText)

}
```
