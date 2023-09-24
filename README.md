# uri-scala

[![CI](https://github.com/j5ik2o/uri-scala/workflows/CI/badge.svg)](https://github.com/j5ik2o/uri-scala/actions?query=workflow%3ACI)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.j5ik2o/uri-scala_2.13/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.j5ik2o/uri-scala_2.13)
[![Renovate](https://img.shields.io/badge/renovate-enabled-brightgreen.svg)](https://renovatebot.com)
[![License](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
[![Tokei](https://tokei.rs/b1/github/j5ik2o/uri-scala)](https://github.com/XAMPPRocky/tokei)

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
