package com.github.j5ik2o.uri

object Example extends App {

  val uriText = "http://yahoo.co.jp/abc?key1=abc"

  val uri = Uri.parseWithException(uriText)

  println(s"uri = ${uri.asString}")
  println(s"uri.scheme = ${uri.scheme.asString}")
  println(s"uri.authority = ${uri.authority.asString}")
  println(s"uri.path = ${uri.path.asString}")
  println(s"uri.query = ${uri.query.map(_.asString)}")
  println(s"uri.fragment = ${uri.fragment}")

  assert(uri.asString == uriText)

}
