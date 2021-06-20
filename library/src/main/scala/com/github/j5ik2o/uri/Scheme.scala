package com.github.j5ik2o.uri

import java.text.ParseException

final case class Scheme(value: String) {
  def asString: String = value
}

object Scheme {

  def parseWithException(s: CharSequence): Scheme =
    parse(s.toString).fold(throw _, identity)

  def parse(s: CharSequence): Either[ParseException, Scheme] = {
    import fastparse._
    val parsed = fastparse.parse(s.toString, UriParser.scheme(_))
    if (parsed.isSuccess) {
      val Parsed.Success(result, _) = parsed
      Right(result)
    } else {
      val Parsed.Failure(msg, index, _) = parsed
      Left(new ParseException(msg, index))
    }
  }

}
