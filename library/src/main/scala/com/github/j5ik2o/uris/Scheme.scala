package com.github.j5ik2o.uris

import java.text.ParseException

final case class Scheme(value: String)

object Scheme {

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
