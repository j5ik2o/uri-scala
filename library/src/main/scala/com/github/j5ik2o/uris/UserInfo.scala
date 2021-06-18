package com.github.j5ik2o.uris

import java.text.ParseException

case class UserInfo(user: String, password: Option[String] = None) {
  override def toString = s"$user${password.fold("")(v => s":$v")}"
}

object UserInfo {

  def parse(s: CharSequence): Either[ParseException, UserInfo] = {
    import fastparse._
    val parsed = fastparse.parse(s.toString, UriParser.userInfo(_))
    if (parsed.isSuccess) {
      val Parsed.Success(result, _) = parsed
      Right(result)
    } else {
      val Parsed.Failure(msg, index, _) = parsed
      Left(new ParseException(msg, index))
    }
  }

}
