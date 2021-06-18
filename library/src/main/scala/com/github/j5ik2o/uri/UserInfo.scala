package com.github.j5ik2o.uri

import java.text.ParseException

case class UserInfo(user: String, password: Option[String] = None) {
  override def toString: String = s"$user${password.fold("")(v => s":$v")}"
}

object UserInfo {

  def parseWithException(user: String, password: Option[String] = None): UserInfo = {
    parse(user, password).fold(throw _, identity)
  }

  def parse(user: String, password: Option[String] = None): Either[ParseException, UserInfo] = {
    parse(new UserInfo(user, password).toString)
  }

  def parseWithException(s: CharSequence): UserInfo =
    parse(s).fold(throw _, identity)

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
