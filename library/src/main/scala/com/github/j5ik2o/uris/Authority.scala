package com.github.j5ik2o.uris

import java.text.ParseException

case class Authority(hostName: String, port: Option[Int], userInfo: Option[UserInfo]) {

  def withHostName(value: String): Authority = copy(hostName = value)

  def withPort(value: Option[Int]): Authority = copy(port = value)

  def withUserInfo(value: Option[UserInfo]): Authority = copy(userInfo = value)

  override def toString = s"${userInfo.fold("")(v => s"$v@")}$hostName${port.fold("")(v => s":$v")}"
}

object Authority {

  def parse(s: CharSequence): Either[ParseException, Authority] = {
    import fastparse._
    val parsed = fastparse.parse(s.toString, UriParser.authority(_))
    if (parsed.isSuccess) {
      val Parsed.Success(result, _) = parsed
      Right(result)
    } else {
      val Parsed.Failure(msg, index, _) = parsed
      Left(new ParseException(msg, index))
    }
  }

}
