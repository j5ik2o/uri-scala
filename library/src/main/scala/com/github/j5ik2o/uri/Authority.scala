package com.github.j5ik2o.uri

import java.text.ParseException

case class Authority(hostName: String, port: Option[Int], userInfo: Option[UserInfo]) {

  def withHostName(value: String): Authority = copy(hostName = value)

  def withPort(value: Option[Int]): Authority = copy(port = value)

  def withUserInfo(value: Option[UserInfo]): Authority = copy(userInfo = value)

  def asString: String = s"${userInfo.fold("")(v => s"${v.asString}@")}$hostName${port.fold("")(v => s":$v")}"
}

object Authority {

  def parseWithException(hostName: String, port: Option[Int], userInfo: Option[UserInfo]): Authority =
    parse(hostName, port, userInfo).fold(throw _, identity)

  def parse(hostName: String, port: Option[Int], userInfo: Option[UserInfo]): Either[ParseException, Authority] = {
    parse(new Authority(hostName, port, userInfo).asString)
  }

  def parseWithException(s: CharSequence): Authority = parse(s).fold(throw _, identity)

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
