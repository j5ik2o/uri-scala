package com.github.j5ik2o.uris

case class Authority(hostName: String, port: Option[Int], userInfo: Option[UserInfo]) {

  def withHostName(value: String): Authority = copy(hostName = value)

  def withPort(value: Option[Int]): Authority = copy(port = value)

  def withUserInfo(value: Option[UserInfo]): Authority = copy(userInfo = value)

  override def toString = s"${userInfo.fold("")(v => s"$v@")}$hostName${port.fold("")(v => s":$v")}"
}
