package com.github.j5ik2o.uris

case class Authority(hostName: String, port: Option[Int], userInfo: Option[UserInfo]) {

  def withHostName(value: String): Authority = copy(hostName = value)

  def withPort(value: Option[Int]): Authority = copy(port = value)

  def withUserInfo(value: Option[UserInfo]): Authority = copy(userInfo = value)
}

/**
  * foo://example.com:8042/over/there?name=ferret#nose
  * \_/   \______________/\_________/ \_________/ \__/
  * |           |            |            |        |
  * scheme     authority       path        query   fragment
  * |   _____________________|__
  * / \ /                        \
  * urn:example:animal:ferret:nose
  **/
