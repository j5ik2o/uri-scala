package com.github.j5ik2o.uris

case class UserInfo(user: String, password: Option[String] = None) {
  override def toString = s"$user${password.fold("")(v => s":$v")}"
}
