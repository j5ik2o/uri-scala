package com.github.j5ik2o.uris

case class Uri(scheme: Scheme, authority: Authority, path: Path, query: Option[String], fragment: Option[String]) {
  def isAbsolute: Boolean = fragment.isEmpty

  def withSchema(value: Scheme): Uri = copy(scheme = value)

  def withUserInfo(value: Option[UserInfo]): Uri = copy(authority = authority.withUserInfo(value))

  def withPath(value: Path): Uri = copy(path = value)

  def withQuery(value: Option[String]): Uri = copy(query = value)

  def withFragment(value: Option[String]): Uri = copy(fragment = value)
}
