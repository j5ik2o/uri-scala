package com.github.j5ik2o.uris

import java.text.ParseException

case class Uri(
    scheme: Scheme,
    authority: Authority,
    path: Path,
    query: Option[Query],
    fragment: Option[String]
) {

  def isAbsolute: Boolean = fragment.isEmpty

  def withSchema(value: Scheme): Uri = copy(scheme = value)

  def withAuthority(value: Authority): Uri = copy(authority = value)

  def withHostName(value: String): Uri = copy(authority = authority.withHostName(value))

  def withPort(value: Option[Int]): Uri = copy(authority = authority.withPort(value))

  def withUserInfo(value: Option[UserInfo]): Uri = copy(authority = authority.withUserInfo(value))

  def withPath(value: Path): Uri = copy(path = value)

  def withQuery(value: Option[Query]): Uri = copy(query = value)

  def withQueryString(params: (String, Option[String])*): Uri =
    withQuery(Some(Query(params.toVector)))

  def withFragment(value: Option[String]): Uri = copy(fragment = value)

  override def toString: String = {
    s"${scheme.value}://$authority$path${query.fold("")(v => s"?$v")}${fragment.fold("")(v => s"#$v")}"
  }

}

object Uri {

  def parseWithException(
      scheme: Scheme,
      authority: Authority,
      path: Path,
      query: Option[Query],
      fragment: Option[String]
  ): Uri = parse(scheme, authority, path, query, fragment).fold(throw _, identity)

  def parse(
      scheme: Scheme,
      authority: Authority,
      path: Path,
      query: Option[Query],
      fragment: Option[String]
  ): Either[ParseException, Uri] =
    parse(new Uri(scheme, authority, path, query, fragment).toString)

  def parseWithException(s: CharSequence): Uri = parse(s).fold(throw _, identity)

  def parse(s: CharSequence): Either[ParseException, Uri] = {
    import fastparse._
    val parsed = fastparse.parse(s.toString, UriParser.uri(_))
    if (parsed.isSuccess) {
      val Parsed.Success(result, _) = parsed
      Right(result)
    } else {
      val Parsed.Failure(msg, index, _) = parsed
      Left(new ParseException(msg, index))
    }
  }

}
