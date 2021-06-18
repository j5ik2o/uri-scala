package com.github.j5ik2o.uris

import fastparse.NoWhitespace._
import fastparse._

object UriParser extends BaseParser {

//  def parse[_: P](text: String): Parsed[Uri] = {
//    fastparse.parse(text, URI(_))
//  }

  def absoluteURI[_: P]: P[Uri] = P(scheme ~ ":" ~ hierPart ~ ("?" ~ query).? ~ End).map {
    case (scheme, (authority, path), query) =>
      Uri(scheme, authority, path, query, None)
  }

  def URI[_: P]: P[Uri] = P(scheme ~ ":" ~ hierPart ~ ("?" ~ query).? ~ ("#" ~ fragment).? ~ End).map {
    case (scheme, (authority, path), query, fragment) =>
      Uri(scheme, authority, path, query, fragment)
  }

  // def uRIReference = P(URI | relativeRef)

  def hierPart[_: P]: P[(Authority, Path)] = P(
    "//" ~ authority ~ (pathAbempty | pathAbsolute | pathRootless | pathEmpty)
  )

  def relativeRef[_: P]: P[(Authority, Path, Option[Query], String)] =
    P(relativePart ~ ("?" ~ query).? ~ ("#" ~ fragment))

  def relativePart[_: P]: P[(Authority, Path)] = P(
    "//" ~ authority ~ (pathAbempty | pathAbsolute | pathNoScheme | pathEmpty)
  )

  def scheme[_: P]: P[Scheme] = P(ALPHA ~ (ALPHA | DIGIT | "+" | "-" | ".").rep).!.map(Scheme)

  def authority[_: P]: P[Authority] = P((userInfo ~ "@").? ~ host ~ (":" ~ port).?).map {
    case (userInfoOpt, hostName, portOpt) =>
      Authority(hostName, portOpt, userInfoOpt)
  }

  def userInfo[_: P]: P[UserInfo] =
    P(
      (unreserved | pctEncoded | subDelims).rep ~ (":" ~ (unreserved | pctEncoded | subDelims).rep).?
    ).map { case (v1, v2) =>
      UserInfo(v1.mkString, v2.map(_.mkString))
    }
  def host[_: P]: P[String] = P(ipLiteral | ipv4Address | regName)
  def port[_: P]: P[Int]    = P(DIGIT.rep).!.map(_.toInt)

  def ipLiteral[_: P]: P[String] = P("[" ~ (ipv6Address | ipv4Address) ~ "]").!
  def ipvFuture[_: P]: P[String] = P("v" ~ HEXDIGIT.rep(1) ~ "." ~ (unreserved | subDelims | ":".!).rep(1)).!

  //  6( h16 ":" ) ls32
  def ipv6Address1[_: P]: P[String] = P((h16 ~ ":").rep(6, max = 6) ~ ls32).!
  // "::" 5( h16 ":" ) ls32
  def ipv6Address2[_: P]: P[String] = P("::" ~ (h16 ~ ":").rep(5, max = 5) ~ ls32).!
  // [               h16 ] "::" 4( h16 ":" ) ls32
  def ipv6Address3[_: P]: P[String] = P(h16 ~ "::" ~ (h16 ~ ":").rep(4, max = 4) ~ ls32).!

  // [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
  def ipv6Address4[_: P]: P[String] =
    P(((h16 ~ ":").rep(max = 1) ~ h16).? ~ "::" ~ (h16 ~ ":").rep(3, max = 3) ~ ls32).!

  //  [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
  def ipv6Address5[_: P]: P[String] =
    P(
      ((h16 ~ ":").rep(max = 2) ~ h16).? ~ "::" ~ (h16 ~ ":").rep(2, max = 2) ~ ls32
    ).!

  //  [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
  def ipv6Address6[_: P]: P[String] =
    P(
      ((h16 ~ ":").rep(max = 3) ~ h16).? ~ "::" ~ (h16 ~ ":").rep(1, max = 1) ~ ls32
    ).!
  // [ *4( h16 ":" ) h16 ] "::"              ls32
  def ipv6Address7[_: P]: P[String] = P(((h16 ~ ":").rep(max = 4) ~ h16).? ~ "::" ~ ls32).!
  //  [ *5( h16 ":" ) h16 ] "::"              h16
  def ipv6Address8[_: P]: P[String] = P(((h16 ~ ":").rep(max = 5) ~ h16).? ~ "::" ~ h16).!
  //  [ *6( h16 ":" ) h16 ] "::"
  def ipv6Address9[_: P]: P[String] = P(((h16 ~ ":").rep(max = 6) ~ h16).? ~ "::").!

  def ipv6Address[_: P] = P(
    ipv6Address1 | ipv6Address2 | ipv6Address3 | ipv6Address4 | ipv6Address4 | ipv6Address5 | ipv6Address6 | ipv6Address7 | ipv6Address8 | ipv6Address9
  )

  def h16[_: P]: P[String]  = P(HEXDIGIT.rep(1, max = 4)).map(_.mkString)
  def ls32[_: P]: P[String] = P((h16 ~ ":" ~ h16).! | ipv4Address)

  def ipv4Address[_: P]: P[String] =
    P(decOctet ~ "." ~ decOctet ~ "." ~ decOctet ~ "." ~ decOctet).!

  private def decOctet1[_: P]: P[String] = DIGIT
  private def decOctet2[_: P]: P[String] = P(CharIn("1-9").! ~ DIGIT).!
  private def decOctet3[_: P]: P[String] = P("1".! ~ DIGIT ~ DIGIT).!
  private def decOctet4[_: P]: P[String] = P("2".! ~ CharIn("0-4").! ~ DIGIT).!
  private def decOctet5[_: P]: P[String] = P("25".! ~ CharIn("0-5").!).!

  def decOctet[_: P]: P[String] = P(decOctet5 | decOctet4 | decOctet3 | decOctet2 | decOctet1)

  def regName[_: P]: P[String] = P((unreserved | pctEncoded | subDelims).rep).!

  def path[_: P]: P[Path] = P(pathRootless | pathNoScheme | pathAbempty | pathAbsolute | pathEmpty)

  def pathAbempty[_: P]: P[AbemptyPath] = P(("/" ~ segment).rep).map { v =>
    AbemptyPath(v.toVector)
  }

  def pathAbsolute[_: P]: P[AbsolutePath] = P("/" ~ (segmentNz ~ ("/" ~ segment).rep).?).map { v =>
    v.map { case (h, t) =>
      AbsolutePath(h +: t.toVector)
    }
      .getOrElse(AbsolutePath(Vector.empty))
  }

  def pathNoScheme[_: P]: P[NoSchemePath] = P(segmentNzNc ~ ("/" ~ segment).rep).map { case (v1, v2) =>
    NoSchemePath(v1 +: v2.toVector)
  }

  def pathRootless[_: P]: P[RootlessPath] = P(segmentNz ~ ("/" ~ segment).rep).map { case (v1, v2) =>
    RootlessPath(v1 +: v2.toVector)
  }
  def pathEmpty[_: P]: P[Path] = P(pchar.?).map(_ => EmptyPath)

  def segment[_: P]: P[String]     = P(pchar.rep).map(_.mkString)
  def segmentNz[_: P]: P[String]   = P(pchar.rep(1)).map(_.mkString)
  def segmentNzNc[_: P]: P[String] = P((unreserved | pctEncoded | subDelims | "@".!).rep(1)).map(_.mkString)

  def pchar[_: P]: P[String]  = P(unreserved | pctEncoded | subDelims | ":".! | "@".!)
  def pchar2[_: P]: P[String] = P(unreserved | pctEncoded | subDelimsWithout | ":".! | "@".!)

  def query[_: P]: P[Query] =
    P(((pchar2 | "/" | "?").rep.! ~ ("=" ~ (pchar2 | "/" | "?").rep.!).?).rep(sep = "&")).map { v =>
      Query(v.toVector)
    }

  def fragment[_: P]: P[String] = P((pchar | "/" | "?").rep).!

  def pctEncoded[_: P]: P[String] = P("%" ~ HEXDIGIT ~ HEXDIGIT).!

  def unreserved[_: P]: P[String]       = P(ALPHA | DIGIT | "-".! | ".".! | "_".! | "~".!)
  def reserved[_: P]: P[String]         = P(genDelims | subDelims)
  def genDelims[_: P]: P[String]        = P((":" | "/" | "?" | "#" | "[" | "]" | "@").!)
  def subDelims[_: P]: P[String]        = P(("!" | "$" | "&" | "\"" | "(" | ")" | "*" | "+" | "," | ";" | "=").!)
  def subDelimsWithout[_: P]: P[String] = P(("!" | "$" | "\"" | "(" | ")" | "*" | "+" | "," | ";").!)
}
