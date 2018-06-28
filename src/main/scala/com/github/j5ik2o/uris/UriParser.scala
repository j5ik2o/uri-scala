package com.github.j5ik2o.uris

import java.net.Inet4Address

import fastparse.WhitespaceApi
import fastparse.all._

object UriParser extends BaseParser {
  val White = WhitespaceApi.Wrapper {
    import fastparse.all._
    NoTrace(" ".rep)
  }
  import White._

  def parse(text: String) = {
    URI.parse(text)
  }

  val absoluteURI = P(scheme ~ ":" ~ hierPart ~ ("?" ~ query).? ~ End).map {
    case (scheme, (authority, path), query) =>
      Uri(scheme, authority, path, query, None)
  }

  val URI: P[Uri] = P(scheme ~ ":" ~ hierPart ~ ("?" ~ query).? ~ ("#" ~ fragment).? ~ End).map {
    case (scheme, (authority, path), query, fragment) =>
      Uri(scheme, authority, path, query, fragment)
  }

  // val uRIReference = P(URI | relativeRef)

  val hierPart: P[(Authority, Path)] = P("//" ~ authority ~ (pathAbempty | pathAbsolute | pathRootless | pathEmpty))

  val relativeRef = P(relativePart ~ ("?" ~ query).? ~ ("#" ~ fragment))
  val relativePart: P[(Authority, Path)] = P(
    "//" ~ authority ~ (pathAbempty | pathAbsolute | pathNoScheme | pathEmpty)
  )

  val scheme: P[Scheme] = P(ALPHA ~ (ALPHA | DIGIT | "+" | "-" | ".").rep).!.map(Scheme)

  val authority: P[Authority] = P((userInfo ~ "@").? ~ host ~ (":" ~ port).?).map {
    case (userInfoOpt, hostName, portOpt) =>
      Authority(hostName, portOpt, userInfoOpt)
  }
  val userInfo: P[UserInfo] = P(
    (unreserved | pctEncoded | subDelims).rep ~ (":" ~ (unreserved | pctEncoded | subDelims).rep).?
  ).map {
    case (v1, v2) =>
      UserInfo(v1.mkString, v2.map(_.mkString))
  }
  val host: P[String] = P(ipLiteral | ipv4Address | regName)
  val port: P[Int]    = P(DIGIT.rep).!.map(_.toInt)

  val ipLiteral: P[String] = P("[" ~ (ipv6Address | ipv4Address) ~ "]").!
  val ipvFuture: P[String] = P("v" ~ HEXDIG.rep(1) ~ "." ~ (unreserved | subDelims | ":".!).rep(1)).!

  //  6( h16 ":" ) ls32
  val ipv6Address1: P[String] = P((h16 ~ ":").rep(6, max = 6) ~ ls32).!
  // "::" 5( h16 ":" ) ls32
  val ipv6Address2: P[String] = P("::" ~ (h16 ~ ":").rep(5, max = 5) ~ ls32).!
  // [               h16 ] "::" 4( h16 ":" ) ls32
  val ipv6Address3: P[String] = P(h16 ~ "::" ~ (h16 ~ ":").rep(4, max = 4) ~ ls32).!
  // [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
  val ipv6Address4: P[String] =
    P(((h16 ~ ":").rep(max = 1) ~ h16).? ~ "::" ~ (h16 ~ ":").rep(3, max = 3) ~ ls32).!
  //  [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
  val ipv6Address5: P[String] = P(((h16 ~ ":").rep(max = 2) ~ h16).? ~ "::" ~ (h16 ~ ":").rep(2, max = 2) ~ ls32).!
  //  [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
  val ipv6Address6: P[String] = P(((h16 ~ ":").rep(max = 3) ~ h16).? ~ "::" ~ (h16 ~ ":").rep(1, max = 1) ~ ls32).!
  // [ *4( h16 ":" ) h16 ] "::"              ls32
  val ipv6Address7: P[String] = P(((h16 ~ ":").rep(max = 4) ~ h16).? ~ "::" ~ ls32).!
  //  [ *5( h16 ":" ) h16 ] "::"              h16
  val ipv6Address8: P[String] = P(((h16 ~ ":").rep(max = 5) ~ h16).? ~ "::" ~ h16).!
  //  [ *6( h16 ":" ) h16 ] "::"
  val ipv6Address9: P[String] = P(((h16 ~ ":").rep(max = 6) ~ h16).? ~ "::").!
  val ipv6Address = P(
    ipv6Address1 | ipv6Address2 | ipv6Address3 | ipv6Address4 | ipv6Address4 | ipv6Address5 | ipv6Address6 | ipv6Address7 | ipv6Address8 | ipv6Address9
  )

  val h16: P[String]  = P(HEXDIG.rep(1, max = 4)).map(_.mkString)
  val ls32: P[String] = P((h16 ~ ":" ~ h16).! | ipv4Address)

  val ipv4Address: P[String] =
    P(decOctet ~ "." ~ decOctet ~ "." ~ decOctet ~ "." ~ decOctet).!

  private val decOctet1: P[String] = DIGIT
  private val decOctet2: P[String] = P(CharIn(0x31.toChar to 0x39.toChar).! ~ DIGIT).!
  private val decOctet3: P[String] = P("1".! ~ DIGIT ~ DIGIT).!
  private val decOctet4: P[String] = P("2".! ~ CharIn(0x30.toChar to 0x34.toChar).! ~ DIGIT).!
  private val decOctet5: P[String] = P("25".! ~ CharIn(0x30.toChar to 0x35.toChar).!).!

  val decOctet = P(decOctet5 | decOctet4 | decOctet3 | decOctet2 | decOctet1)

  val regName: P[String] = P((unreserved | pctEncoded | subDelims).rep).!

  val path: P[Path] = P(pathRootless | pathNoScheme | pathAbempty | pathAbsolute | pathEmpty)

  val pathAbempty: P[AbemptyPath] = P(("/" ~ segment).rep).map { v =>
    AbemptyPath(v.toVector)
  }

  val pathAbsolute: P[AbsolutePath] = P("/" ~ (segmentNz ~ ("/" ~ segment).rep).?).map { v =>
    v.map {
        case (h, t) =>
          AbsolutePath(h +: t.toVector)
      }
      .getOrElse(AbsolutePath(Vector.empty))
  }

  val pathNoScheme: P[NoSchemePath] = P(segmentNzNc ~ ("/" ~ segment).rep).map {
    case (v1, v2) =>
      NoSchemePath(v1 +: v2.toVector)
  }
  val pathRootless: P[RootlessPath] = P(segmentNz ~ ("/" ~ segment).rep).map {
    case (v1, v2) =>
      RootlessPath(v1 +: v2.toVector)
  }
  val pathEmpty: P[Path] = P(pchar.?).map(_ => EmptyPath)

  val segment: P[String]     = P(pchar.rep).map(_.mkString)
  val segmentNz: P[String]   = P(pchar.rep(1)).map(_.mkString)
  val segmentNzNc: P[String] = P((unreserved | pctEncoded | subDelims | "@".!).rep(1)).map(_.mkString)

  val pchar: P[String]  = P(unreserved | pctEncoded | subDelims | ":".! | "@".!)
  val pchar2: P[String] = P(unreserved | pctEncoded | subDelimsWithout | ":".! | "@".!)

  val query: P[Query] =
    P(((pchar2 | "/" | "?").rep.! ~ ("=" ~ (pchar2 | "/" | "?").rep.!).?).rep(sep = "&")).map { v =>
      Query(v.toVector)
    }

  val fragment: P[String] = P((pchar | "/" | "?").rep).!

  val pctEncoded: P[String] = P("%" ~ HEXDIG ~ HEXDIG).!

  val unreserved: P[String]       = P(ALPHA | DIGIT | "-".! | ".".! | "_".! | "~".!)
  val reserved: P[String]         = P(genDelims | subDelims)
  val genDelims: P[String]        = P((":" | "/" | "?" | "#" | "[" | "]" | "@").!)
  val subDelims: P[String]        = P(("!" | "$" | "&" | "\"" | "(" | ")" | "*" | "+" | "," | ";" | "=").!)
  val subDelimsWithout: P[String] = P(("!" | "$" | "\"" | "(" | ")" | "*" | "+" | "," | ";").!)
}
