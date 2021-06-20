package com.github.j5ik2o.uri

import fastparse._
import org.scalacheck.{ Gen, Shrink }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class UriParserSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks {

  implicit val noShrink: Shrink[String] = Shrink.shrinkAny
  val lowAlphaGen: Gen[Char]            = Gen.choose('a', 'z')
  val highAlphaGen: Gen[Char]           = Gen.choose('A', 'Z')
  val alphaCharGen: Gen[Char]           = Gen.frequency((5, lowAlphaGen), (5, highAlphaGen))
  val digitGen: Gen[Char]               = Gen.choose('0', '9')
  val hexdigit: Gen[Char]               = Gen.frequency((5, digitGen), (5, Gen.choose('A', 'F')), (5, Gen.choose('a', 'f')))

  val unreservedGen: Gen[Char] =
    Gen.frequency((2, alphaCharGen), (3, digitGen), (5, Gen.oneOf(Seq('-', '.', '_', '~'))))
  val genDelimsGen: Gen[Char]        = Gen.oneOf(Seq(':', '/', '?', '#', '[', ']', '@'))
  val subDelimsGen: Gen[Char]        = Gen.oneOf(Seq('!', '$', '&', '"', '(', ')', '*', '+', ',', ';', '='))
  val subDelimsWithoutGen: Gen[Char] = Gen.oneOf(Seq('!', '$', '"', '(', ')', '*', '+', ',', ';'))
  val reservedGen: Gen[Char]         = Gen.frequency((5, genDelimsGen), (5, subDelimsGen))
  val pctEncodedGen: Gen[String]     = Gen.listOfN(2, hexdigit).map(v => "%" + v.mkString)

  val pcharGen: Gen[String] = Gen.frequency(
    (2, unreservedGen.map(_.toString)),
    (3, pctEncodedGen),
    (2, subDelimsGen.map(_.toString)),
    (3, Gen.oneOf(Seq(':', '@')).map(_.toString))
  )

  val pcharGen2: Gen[String] = Gen
    .frequency(
      (2, unreservedGen.map(_.toString)),
      (3, pctEncodedGen),
      (2, subDelimsWithoutGen.map(_.toString)),
      (3, Gen.oneOf(Seq(':', '@')).map(_.toString))
    )

  val queryGen: Gen[String] = Gen
    .listOf(pcharGen2.suchThat(_.nonEmpty).flatMap { v1 =>
      pcharGen2.map { v2 =>
        v1 + "=" + v2
      }
    })
    .suchThat(_.nonEmpty)
    .map(_.mkString("&"))
  //val queryGen     = Gen.frequency((9, pcharGen), (1, Gen.oneOf(Seq('/', '?')).map(_.toString)))
  val fragmentGen: Gen[String]  = Gen.frequency((9, pcharGen), (1, Gen.oneOf(Seq('/', '?')).map(_.toString)))
  val segmentGen: Gen[String]   = Gen.listOf(pcharGen).map(_.mkString)
  val segmentNzGen: Gen[String] = Gen.listOf(pcharGen).map(_.mkString).suchThat(_.nonEmpty)

  val segmentNzNcGen: Gen[String] = Gen
    .listOf(
      Gen.frequency(
        (2, unreservedGen.map(_.toString)),
        (3, pctEncodedGen),
        (2, subDelimsGen.map(_.toString)),
        (3, Gen.oneOf(Seq('@')).map(_.toString))
      )
    )
    .map(_.mkString)
    .suchThat(_.nonEmpty)

  val pathAbemptyGen: Gen[String] = Gen.listOf(segmentGen.map(v => "/" + v)).map(_.mkString).suchThat(_.nonEmpty)

  val pathAbsoluteGen: Gen[String] = Gen
    .listOf(segmentNzGen.map(v => "/" + v))
    .map(_.mkString)
    .flatMap { v =>
      pathAbemptyGen.map(v2 => v + v2)
    }
    .suchThat(_.nonEmpty)

  val pathNoschemeGen: Gen[String] = segmentNzNcGen
    .flatMap { v =>
      Gen.listOf(segmentGen.map(v2 => "/" + v2)).map(_.mkString).map(v2 => v + v2)
    }
    .suchThat(_.nonEmpty)

  val pathRootlessGen: Gen[String] = segmentNzGen
    .flatMap { v =>
      Gen.listOf(segmentGen.map(v2 => "/" + v2)).map(_.mkString).map(v2 => v + v2)
    }
    .suchThat(_.nonEmpty)

  val pathGen: Gen[String] = Gen
    .frequency((2, pathAbemptyGen), (2, pathAbsoluteGen), (2, pathNoschemeGen), (2, pathRootlessGen))
    .suchThat(_.nonEmpty)

  val regNameGen: Gen[String] = Gen
    .listOf(
      Gen
        .frequency((2, unreservedGen.map(_.toString)), (2, subDelimsGen.map(_.toString)), (2, pctEncodedGen))
    )
    .map(_.mkString)
    .suchThat(_.nonEmpty)

  val decOctetGen: Gen[String] = Gen.choose(1, 255).map(_.toString).suchThat(_.nonEmpty)

  val ipv4AddressGen: Gen[String] = for {
    v1 <- decOctetGen
    v2 <- decOctetGen
    v3 <- decOctetGen
    v4 <- decOctetGen
  } yield s"$v1.$v2.$v3.$v4"

  val h16Gen: Gen[String] = Gen
    .choose(1, 4)
    .flatMap { n =>
      Gen.listOfN(n, hexdigit).map(_.mkString)
    }
    .suchThat(_.nonEmpty)

  val ls32Gen: Gen[String] = Gen.frequency((5, ipv4AddressGen), (5, h16Gen.flatMap(v => h16Gen.map(s => v + ":" + s))))

  //  6( h16 ":" ) ls32
  val ipv6AddressGen1: Gen[String] = Gen.listOfN(6, h16Gen).flatMap { s =>
    ls32Gen.map(v => s.mkString(":") + ":" + v)
  }

  // "::" 5( h16 ":" ) ls32
  val ipv6AddressGen2: Gen[String] = Gen.listOfN(5, h16Gen).flatMap { s =>
    ls32Gen.map(v => "::" + s.mkString(":") + ":" + v)
  }

  val ipv6AddressGen3: Gen[String] = Gen.listOfN(4, h16Gen).flatMap { s =>
    ls32Gen.map(v => "::" + s.mkString(":") + ":" + v)
  }

  "UriParser" - {
    "unreserved" in forAll(unreservedGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value.toString, UriParser.unreserved(_))
      result shouldBe value.toString
    }
    "genDelims" in forAll(genDelimsGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value.toString, UriParser.genDelims(_))
      result shouldBe value.toString
    }
    "subDelims" in forAll(subDelimsGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value.toString, UriParser.subDelims(_))
      result shouldBe value.toString
    }
    "reserved" in forAll(reservedGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value.toString, UriParser.reserved(_))
      result shouldBe value.toString
    }
    "pctEncoded" in forAll(pctEncodedGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.pctEncoded(_))
      result shouldBe value
    }
    "pchar" in forAll(pcharGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.pchar(_))
      result shouldBe value
    }
    "query" in forAll(queryGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.query(_))
      result.params.nonEmpty shouldBe true
    }
    "fragment" in forAll(fragmentGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.fragment(_))
      result shouldBe value
    }
    "segment" in forAll(segmentGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.segment(_))
      result shouldBe value
    }
    "segmentNz" in forAll(segmentNzGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.segmentNz(_))
      result shouldBe value
    }
    "segmentNzNc" in forAll(segmentNzNcGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.segmentNzNc(_))
      result shouldBe value
    }
    "pathAbempty" in forAll(pathAbemptyGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.pathAbempty(_))
      result.asString shouldBe value
    }
    "pathAbsolute" in forAll(pathAbsoluteGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.pathAbsolute(_))
      result.asString shouldBe value
    }
    "pathNoscheme" in forAll(pathNoschemeGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.pathNoScheme(_))
      result.asString shouldBe value
    }
    "pathRootless" in forAll(pathRootlessGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.pathRootless(_))
      result.asString shouldBe value
    }
    "pathEmpty" in {
      val value                     = ""
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.pathEmpty(_))
      result.asString shouldBe value
    }
    "path" in forAll(pathGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.path(_))
      result.asString shouldBe value
    }
    "regName" in forAll(regNameGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.regName(_))
      result shouldBe value
    }
    "decOctet" in forAll(decOctetGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.decOctet(_))
      result shouldBe value
    }
    "ipv4Address" in forAll(ipv4AddressGen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.ipv4Address(_))
      result shouldBe value
    }
    "h16" in forAll(h16Gen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.h16(_))
      result shouldBe value
    }
    "ls32" in forAll(ls32Gen) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.ls32(_))
      result shouldBe value
    }
    "ipv6Address1" in forAll(ipv6AddressGen1) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.ipv6Address(_))
      result shouldBe value
    }
    "ipv6Address2" in forAll(ipv6AddressGen2) { value =>
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.ipv6Address(_))
      result shouldBe value
    }
    "URI" in {
      val value                     = "http://yahoo.co.jp/abc?key1=abc"
      val Parsed.Success(result, _) = fastparse.parse(value, UriParser.uri(_))
      result shouldBe Uri(
        Scheme.parseWithException("http"),
        Authority.parseWithException("yahoo.co.jp", None, None),
        Path.parseWithException("/abc"),
        Some(Query.parseWithException(Vector("key1" -> Some("abc")))),
        None
      )
      result.asString shouldBe value
    }
  }

}
