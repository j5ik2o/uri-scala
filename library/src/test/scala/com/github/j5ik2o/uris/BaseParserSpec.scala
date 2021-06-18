package com.github.j5ik2o.uris

import fastparse._
import org.scalacheck.{ Gen, Shrink }
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BaseParserSpec extends AnyFreeSpec with Matchers with ScalaCheckPropertyChecks with BaseParser {

  implicit val noShrink: Shrink[String] = Shrink.shrinkAny
  val lowAlphaGen: Gen[Char]            = Gen.choose('a', 'z')
  val highAlphaGen: Gen[Char]           = Gen.choose('A', 'Z')
  val alphaCharGen: Gen[Char]           = Gen.frequency((5, lowAlphaGen), (5, highAlphaGen))
  val digitGen: Gen[Char]               = Gen.choose('0', '9')
  val hexdigit: Gen[Char]               = Gen.frequency((5, digitGen), (5, Gen.choose('A', 'F')))

  //      val stringGen: Gen[String] = Gen.listOf(alphaCharGen).map(_.mkString).suchThat(_.nonEmpty)

  "BaseParser" - {
    "ALPHA" in
    forAll(alphaCharGen) { value =>
      val Parsed.Success(text, _) = fastparse.parse(value.toString, ALPHA(_))
      text shouldBe value.toString
    }
    "DIGIT" in forAll(digitGen) { value =>
      val Parsed.Success(text, _) = fastparse.parse(value.toString, DIGIT(_))
      text shouldBe value.toString
    }
    "HEXDIG" in forAll(hexdigit) { value =>
      val Parsed.Success(text, _) = fastparse.parse(value.toString, HEXDIGIT(_))
      text shouldBe value.toString
    }
  }
}
