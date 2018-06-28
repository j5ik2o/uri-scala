package com.github.j5ik2o.uris

import fastparse.core.Parsed
import org.scalacheck.{Gen, Shrink}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.PropertyChecks

class BaseParserSpec extends FreeSpec with Matchers with PropertyChecks with BaseParser {

  implicit val noShrink: Shrink[String] = Shrink.shrinkAny
  val lowAlphaGen = Gen.choose('a', 'z')
  val highAlphaGen = Gen.choose('A', 'Z')
  val alphaCharGen = Gen.frequency((5, lowAlphaGen), (5, highAlphaGen))
  val digitGen = Gen.choose('0', '9')
  val hexdigit = Gen.frequency((5,digitGen), (5, Gen.choose('A' , 'F')))

  //      val stringGen: Gen[String] = Gen.listOf(alphaCharGen).map(_.mkString).suchThat(_.nonEmpty)

  "BaseParser" - {
    "ALPHA" in
      forAll(alphaCharGen) { value =>
        val Parsed.Success(text, _) = ALPHA.parse(value.toString)
        text shouldBe value.toString
      }
    "DIGIT" in forAll(digitGen){ value =>
      val Parsed.Success(text, _) = DIGIT.parse(value.toString)
      text shouldBe value.toString
    }
    "HEXDIG" in forAll(hexdigit) { value =>
      val Parsed.Success(text, _) = HEXDIG.parse(value.toString)
      text shouldBe value.toString
    }
  }
}

