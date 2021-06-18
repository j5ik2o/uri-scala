package com.github.j5ik2o.uris

import fastparse._, NoWhitespace._

trait BaseParser {

  def ALPHA[_: P]: P[String]    = P(CharIn("A-Z").! | CharIn("a-z").!)
  def alphaStr[_: P]: P[String] = P(ALPHA.rep.!)

  def DIGIT[_: P]: P[String]    = P(CharIn("0-9").!)
  def HEXDIGIT[_: P]: P[String] = P(DIGIT | CharIn("A-F").! | CharIn("a-f").!)

  def DQUOTET[_: P]: P[String] = P(CharIn("\"").!)
  def SPT[_: P]: P[String]     = P(CharIn(" ").!)
}
