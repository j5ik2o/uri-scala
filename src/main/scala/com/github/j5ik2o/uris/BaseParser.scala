package com.github.j5ik2o.uris

import fastparse.all._

trait BaseParser {

  val ALPHA: P[String]    = P(CharIn(0x41.toChar to 0x5A.toChar).! | CharIn(0x61.toChar to 0x7A.toChar).!)
  val alphaStr: P[String] = P(ALPHA.rep.!)

  val DIGIT: P[String]  = P(CharIn('0' to '9').!)
  val HEXDIG: P[String] = P(DIGIT | CharIn('A' to 'F').! | CharIn('a' to 'f').!)

  val DQUOTE: P[String] = P(CharIn(Seq(0x22.toChar)).!)
  val SP: P[String]     = P(CharIn(Seq(0x20.toChar)).!)
}
