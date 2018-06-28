package com.github.j5ik2o.uris

import fastparse.core.Parsed

import scala.collection.GenTraversableOnce

sealed trait Path {

  def parts: Vector[String]

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def withParts(parts: GenTraversableOnce[String]): Path

  def toRootless: RootlessPath

  def toAbsolute: AbsolutePath

  def addPart(part: String): Path =
    withParts(parts :+ part)

  def addParts(otherParts: String*): Path =
    addParts(otherParts)

  def addParts(otherParts: GenTraversableOnce[String]): Path =
    withParts(parts = parts ++ otherParts)

}

object Path {

  def empty: Path = EmptyPath

  val slash: Path = AbsolutePath(Vector.empty)

  def apply(parts: GenTraversableOnce[String]): Path = {
    parts match {
      case p if p.isEmpty => EmptyPath
      case p              => AbsolutePath(p.toVector)
    }
  }

  def parse(s: CharSequence): Path = {
    val Parsed.Success(result, _) = UriParser.path.parse(s.toString)
    result
  }

}

final case class AbemptyPath(parts: Vector[String]) extends Path {

  override def withParts(otherParts: GenTraversableOnce[String]): Path =
    AbemptyPath(otherParts.toVector)

  override def toAbsolute: AbsolutePath = AbsolutePath(parts)

  override def isEmpty: Boolean = false

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toString: String = parts.mkString("/", "/", "")

}

final case class AbsolutePath(parts: Vector[String]) extends Path {

  def toAbsolute: AbsolutePath = this

  def withParts(otherParts: GenTraversableOnce[String]): Path =
    copy(parts = otherParts.toVector)

  def isEmpty: Boolean = false

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toString: String = parts.mkString("/", "/", "")

}

final case class NoSchemePath(parts: Vector[String]) extends Path {

  override def withParts(otherParts: GenTraversableOnce[String]): Path =
    NoSchemePath(otherParts.toVector)

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toAbsolute: AbsolutePath = AbsolutePath(parts)

  override def isEmpty: Boolean = parts.isEmpty

  override def toString: String = parts.mkString("", "/", "")

}

final case class RootlessPath(parts: Vector[String]) extends Path {

  def toRootless: RootlessPath = this

  def toAbsolute: AbsolutePath = AbsolutePath(parts)

  def withParts(otherParts: GenTraversableOnce[String]): Path =
    RootlessPath(otherParts.toVector)

  def isEmpty: Boolean = parts.isEmpty

  override def toString: String = parts.mkString("", "/", "")

}

case object EmptyPath extends Path {

  def isEmpty: Boolean = true

  def toAbsolute: AbsolutePath = AbsolutePath(Vector.empty)

  def withParts(parts: GenTraversableOnce[String]): Path =
    Path(parts.toVector)

  def parts: Vector[String] = Vector.empty

  def unapply(path: Path): Boolean = path.isEmpty

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toString: String = ""

}
