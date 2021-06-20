package com.github.j5ik2o.uri

import java.text.ParseException

sealed trait Path {

  def parts: Vector[String]

  def isEmpty: Boolean

  def nonEmpty: Boolean = !isEmpty

  def withParts(parts: IterableOnce[String]): Path

  def toRootless: RootlessPath

  def toAbsolute: AbsolutePath

  def addPart(part: String): Path =
    withParts(parts :+ part)

  def addParts(otherParts: String*): Path =
    addParts(otherParts)

  def addParts(otherParts: IterableOnce[String]): Path =
    withParts(parts = parts ++ otherParts)

  def asString: String

}

object Path {

  def empty: Path = EmptyPath

  val slash: Path = AbsolutePath(Vector.empty)

  def fromParts(parts: IterableOnce[String]): Path = {
    parts match {
      case p if p.iterator.isEmpty => EmptyPath
      case p                       => AbsolutePath(p.iterator.toVector)
    }
  }

  def parseWithException(value: String): Path =
    parse(value).fold(throw _, identity)

  def parse(s: CharSequence): Either[ParseException, Path] = {
    import fastparse._
    val parsed = fastparse.parse(s.toString, UriParser.path(_))
    if (parsed.isSuccess) {
      val Parsed.Success(result, _) = parsed
      Right(result)
    } else {
      val Parsed.Failure(msg, index, _) = parsed
      Left(new ParseException(msg, index))
    }
  }

}

final case class AbemptyPath(parts: Vector[String]) extends Path {

  override def withParts(otherParts: IterableOnce[String]): Path =
    AbemptyPath(otherParts.iterator.to(Vector))

  override def toAbsolute: AbsolutePath = AbsolutePath(parts)

  override def isEmpty: Boolean = false

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def asString: String = parts.mkString("/", "/", "")

}

final case class AbsolutePath(parts: Vector[String]) extends Path {

  override def toAbsolute: AbsolutePath = this

  override def withParts(otherParts: IterableOnce[String]): Path =
    copy(parts = otherParts.iterator.to(Vector))

  override def isEmpty: Boolean = false

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def asString: String = parts.mkString("/", "/", "")

}

final case class NoSchemePath(parts: Vector[String]) extends Path {

  override def withParts(otherParts: IterableOnce[String]): Path =
    NoSchemePath(otherParts.iterator.to(Vector))

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toAbsolute: AbsolutePath = AbsolutePath(parts)

  override def isEmpty: Boolean = parts.isEmpty

  override def asString: String = parts.mkString("", "/", "")

}

final case class RootlessPath(parts: Vector[String]) extends Path {

  override def toRootless: RootlessPath = this

  override def toAbsolute: AbsolutePath = AbsolutePath(parts)

  override def withParts(otherParts: IterableOnce[String]): Path =
    RootlessPath(otherParts.iterator.to(Vector))

  override def isEmpty: Boolean = parts.isEmpty

  override def asString: String = parts.mkString("", "/", "")

}

case object EmptyPath extends Path {

  override def isEmpty: Boolean = true

  override def toAbsolute: AbsolutePath = AbsolutePath(Vector.empty)

  override def withParts(parts: IterableOnce[String]): Path =
    Path.fromParts(parts.iterator.to(Vector))

  override def parts: Vector[String] = Vector.empty

  def unapply(path: Path): Boolean = path.isEmpty

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def asString: String = ""

}
