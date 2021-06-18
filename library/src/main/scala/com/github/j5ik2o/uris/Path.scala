package com.github.j5ik2o.uris

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

}

object Path {

  def empty: Path = EmptyPath

  val slash: Path = AbsolutePath(Vector.empty)

  def apply(parts: IterableOnce[String]): Path = {
    parts match {
      case p if p.iterator.isEmpty => EmptyPath
      case p                       => AbsolutePath(p.iterator.toVector)
    }
  }

  def parse(s: CharSequence): Path = {
    import fastparse._
    val Parsed.Success(result, _) = fastparse.parse(s.toString, UriParser.path(_))
    result
  }

}

final case class AbemptyPath(parts: Vector[String]) extends Path {

  override def withParts(otherParts: IterableOnce[String]): Path =
    AbemptyPath(otherParts.iterator.to(Vector))

  override def toAbsolute: AbsolutePath = AbsolutePath(parts)

  override def isEmpty: Boolean = false

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toString: String = parts.mkString("/", "/", "")

}

final case class AbsolutePath(parts: Vector[String]) extends Path {

  def toAbsolute: AbsolutePath = this

  def withParts(otherParts: IterableOnce[String]): Path =
    copy(parts = otherParts.iterator.to(Vector))

  def isEmpty: Boolean = false

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toString: String = parts.mkString("/", "/", "")

}

final case class NoSchemePath(parts: Vector[String]) extends Path {

  override def withParts(otherParts: IterableOnce[String]): Path =
    NoSchemePath(otherParts.iterator.to(Vector))

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toAbsolute: AbsolutePath = AbsolutePath(parts)

  override def isEmpty: Boolean = parts.isEmpty

  override def toString: String = parts.mkString("", "/", "")

}

final case class RootlessPath(parts: Vector[String]) extends Path {

  def toRootless: RootlessPath = this

  def toAbsolute: AbsolutePath = AbsolutePath(parts)

  def withParts(otherParts: IterableOnce[String]): Path =
    RootlessPath(otherParts.iterator.to(Vector))

  def isEmpty: Boolean = parts.isEmpty

  override def toString: String = parts.mkString("", "/", "")

}

case object EmptyPath extends Path {

  def isEmpty: Boolean = true

  def toAbsolute: AbsolutePath = AbsolutePath(Vector.empty)

  def withParts(parts: IterableOnce[String]): Path =
    Path(parts.iterator.to(Vector))

  def parts: Vector[String] = Vector.empty

  def unapply(path: Path): Boolean = path.isEmpty

  override def toRootless: RootlessPath = RootlessPath(parts)

  override def toString: String = ""

}
