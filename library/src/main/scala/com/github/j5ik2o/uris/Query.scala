package com.github.j5ik2o.uris

case class Query(params: Vector[(String, Option[String])]) {

  lazy val paramMap: Map[String, Vector[String]] = params.foldLeft(Map.empty[String, Vector[String]]) {
    case (m, (k, Some(v))) =>
      val values = m.getOrElse(k, Vector.empty)
      m + (k -> (values :+ v))
    case (m, (k, None)) =>
      val values = m.getOrElse(k, Vector.empty)
      m + (k -> values)
  }

  def withParam(k: String, v: String): Query                    = withParam(k, Some(v))
  def withParam(k: String, v: Option[String]): Query            = Query(params :+ (k -> v))
  def withParam(k: String): Query                               = withParam(k, None)
  def withParam(kv: (String, String)): Query                    = Query(params :+ (kv._1 -> Some(kv._2)))
  def withParamOptionValue(kv: (String, Option[String])): Query = Query(params :+ kv)

  def withParams(other: Query): Query           = Query(params ++ other.params)
  def withParams(kvs: (String, String)*): Query = withParams(kvs)

  def withParams(kvs: Iterable[(String, String)]): Query =
    withParamsOptionValues(kvs.map { case (k, v) => (k, Some(v)) })

  def withParamsOptionValues(kvs: Iterable[(String, Option[String])]): Query = Query(params ++ kvs)
  def withParamsOptionValues(kvs: (String, Option[String])*): Query          = withParamsOptionValues(kvs)

  def getParams(key: String): Vector[Option[String]] = params.collect {
    case (k, v) if k == key => v
  }

  def getParam(key: String): Option[String] = params.collectFirst {
    case (k, Some(v)) if k == key => v
  }

  def map(f: PartialFunction[(String, Option[String]), (String, Option[String])]): Query = {
    Query(params.map { kv =>
      if (f.isDefinedAt(kv)) f(kv) else kv
    })
  }

  def collect(f: PartialFunction[(String, Option[String]), (String, Option[String])]): Query =
    Query(params.collect(f))

  def flatMap(f: ((String, Option[String])) => IterableOnce[(String, Option[String])]): Query =
    Query(params.flatMap(f))

  def mapNames(f: String => String): Query =
    Query(params.map { case (n, v) =>
      (f(n), v)
    })

  def mapValues(f: String => String): Query =
    Query(params.map { case (n, v) =>
      (n, v map f)
    })

  def filter(f: ((String, Option[String])) => Boolean): Query =
    Query(params.filter(f))

  def filterNames(f: String => Boolean): Query =
    Query(params.filter { case (n, _) =>
      f(n)
    })

  def filterValues(f: String => Boolean): Query =
    Query(params.filter {
      case (_, Some(v)) => f(v)
      case _            => false
    })

  def filterOptionValues(f: Option[String] => Boolean): Query =
    Query(params.filter { case (_, v) =>
      f(v)
    })
  def replaceAll(k: String, v: Option[String]): Query = Query(params.filterNot(_._1 == k) :+ (k -> v))
  def replaceAll(k: String, v: String): Query         = replaceAll(k, Some(v))
  def removeAll(k: String): Query                     = filterNames(_ != k)
  def removeAll(k: String*): Query                    = removeAll(k)
  def removeAll(k: IterableOnce[String]): Query       = filterNames(name => !k.iterator.contains(name))
  def isEmpty: Boolean                                = params.isEmpty
  def nonEmpty: Boolean                               = params.nonEmpty

  override def toString =
    paramMap
      .map { case (k, v) =>
        if (v.isEmpty)
          k
        else
          s"$k=${v.mkString(",")}"
      }
      .mkString("&")
}
