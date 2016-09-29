package org.scalafmt.config.hocon

import scala.util.control.NonFatal

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory

object Hocon2Class {
  private def config2map(config: Config): Map[String, Any] = {
    import scala.collection.JavaConverters._
    def loop(obj: Any): Any = obj match {
      case map: java.util.Map[_, _] =>
        map.asScala.map {
          case (key, value) => key -> loop(value)
        }.toMap
      case map: java.util.List[_] =>
        map.asScala.map(loop).toList
      case e => e
    }
    loop(config.root().unwrapped()).asInstanceOf[Map[String, Any]]
  }

  def gimmeConfig(
      str: String,
      path: Option[String]): metaconfig.Result[Map[String, Any]] = {
    try {
      val config = ConfigFactory.parseString(str)
      val extracted = path match {
        case Some(p) => config.getConfig(p)
        case _ => config
      }
      Right(config2map(extracted))
    } catch {
      case NonFatal(e) => Left(e)
    }
  }

  def gimmeClass[T](configStr: String,
                    reader: metaconfig.Reader[T],
                    path: Option[String]): metaconfig.Result[T] = {
    for {
      config <- gimmeConfig(configStr, path).right
      clz <- reader.read(config).right
    } yield clz
  }

}
