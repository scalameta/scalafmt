package org.scalafmt.config

import scala.language.reflectiveCalls
import metaconfig._
import java.io.File
import metaconfig.Conf
import metaconfig.ConfError
import metaconfig.Configured
import metaconfig.Configured.Ok
import org.scalafmt.config.PlatformConfig._

// NOTE: these methods are intended for internal usage and are subject to
// binary and source breaking changes between any release. For a stable API
// use org.scalafmt.Scalafmt. Documentation on using scalafmt as a library
// can be seen here https://scalameta.org/scalafmt/#Standalonelibrary
object Config {

  def fromInput(input: Input, path: Option[String]): Configured[Conf] = {
    val configured = implicitly[MetaconfigParser].fromInput(input)
    path match {
      case Some(x) => ConfDynamic(configured).selectDynamic(x).asConf
      case None => configured
    }
  }

  def fromHoconString(string: String): Configured[ScalafmtConfig] =
    fromHoconString(string, None)

  def fromHoconString(
      string: String,
      path: Option[String]
  ): Configured[ScalafmtConfig] =
    fromHoconString(string, path, ScalafmtConfig.default)

  def fromHoconString(
      string: String,
      path: Option[String],
      default: ScalafmtConfig
  ): Configured[ScalafmtConfig] =
    fromConf(fromInput(Input.String(string), path), default = default)

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHoconFile(
      file: File,
      path: Option[String] = None,
      default: ScalafmtConfig = ScalafmtConfig.default
  ): Configured[ScalafmtConfig] =
    fromConf(fromInput(Input.File(file), path), default = default)

  def fromConf(
      conf: Configured[Conf],
      path: Option[String] = None,
      default: ScalafmtConfig = ScalafmtConfig.default
  ): Configured[ScalafmtConfig] =
    conf.andThen(confRename(_, renamedParameters)).andThen { baseConf =>
      val next = path match {
        case None => Ok(baseConf)
        case Some(p) =>
          baseConf match {
            case Conf.Obj(values) =>
              values
                .collectFirst { case (`p`, value) => Ok(value) }
                .getOrElse(
                  ConfError.message(s"Config $baseConf has no field $p").notOk
                )
            case x =>
              ConfError.typeMismatch("Conf.Obj", x).notOk
          }
      }
      ScalafmtConfig.configReader(default).read(next)
    }

  private val renamedParameters = Map.empty[String, String]

  def confRename(conf: Conf, srcToDst: Map[String, String]): Configured[Conf] =
    conf match {
      case obj: Conf.Obj if srcToDst.nonEmpty =>
        val groupedByDst =
          srcToDst.toSeq.groupBy(_._2).mapValues(_.map(_._1)).toList
        confRenameByDst(obj, groupedByDst)
      case _ =>
        if (srcToDst.isEmpty) Configured.Ok(conf)
        else ConfError.message("Can't rename parameters, not a Conf.Obj").notOk
    }

  private def confRenameByDst(
      conf: Conf.Obj,
      dstToSrcs: Seq[(String, Seq[String])]
  ): Configured[Conf.Obj] = {
    val (dst, srcs) = dstToSrcs.head
    val dstParams = dst.split('.')
    val srcInfo = srcs.flatMap { src =>
      val params = src.split('.')
      confLookup(conf, params).map((src, params, _))
    }
    val cmp = srcInfo.lengthCompare(1)
    if (cmp > 0) {
      val msg = srcInfo.map(_._1).mkString(", ")
      ConfError.message(s"Multiple params renamed to $dst: $msg").notOk
    } else if (cmp == 0 && confLookup(conf, dstParams).isDefined) {
      val src = srcInfo.head._1
      ConfError.message(s"Can't rename $src to $dst, exists").notOk
    } else {
      val tail = dstToSrcs.tail
      val renamed =
        if (tail.nonEmpty) confRenameByDst(conf, tail) else Configured.Ok(conf)
      if (cmp < 0) renamed
      else
        renamed.map { x =>
          val (src, srcParams, srcObj) = srcInfo.head
          Console.err.println(
            s"Deprecated configuration: use $dst instead of $src"
          )
          confAppend(confDelete(x, srcParams), dstParams, srcObj)
        }
    }
  }

  private def confLookup(conf: Conf.Obj, params: Seq[String]): Option[Conf] = {
    val key = params.head
    val rest = params.tail
    if (rest.isEmpty) {
      conf.values.collectFirst {
        case (`key`, v) => v
      }
    } else {
      conf.values.collectFirst {
        case (`key`, v: Conf.Obj) => confLookup(v, rest)
      }.flatten
    }
  }

  private def confAppend(
      conf: Conf.Obj,
      params: Seq[String],
      value: Conf
  ): Conf.Obj = {
    val head = params.head
    val tail = params.tail
    val nonmatching = conf.values.filter(_._1 != head)
    if (tail.isEmpty) {
      Conf.Obj(nonmatching :+ head -> value)
    } else {
      val matchingOpt = conf.values.collectFirst {
        case (`head`, obj: Conf.Obj) => obj
      }
      val matching = matchingOpt.getOrElse(Conf.Obj.empty)
      Conf.Obj(nonmatching :+ (head -> confAppend(matching, tail, value)))
    }
  }

  private def confDelete(
      conf: Conf.Obj,
      params: Seq[String]
  ): Conf.Obj = {
    val head = params.head
    val tail = params.tail
    val nonmatching = conf.values.filter(_._1 != head)
    if (tail.isEmpty) {
      Conf.Obj(nonmatching)
    } else {
      val matchingOpt = conf.values.collectFirst {
        case (`head`, obj: Conf.Obj) => obj
      }
      val matching = matchingOpt.getOrElse(Conf.Obj.empty)
      val deleted = confDelete(matching, tail)
      if (deleted.values.isEmpty)
        Conf.Obj(nonmatching)
      else
        Conf.Obj(nonmatching :+ (head -> deleted))
    }
  }

}
