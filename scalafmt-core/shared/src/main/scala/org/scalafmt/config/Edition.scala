package org.scalafmt.config

import java.time.YearMonth

import metaconfig.{Conf, ConfDecoder, ConfEncoder, Configured}

class Edition(val ym: YearMonth) extends AnyVal with Ordered[Edition] {
  override def toString: String = ym.toString
  override def compare(o: Edition): Int = ym.compareTo(o.ym)
}

object Edition {

  val Latest = Edition(9999, 12)

  def apply(year: Int, month: Int): Edition =
    new Edition(YearMonth.of(year, month))
  def apply(str: String): Edition =
    new Edition(YearMonth.parse(str))

  lazy val format = "(\\d{4})-(\\d{1,2})".r

  implicit val decoder: ConfDecoder[Edition] =
    ConfDecoder.instanceExpect("'$year-$month', for example '2019-08'") {
      case Conf.Str(format(year, month)) =>
        try Configured.ok(Edition(year.toInt, month.toInt))
        catch { case e: Exception => Configured.error(e.getMessage) }
      case Conf.Str("latest") => Configured.ok(Latest)
    }

  implicit val encoder: ConfEncoder[Edition] =
    ConfEncoder.instance(edition => Conf.Str(edition.ym.toString))
}
