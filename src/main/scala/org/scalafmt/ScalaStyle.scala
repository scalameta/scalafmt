package org.scalafmt

import scala.concurrent.duration.Duration

/**
  * A scalafmt style.
  *
  * To support pickling with upickle, it's defined as a trait. It could be a
  * sealed abstract class.
  */
sealed trait ScalaStyle {
  // TODO(olafur) soft column-limit?
  /**
    * Column limit, any formatting exceeding this field is penalized heavily.
    */
  def maxColumn: Int = 80

  /**
    * See [[ArgumentHandling]].
    */
  def argumentHandling: ArgumentHandling = OneArgOneLine

  /**
    * The maximum duration the formatter is allowed to run before giving up.
    * If the formatter times out, the original source code is returned.
    */
  def maxDuration: Duration = Duration(200, "ms")

  /**
    * Debugging only. Maximum number of states that the formatter may visit.
    *
    * @return
    */
  def maxStateVisits = 10000

  /**
    * Debugging only. Should scalafmt create a diagnostics report.
    */
  def debug: Boolean = false

}

/**
  * Recommended style if you are not sure which one to pick.
  */
case object Standard extends ScalaStyle

/**
  * Debugging only. Used in unit tests.
  */
sealed trait UnitTestStyle extends ScalaStyle {
  override def maxDuration = Duration(10, "s")
  override val debug = true
}

case object UnitTest80 extends UnitTestStyle

case object UnitTest40 extends UnitTestStyle {
  override def maxColumn = 40
}