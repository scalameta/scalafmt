package org.scalafmt

import scala.concurrent.duration.Duration

/**
  * A scalafmt style, use [[ScalaStyle.Default]].
  */
// TODO(olafur) refactor to case class or something simpler

sealed trait ScalaStyle {

  /**
    * Column limit, any formatting exceeding this field is penalized heavily.
    */
  // TODO(olafur) soft column-limit?

  def maxColumn: Int = 80

  /**
    * Use scaladoc style docstring, otherwise javadoc style comments.
    *
    * Scaladoc:
    *
    * /** Title.
    *   *
    *   */
    *
    * Javadoc:
    * /**
    *  * Title.
    *  */
    *
    *
    */
  def scalaDocs: Boolean = true

  /**
    * Call-site arguments.
    *
    * If true, will fit as many arguments on each line, only breaking at commas.
    * If false, a function call’s arguments will either be all on the same line
    * or will have one line each.
    */
  def binPackArguments: Boolean = false

  /**
    * Same as [[binPackArguments]], except for def/class definition parameters.
    */
  def binPackParameters: Boolean = false

  /**
    * Call-sites where there is a newline after opening ( and newline
    * before closing ). If true, preserves the newlines and keeps one line per
    * argument.
    */
  def configStyleArguments: Boolean = true

  /**
    * Chains of . selection.
    *
    * If true, will fit as many arguments on each line, only breaking at dots.
    * If false, a either all selects go on the same line or will have one line
    * each.
    */
  def binPackDotChains: Boolean = false

  /**
    * Debugging only. Should scalafmt create a diagnostics report.
    */
  def debug: Boolean = false
}

/**
  * Debugging only. Used in unit tests.
  */
protected[scalafmt] sealed trait UnitTestStyle extends ScalaStyle {
  override val debug = true
}

object ScalaStyle {

  /**
    * Recommended style if you are not sure which one to pick.
    */
  case object Default extends ScalaStyle

  /**
    * EXPERIMENTAL.
    * https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md
    */
  case object ScalaJs extends ScalaStyle {

    override def debug = true

    override def binPackParameters = true

    // TODO(olafur) should be true
    override def binPackArguments = false
  }

  case object UnitTest80 extends UnitTestStyle

  case object UnitTest40 extends UnitTestStyle {

    override def maxColumn = 40
  }

  // TODO(olafur) make scalastyle a case class and each style is an instance.
  case class CustomStyleBecauseIDontLikeTheProvidedStyles(
      override val scalaDocs: Boolean = Default.scalaDocs,
      override val maxColumn: Int = Default.maxColumn,
      override val binPackParameters: Boolean = Default.binPackParameters,
      override val binPackArguments: Boolean = Default.binPackArguments,
      override val binPackDotChains: Boolean = Default.binPackDotChains)
      extends ScalaStyle
}
