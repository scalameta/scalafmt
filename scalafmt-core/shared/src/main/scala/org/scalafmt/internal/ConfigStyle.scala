package org.scalafmt.internal

sealed abstract class ConfigStyle

object ConfigStyle {
  case object None extends ConfigStyle
  case object Source extends ConfigStyle
  case object Forced extends ConfigStyle
}
