package org.scalafmt.config

import metaconfig._

case class GroupImportsSettings(
    groups: Seq[String] = Seq("java", "scala", "_"),
    emptyLineBetweenGroups: Boolean = true
) {
  val reader: ConfDecoder[GroupImportsSettings] =
    generic.deriveDecoder(this).noTypos
}

object GroupImportsSettings {
  implicit lazy val surface: generic.Surface[GroupImportsSettings] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[GroupImportsSettings] =
    generic.deriveEncoder

  def default = GroupImportsSettings()
}
