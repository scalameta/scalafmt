package org.scalafmt.config

import scala.util.matching.Regex

import metaconfig.ConfigReader

@ConfigReader
case class ProjectFiles(
    git: Boolean = false,
    files: Seq[String] = Nil,
    includeFilters: Seq[String] = Seq(".*"),
    excludeFilters: Seq[String] = Nil
)
