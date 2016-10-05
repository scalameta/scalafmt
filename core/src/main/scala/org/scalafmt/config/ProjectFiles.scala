package org.scalafmt.config

import metaconfig.ConfigReader

@ConfigReader
case class ProjectFiles(
    git: Boolean = false,
    files: Seq[String] = Nil,
    includeFilter: Seq[String] = Nil,
    excludeFilter: Seq[String] = Nil
)
