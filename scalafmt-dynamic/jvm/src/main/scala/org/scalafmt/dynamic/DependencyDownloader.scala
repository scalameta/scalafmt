package org.scalafmt.dynamic

import java.net.URL

import scala.util.Try

trait DependencyDownloader {
  def download(dependencies: Seq[Dependency]): Try[Seq[URL]]
}

trait DependencyDownloaderFactory {
  def create(properties: ScalafmtProperties): DependencyDownloader
}
