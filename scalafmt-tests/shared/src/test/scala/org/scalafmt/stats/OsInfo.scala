package org.scalafmt.stats

case class OsInfo(name: String, architecture: String, version: String)

object OsInfo {

  def apply(): OsInfo =
    OsInfo(
      sys.props("os.name"),
      sys.props("os.arch"),
      sys.props("os.version")
    )
}
