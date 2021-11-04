package org.scalafmt.stats

case class JavaInfo(name: String, vendor: String, version: String)

object JavaInfo {

  def apply(): JavaInfo =
    JavaInfo(
      sys.props("java.vm.name"),
      sys.props("java.vm.vendor"),
      sys.props("java.vm.version")
    )
}
