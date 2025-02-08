package org.scalafmt.sysops

trait FileStat {

  def isDirectory: Boolean
  def isRegularFile: Boolean
  def isSymlink: Boolean

}
