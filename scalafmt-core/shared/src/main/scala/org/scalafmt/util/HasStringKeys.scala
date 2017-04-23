package org.scalafmt.util

/**
  * Extend this class to expose the fields are string values.
  */
class HasStringKeys(implicit _args: sourcecode.Args) {
  val validKeys = _args.value.value.flatten.map(_.source).toSet
}
