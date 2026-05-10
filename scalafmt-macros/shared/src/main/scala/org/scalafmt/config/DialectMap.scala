package org.scalafmt.config

import scala.meta.Dialect

private[scalafmt] object DialectMap {
  type ValType = (Dialect, Any) => Dialect
  type MapType = Map[String, ValType]
}
