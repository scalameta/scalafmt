package org.scalafmt

import scala.meta._

import scala.language.implicitConversions

package object config extends ScalafmtConfDecoders {
  type MetaParser = parsers.Parse[_ <: Tree]

  type NamedDialect = sourcecode.Text[Dialect]

  implicit def toDialect(nd: NamedDialect): Dialect = nd.value

  implicit class ImplicitNamedDialect(private val nd: NamedDialect)
      extends AnyVal {
    def name: String = nd.source
    def dialect: Dialect = nd.value
  }
}
