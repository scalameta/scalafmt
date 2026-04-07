package org.scalafmt

package object dynamic {

  private[dynamic] type FormatEval[T] = Either[ScalafmtDynamicError, T]

  type FormatResult = FormatEval[String]

}
