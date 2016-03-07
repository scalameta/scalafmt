package org.scalafmt

package object sbt {
  type ScalaFmtLike = {
    def format(code: String): String
  }
}
