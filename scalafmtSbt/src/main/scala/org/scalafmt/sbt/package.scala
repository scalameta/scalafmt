package org.scalafmt

package object sbt {
  type ScalaFmtLike = {
    def format(code: String, configFile: String, filename: String): String
    def format(code: String, filename: String): String
  }
}
