package org.scalafmt

import java.io.InputStream
import java.io.PrintStream

import com.martiansoftware.nailgun.NGContext

package object bootstrap {
  type ScalafmtCli = {
    def nailMain(nGContext: NGContext): Unit
    def main(args: Array[String]): Unit
    def main(args: Array[String],
             in: InputStream,
             out: PrintStream,
             err: PrintStream,
             workingDirectory: String): Unit
  }
}
