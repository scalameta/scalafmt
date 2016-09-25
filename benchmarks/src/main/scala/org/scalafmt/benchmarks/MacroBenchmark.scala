package org.scalafmt.benchmarks

import scala.collection.GenIterable
import scala.util.Try

import java.io.File
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.Warmup
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtOptimizer
import org.scalafmt.ScalafmtRunner
import org.scalafmt.ScalafmtStyle
import org.scalafmt.util.ScalaFile
import org.scalafmt.util.FileOps
import scala.meta.Source
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences.FormattingPreferences
import scalariform.formatter.preferences.IndentSpaces

import org.scalafmt.config.RewriteSettings
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.rewrite.SortImports

trait FormatBenchmark {
  def formatRewrite(code: String) = {
    Scalafmt
      .format(code,
              style = ScalafmtStyle.default.copy(
                rewrite = RewriteSettings(
                  rules = Seq(SortImports, RedundantBraces)
                )
              ))
      .get
  }
}

/**
  * Formats filename at with scalafmt and scalariform.
  *
  * To run benchmark:
  *
  * > benchmarks/jmh:run -i 10 -wi 10 -f1 -t1 org.scalafmt.*
  */
@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.SECONDS)
abstract class MacroBenchmark(parallel: Boolean, maxFiles: Int)
    extends FormatBenchmark {
  val scalariformPreferences =
    FormattingPreferences().setPreference(IndentSpaces, 3)
  var files: GenIterable[String] = _

  override def toString = s"${this.getClass.getName}(parallel=$parallel)"

  @Setup
  def setup(): Unit = {
    files = {
      val x = ScalaFile.getAll.filter { f =>
        f.projectUrl.contains("scala-js")
      }.take(maxFiles).map(_.read)
      if (parallel) x.par
      else x
    }
  }

  def testMe(): Unit = {
    setup()
    scalafmt()
    scalariform()
  }

  @Benchmark
  def scalafmt(): Unit = {
    files.foreach { file =>
      Try(Scalafmt.format(file))
    }
  }

  @Benchmark
  def scalafmt_rewrite(): Unit = {
    files.foreach { file =>
      Try(formatRewrite(file))
    }
  }

  // No need to run same benchmark again and again.
  @Benchmark
  def scalariform(): Unit = {
    files.foreach { file =>
      Try(ScalaFormatter.format(file))
    }
  }
}

object MacroSmall {
  val size = 10
  class Parallel extends MacroBenchmark(parallel = true, size)
//  class Synchronous extends MacroBenchmark(parallel = false, size)
}

object MacroHuge {
  val size = 10000
  class Parallel extends MacroBenchmark(parallel = true, size)
//  class Synchronous extends MacroBenchmark(parallel = false, size)
}
