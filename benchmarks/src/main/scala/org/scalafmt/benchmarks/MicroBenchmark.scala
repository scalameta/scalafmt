package org.scalafmt.benchmarks

import scala.collection.mutable

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
import org.scalafmt.util.FileOps
import scala.meta.Source
import scala.meta.Tree
import scala.meta.dialects.Scala211
import scala.meta.internal.parsers.ScalametaParser
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences.FormattingPreferences
import scalariform.formatter.preferences.IndentSpaces

import org.scalafmt.config.RedundantBracesSettings
import org.scalafmt.config.RewriteSettings
import org.scalafmt.config.ScalafmtOptimizer
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.rewrite.RedundantBraces
import org.scalafmt.rewrite.SortImports
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TokenOps.TokenHash
import org.scalafmt.util.TreeOps

/**
  * Formats filename at [[path]] with scalafmt and scalariform.
  *
  * To run benchmark:
  *
  * > benchmarks/jmh:run -i 10 -wi 10 -f1 -t1 org.scalafmt.*
  */
@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class MicroBenchmark(path: String*) extends FormatBenchmark {
  val scalariformPreferences =
    FormattingPreferences().setPreference(IndentSpaces, 3)
  val classLoader = getClass.getClassLoader
  var code: String = _
  var tree: Tree = _

  @Setup
  def setup(): Unit = {
    code = FileOps.readFile(getPath.getAbsolutePath)
  }

  def getPath: File = {
    val filename = FileOps.getFile(Seq("src", "resources") ++ path: _*)
    // jmh runs from benchmarks directory while tests run from from root.
    // Can't bother to find more generic solution
    if (filename.isFile) filename
    else FileOps.getFile(Seq("benchmarks", "src", "resources") ++ path: _*)
  }

//  @Benchmark
  def scalametaParser(): Unit = {
    import scala.meta._
    code.parse[Source]
  }

  @Benchmark
  def scalafmt(): String = {
    Scalafmt.format(code).get
  }

//  @Benchmark
  def scalafmt_rewrite(): String = {
    formatRewrite(code)
  }

  def testMe(): Unit = {
    setup()
    scalafmt()
    scalafmt_rewrite()
    getOwners()
    fastGetOwners()
  }

//  @Benchmark
  def getOwners(): String = {
    import scala.meta._
    val tree = code.parse[Source].get
    TreeOps.getOwners(tree).nonEmpty.toString
  }

//  @Benchmark
  def fastGetOwners(): String = {
    import scala.meta._
    val tree = code.parse[Source].get
    TreeOps.fastGetOwners(tree).nonEmpty.toString
  }

//  @Benchmark
  def scalafmt_noPruneSlowStates(): String = {
    Scalafmt
      .format(
        code,
        style = ScalafmtConfig.default.copy(
          runner = ScalafmtRunner.default.copy(optimizer =
            ScalafmtOptimizer.default.copy(pruneSlowStates = false)))
      )
      .get
  }

  // No need to run same benchmark again and again.
//  @Benchmark
  def scalariform(): String = {
    ScalaFormatter.format(code)
  }
}

object Micro {

  abstract class ScalaJsFile(filename: String)
      extends MicroBenchmark("scala-js", filename)
  abstract class SparkFile(filename: String)
      extends MicroBenchmark("spark", filename)

//  class OptimizerCore extends ScalaJsFile("OptimizerCore.scala")

//  class GenJsCode extends ScalaJsFile("GenJSCode.scala")
  class Small extends ScalaJsFile("EventSerializers.scala")
  class Medium extends ScalaJsFile("PrintStreamTest.scala")
  class Large extends SparkFile("HiveMetastoreCatalog.scala")
  class ExtraLarge extends ScalaJsFile("GenJSCode.scala")

//  class ScalaJSClassEmitter
//      extends ScalaJsFile("ScalaJSClassEmitter.scala")
//
//  class JavaLangString extends ScalaJsFile("JavaLangString.scala")
}
