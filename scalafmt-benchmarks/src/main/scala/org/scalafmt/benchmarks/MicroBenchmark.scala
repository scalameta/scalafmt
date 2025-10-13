package org.scalafmt.benchmarks

import org.scalafmt.Scalafmt
import org.scalafmt.sysops.{FileOps, PlatformFileOps}

import java.nio.file.Path
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

/** Formats some files with scalafmt.
  *
  * To run benchmark:
  * {{{
  * > benchmarks/jmh:run -i 10 -wi 10 -f1 -t1 org.scalafmt.*
  * }}}
  *
  * @param path
  *   filename(s) to format
  */
@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime)) @OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class MicroBenchmark(path: String*) extends FormatBenchmark {
  val classLoader = getClass.getClassLoader
  var code: String = _

  @Setup
  def setup(): Unit = code = PlatformFileOps.readFile(getPath)

  def getPath: Path = {
    val filename = FileOps.getFile(Seq("src", "resources") ++ path)
    // jmh runs from benchmarks directory while tests run from from root.
    // Can't bother to find more generic solution
    if (PlatformFileOps.isRegularFile(filename)) filename
    else FileOps.getFile(Seq("scalafmt-benchmarks", "src", "resources") ++ path)
  }

  def scalametaParser(): Unit = {
    import scala.meta._
    code.parse[Source]
  }

  @Benchmark
  def scalafmt(): String = Scalafmt.format(code).get

  @Benchmark
  def scalafmt_rewrite(): String = formatRewrite(code)

  def testMe(): Unit = {
    setup()
    scalafmt()
    scalafmt_rewrite()
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
