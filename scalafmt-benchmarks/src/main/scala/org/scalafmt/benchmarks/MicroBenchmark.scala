package org.scalafmt.benchmarks

import org.scalafmt.Scalafmt
import org.scalafmt.config.{Indents, ScalafmtConfig}
import org.scalafmt.sysops.{FileOps, PlatformFileOps}

import scala.meta.dialects

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
  * The `style` param selects a `newlines.source` mode from [[BenchStyles]]
  * (`classic` == unspecified/default). Each corpus fixes its own dialect via
  * `adjust` (Scala 2 corpus keeps the default; the Scala 3 corpus sets scala3).
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

  @Param(Array("classic", "keep", "fold", "unfold"))
  var style: String = _

  var cfg: ScalafmtConfig = _
  var relLhsCfg: ScalafmtConfig = _

  /** Corpus-specific config tweak; overridden to set a non-default dialect. */
  protected def adjust(cfg: ScalafmtConfig): ScalafmtConfig = cfg

  @Setup
  def setup(): Unit = {
    code = PlatformFileOps.readFile(getPath)
    cfg = adjust(BenchStyles.byName(style))
    // exercises State.getUnexpired's non-empty `relativeToLhsLastLine` path
    relLhsCfg = cfg.copy(indent =
      cfg.indent.copy(relativeToLhsLastLine =
        Seq(Indents.RelativeToLhs.`match`, Indents.RelativeToLhs.`infix`),
      ),
    )
  }

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
  def scalafmt(): String = Scalafmt.format(code, cfg).get

  @Benchmark
  def scalafmt_relLhs(): String = Scalafmt.format(code, relLhsCfg).get

  @Benchmark
  def scalafmt_rewrite(): String = formatRewrite(code, cfg)

  def testMe(): Unit = {
    style = "classic"
    setup()
    scalafmt()
    scalafmt_relLhs()
    scalafmt_rewrite()
  }

}

object Micro {

  abstract class ScalaJsFile(filename: String)
      extends MicroBenchmark("scala-js", filename)
  abstract class SparkFile(filename: String)
      extends MicroBenchmark("spark", filename)

  class Small extends ScalaJsFile("EventSerializers.scala")
  class Medium extends ScalaJsFile("PrintStreamTest.scala")
  class Large extends SparkFile("HiveMetastoreCatalog.scala")
  class ExtraLarge extends ScalaJsFile("GenJSCode.scala")

  class Scala3 extends MicroBenchmark("scala3", "Scala3Syntax.scala") {
    override protected def adjust(cfg: ScalafmtConfig): ScalafmtConfig = cfg
      .withDialect(dialects.Scala3, "scala3")
  }

}
