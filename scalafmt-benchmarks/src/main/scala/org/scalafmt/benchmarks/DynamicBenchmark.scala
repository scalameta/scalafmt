package org.scalafmt.benchmarks

import org.scalafmt.{Scalafmt => CoreScalafmt}
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.interfaces.{Scalafmt => IScalafmt}
import org.scalafmt.interfaces.{ScalafmtSession, ScalafmtSessionFactory}

import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

/** Benchmarks `scalafmt-dynamic` overhead — the "stuff around formatting" paid
  * when `.scalafmt.conf` pins a version other than the running build's, so the
  * CLI resolves+classloads that version instead of formatting in-process.
  *
  * The CLI creates ONE dynamic instance + ONE session per run and reuses it, so
  * the interesting cost is the fixed per-invocation startup:
  * coursier class-load + (disk-cached) resolution + `URLClassLoader` +
  * reflection setup. That's measured cold, in fresh forks (each fork = a fresh
  * process, like a real `scalafmt` invocation), against cold in-process core as
  * the baseline; the delta is the dynamic tax.
  *
  * Pins 3.10.6 (must be in the coursier cache; it is if you've formatted scala-js
  * — otherwise the first fork pays a one-time network download).
  */
object DynamicBenchmark {
  // small but structured, so formatting does real work yet stays fast
  final val code =
    """object A { def f(x:Int,y:Int)={val z=x+y;List(1,2,3).map(_+z).filter(_>0)} }"""

  val file: Path = Paths.get("A.scala") // path metadata only

  def mkConfig(): Path = {
    val p = Files.createTempFile("scalafmt-dyn", ".scalafmt.conf")
    Files.write(
      p,
      """|version = 3.10.6
         |runner.dialect = scala213
         |""".stripMargin.getBytes("UTF-8"),
    )
    p
  }
}

@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@BenchmarkMode(Array(Mode.SingleShotTime)) @OutputTimeUnit(TimeUnit.MILLISECONDS)
class DynamicBenchmark {
  import DynamicBenchmark._

  private var config: Path = _

  @Setup(Level.Trial)
  def setup(): Unit = config = mkConfig()

  /** Fresh dynamic instance + first format: coursier load + resolve + classload
    * + reflect + one format. This is what a fresh `scalafmt` process pays.
    */
  @Benchmark
  def coldDynamic(): String =
    IScalafmt.create(getClass.getClassLoader).format(config, file, code)

  /** Baseline: cold in-process core format of the same snippet (no dynamic). */
  @Benchmark
  def coldCore(): String =
    CoreScalafmt.format(code, ScalafmtConfig.default).get
}

/** Warm per-format overhead: with the session reused (as the CLI does), how much
  * does a dynamic (reflection + cross-classloader) format add over a direct core
  * format of the same input. Expected: tiny.
  */
@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime)) @OutputTimeUnit(TimeUnit.MICROSECONDS)
class DynamicWarmBenchmark {
  import DynamicBenchmark._

  private var session: ScalafmtSession = _
  private var cfg: ScalafmtConfig = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    val config = mkConfig()
    val dyn = IScalafmt.create(getClass.getClassLoader)
    session = dyn.asInstanceOf[ScalafmtSessionFactory].createSession(config)
    session.format(file, code) // trigger resolve/classload/reflect now
    cfg = ScalafmtConfig.default
  }

  @Benchmark
  def warmDynamic(): String = session.format(file, code)

  @Benchmark
  def warmCore(): String = CoreScalafmt.format(code, cfg).get
}
