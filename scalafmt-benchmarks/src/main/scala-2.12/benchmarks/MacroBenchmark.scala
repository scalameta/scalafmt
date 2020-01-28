package benchmarks

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._
import org.scalafmt.Scalafmt
import org.scalafmt.benchmarks.FormatBenchmark

import scala.collection.GenIterable
import scala.meta.testkit.Corpus
import scala.util.Try

/**
  * Formats filename at with scalafmt.
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
  var files: GenIterable[String] = _

  override def toString = s"${this.getClass.getName}(parallel=$parallel)"

  @Setup
  def setup(): Unit = {
    files = {
      val x = Corpus
        .files(
          Corpus.fastparse.copy(
            // TODO(olafur) remove once testkit 1.7 is out
            url = Corpus.fastparse.url.replace("olafurpg", "scalameta")
          )
        )
        .filter { f =>
          f.projectUrl.contains("scala-js")
        }
        .take(maxFiles)
        .map(_.read)
        .toBuffer
      if (parallel) x.par
      else x
    }
  }

  def testMe(): Unit = {
    setup()
    scalafmt()
  }

  @Benchmark
  def scalafmt(): Unit = {
    files.foreach { file =>
      Try(Scalafmt.formatCode(file))
    }
  }

  @Benchmark
  def scalafmt_rewrite(): Unit = {
    files.foreach { file =>
      Try(formatRewrite(file))
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
