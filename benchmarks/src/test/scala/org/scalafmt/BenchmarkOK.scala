package org.scalafmt

import org.scalafmt.util.FormatAssertions
import org.scalatest.FunSuite

class BenchmarkOK extends FunSuite with FormatAssertions {
  import org.scalafmt.benchmarks.run._

  import scala.meta._

  // TODO(olafur) DRY.
  val formatBenchmarks = Seq(
    new OptimizerCore,
    new SourceMapWriter,
    new Division,
    new BaseLinker
  )
  formatBenchmarks.foreach { formatBenchmark =>
    val name = formatBenchmark.getClass.getSimpleName
    test(s"$name: runs without exception.") {
      formatBenchmark.setup()
      formatBenchmark.scalametaParser()
      formatBenchmark.scalafmt()
      formatBenchmark.scalariform()
      println(name)
    }
  }
}
