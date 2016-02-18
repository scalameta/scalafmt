package org.scalafmt

import org.scalafmt.util.FormatAssertions
import org.scalatest.FunSuite

class BenchmarkOK extends FunSuite with FormatAssertions {
  import org.scalafmt.benchmarks.run._

  import scala.meta._

  // TODO(olafur) DRY.
  val formatBenchmarks = Seq(
    new Basic,
    new Utils,
    new SourceMapWriter,
   // TODO(olafur) make larger files integration tests.
    new Division,
    new BaseLinker,
    new JsDependency
  )
  formatBenchmarks.foreach { formatBenchmark =>
    val name = formatBenchmark.getClass.getSimpleName
    test(s"$name: runs without exception.") {
      formatBenchmark.setup()
      formatBenchmark.scalafmt()
      formatBenchmark.scalariform()
      println(name)
    }
  }
}
