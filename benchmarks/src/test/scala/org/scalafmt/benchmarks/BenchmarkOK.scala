package org.scalafmt.benchmarks

import org.scalafmt.benchmarks.run.ScalaJsFile
import org.scalafmt.util.FormatAssertions
import org.scalatest.FunSuite

class TestMacroP extends MacroBenchmark(true, 10)
class TestMacroS extends MacroBenchmark(true, 5)
class TestMicroSmall extends ScalaJsFile("EventSerializers.scala")
class TestMicroMedium extends ScalaJsFile("PrintStreamTest.scala")

class BenchmarkOK extends FunSuite with FormatAssertions {

  Seq(
    new TestMacroP,
    new TestMacroS
  ).foreach { benchmark =>
    val name = s"macroBenchmark: $benchmark"
    test(name) {
      benchmark.setup()
      benchmark.scalariform()
      benchmark.scalafmt()
      println(name)
    }
  }

  Seq(
    new TestMicroMedium,
    new TestMicroSmall
  ).foreach { formatBenchmark =>
    val name = s"microBenchmark: ${formatBenchmark.getClass}"
    test(name) {
      formatBenchmark.setup()
      formatBenchmark.scalametaParser()
      formatBenchmark.scalafmt()
      formatBenchmark.scalariform()
      println(name)
    }
  }
}
