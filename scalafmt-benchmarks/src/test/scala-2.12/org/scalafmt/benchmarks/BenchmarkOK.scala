package org.scalafmt.benchmarks

import benchmarks.MacroBenchmark
import org.scalafmt.benchmarks.Micro.ScalaJsFile
import org.scalatest.FunSuite

class TestMacroP extends MacroBenchmark(true, 10)
class TestMacroS extends MacroBenchmark(true, 5)
class TestMicroSmall extends ScalaJsFile("EventSerializers.scala")
class TestMicroMedium extends ScalaJsFile("PrintStreamTest.scala")

class BenchmarkOK extends FunSuite {

  Seq(
    new TestMacroP,
    new TestMacroS
  ).foreach { benchmark =>
    val name = s"macroBenchmark: $benchmark"
    test(name) {
      benchmark.testMe()
      println(name)
    }
  }

  Seq(
    new TestMicroMedium,
    new TestMicroSmall
  ).foreach { formatBenchmark =>
    val name = s"microBenchmark: ${formatBenchmark.getClass}"
    test(name) {
      formatBenchmark.testMe()
      println(name)
    }
  }
}
