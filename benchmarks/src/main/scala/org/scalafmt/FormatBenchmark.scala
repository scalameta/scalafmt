package org.scalafmt

import scala.meta.Source
import scalariform.formatter.preferences._
import scalariform.formatter.ScalaFormatter


import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.Warmup

@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.NANOSECONDS)
class FormatBenchmark {
  val tests = UnitTests.tests.map(_.original).toArray

  val fmt = new ScalaFmt(Standard)

  val preferences = FormattingPreferences().setPreference(IndentSpaces, 3)

  @Benchmark
  def scalafmt(): Unit = {
    tests.foreach(x => fmt.format_![Source](x, None)(scala.meta.parsers.parseSource))
  }

  @Benchmark
  def scalariform(): Unit = {
    tests.foreach(ScalaFormatter.format(_, preferences))
  }
}
