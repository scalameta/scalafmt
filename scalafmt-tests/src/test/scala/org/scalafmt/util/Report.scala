package org.scalafmt.util

import scalatags.Text
import scalatags.Text.all._

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.stats.TestStats

object Report {
  val MaxVisits = 8 // 2 ** 6

  def heatmap(results: Seq[Result]): String =
    reportBody(
      div(
        h1(id := "title", "Heatmap"),
        explanation,
        for (result <- results.sortBy(-_.maxVisitsOnSingleToken)
          if result.test.name != "Warmup") yield {
          div(
            h2(result.title),
            pre(
              fontFamily := "monospace",
              background := "#fff",
              fontSize := "16px",
              width := testWidth(result),
              code(
                heatmapBar(result.test.style),
                raw(result.obtainedHtml),
                span("\n" + ("‾" * result.test.style.maxColumn))
              )
            )
          )
        }
      )
    ).render

  def heatmapBar(scalaStyle: ScalafmtConfig): Seq[Text.Modifier] =
    (1 to MaxVisits).map { i =>
      val v = Math.pow(2, i).toInt
      val color = red(v)
      span(
        background := s"rgb(256, $color, $color)",
        s" $v "
      )
    } :+ span("\n" + ("_" * scalaStyle.maxColumn) + "\n")

  def red(visits: Int): Int = {
    val v = log(visits, 2)
    val ratio = v / MaxVisits.toDouble
    val result = Math.min(256, 20 + 256 - (ratio * 256)).toInt
    result
  }

  def log(x: Int, base: Int): Double = {
    Math.log(x) / Math.log(base)
  }

  def explanation =
    div(
      p("""Formatting output from scalafmt's test suite.
          |The formatter uses Dijkstra's shortest path to determine the
          |formatting with the "cheapest" cost. The red regions are
          |tokens the formatter visits often.
        """.stripMargin),
      ul(
        li("Declaration arguments: bin packed"),
        li("Callsite arguments: one arg per line if overflowing")
      )
    )

  def testWidth(result: Result) = result.test.style.maxColumn.toDouble * 9.625

  def reportBody(xs: Text.Modifier*) =
    html(
      body(background := "#f9f9f9", xs)
    )

  def compare(before: TestStats, after: TestStats): String =
    reportBody(
      div(
        h1(
          id := "title",
          s"Compare ${after.gitInfo.branch} and" +
            s" ${before.gitInfo.branch}" +
            s" (${before.shortCommit}...${after.shortCommit})"
        ),
        explanation,
        after
          .intersectResults(before)
          .sortBy {
            case (aft, bef) =>
              -Math.abs(aft.visitedStates - bef.visitedStates)
          }
          .map {
            case (aft, bef) =>
              div(
                h2(aft.test.fullName),
                table(
                  tr(
                    th(""),
                    th("Before"),
                    th("After"),
                    th("Diff")
                  ),
                  tr(
                    td("Time (ms)"),
                    td(bef.timeMs),
                    td(aft.timeMs),
                    td(bef.timeMs - aft.timeMs)
                  ),
                  tr(
                    td("States"),
                    td(bef.visitedStates),
                    td(aft.visitedStates),
                    td(aft.visitedStates - bef.visitedStates)
                  )
                ),
                pre(
                  fontFamily := "monospace",
                  background := "#fff",
                  fontSize := "16px",
                  width := testWidth(aft),
                  code(
                    heatmapBar(aft.test.style),
                    raw(mkHtml(mergeResults(aft, bef), aft.test.style))
                  )
                )
              )
          }
      )
    ).render

  def mergeResults(after: Result, before: Result): Seq[FormatOutput] =
    after.tokens.zip(before.tokens).map {
      case (aft, bef) =>
        FormatOutput(aft.token, aft.whitespace, aft.visits - bef.visits)
    }

  def mkHtml(output: Seq[FormatOutput], scalaStyle: ScalafmtConfig): String = {
    val sb = new StringBuilder()
    output.foreach { x =>
      import scalatags.Text.all._
      val redness = red(Math.abs(x.visits))
      val isRed =
        if (x.visits > 0) true
        else false
      val styleBackground =
        if (isRed) s"rgb(256, $redness, $redness)"
        else s"rgb($redness, 256, $redness)" // green
      val html = span(background := styleBackground, x.token).render
      sb.append(html)
      sb.append(x.whitespace.replace("\n\n", "\n\n"))
    }
    sb.toString()
  }
}
