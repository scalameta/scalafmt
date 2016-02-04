package org.scalafmt

import org.scalafmt.stats.TestStats

import scalatags.Text
import scalatags.Text.all._

object Report {

  val MaxVisits = 5 // 2 ** 5

  def heatmapBar(scalaStyle: ScalaStyle): Seq[Text.Modifier] =
    (1 to MaxVisits).map { i =>
      val v = Math.pow(2, i).toInt
      val color = red(v)
      span(
        background := s"rgb(256, $color, $color)",
        s" $v "
      )
    } :+ span("\n" + ("_" * scalaStyle.maxColumn) + "\n")

  def explanation =
    div(
      p( """Formatting output from scalafmt's test suite.
           |The formatter uses Dijkstra's shortest path to determine the
           |formatting with the "cheapest" cost. The red regions are
           |tokens the formatter visits often.
         """.stripMargin),
      ul(
        li("Declaration arguments: bin packed"),
        li("Callsite arguments: one arg per line if overflowing")
      )
    )

  def heatmap(results: Seq[Result]): String = {
    html(
      head(
      ),
      body(
        div(
          background := "#f9f9f9",
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
                width := result.test.style.maxColumn.toDouble * 9.625,
                code(
                  heatmapBar(result.test.style),
                  raw(result.obtainedHtml),
                  span("\n" + ("‾" * result.test.style.maxColumn))
                )
              )
            )
          }
        )
      )
    ).render
  }

  def compare(before: TestStats, after: TestStats): String = reportBody(
    div(
      h1(id := "title", s"Compare ${after.gitInfo.branch} and" +
        s" ${before.gitInfo.branch}" +
        s" (${before.shortCommit}...${after.shortCommit})"),
      explanation,
      after.intersectResults(before).sortBy {
        case (aft, bef) =>
          -Math.abs(aft.visitedStates - bef.visitedStates)
      }.map {
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
              code(
                raw( mkHtml(mergeResults(aft, bef), aft.test.style) )
              )
            )
          )
      }
    )
  ).render

  def mergeResults(after: Result, before: Result): Seq[FormatOutput] =
   after.tokens.zip(before.tokens).map {
    case (aft, bef) =>
      FormatOutput(aft.token, aft.whitespace, bef.visits - aft.visits)
  }

  def reportBody(xs: Text.Modifier*) =
    html(
      body(xs: _*)
    )



  def mkHtml(output: Seq[FormatOutput], scalaStyle: ScalaStyle): String = {
    val sb = new StringBuilder()

    output.foreach { x =>
      import scalatags.Text.all._
      val redness = red(x.visits)
      val html = span(
        background := s"rgb(256, $redness, $redness)",
        x.token).render
      sb.append(html)
      sb.append(x.whitespace.replace("\n\n", "\n\n"))
    }
    sb.toString()
  }

  def red(visits: Int): Int = {
    val v = log(visits, 2)
    val ratio = v / MaxVisits.toDouble
    val result = Math.min(256, 20 + 256 - (ratio * 256)).toInt
    result
  }

  def log(x: Int, base: Int): Double = {
    Math.log(x) / Math.log(base)
  }


}
