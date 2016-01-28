package org.scalafmt

import org.scalafmt.stats.TestStats

import scalatags.Text
import scalatags.Text.all._

object Report {

  def heatmap(results: Seq[Result]): String = {
    html(
      head(
      ),
      body(
        div(
          h1(id := "title", "Heatmap"),
          for (result <- results.sortBy(-_.maxVisitsOnSingleToken)
               if result.test.name != "Warmup") yield {
            div(
              h2(result.title),
              pre(
                code(
                  raw(result.obtainedHtml)
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
      h1(id := "title", s"Before=${after.gitInfo.branch}," +
        s"after=${before.gitInfo.branch}" +
        s" (${before.shortCommit}...${after.shortCommit})"),
      after.intersectResults(before).sortBy {
        case (aft, _) =>
          -aft.maxVisitsOnSingleToken
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
                td(bef.visitedStates - aft.visitedStates)
              )
            ),
            pre(
              code(
                raw( mkHtml(mergeResults(aft, bef)) )
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

  def reportBody(xs: Text.Modifier*) =
    html(
      body(xs: _*)
    )

  def mkHtml(output: Seq[FormatOutput]): String = {
    val sb = new StringBuilder()
    output.foreach { x =>
      import scalatags.Text.all._
      val color = red(x.visits)
      val html = span(
        background := s"rgb(256, $color, $color)",
        x.token).render
      sb.append(html)
      sb.append(x.whitespace)
    }
    sb.toString()
  }

  def red(visits: Int): Int = {
    val max = 10
    val i = Math.min(max, visits)
    val k = (i.toDouble / max.toDouble * 256).toInt
    Math.min(256, 270 - k)
  }


}
