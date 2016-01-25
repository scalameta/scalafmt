package org.scalafmt

import scalatags.Text.all._

object Report {
  def generate(results: Seq[Result]): String = {
    html(
      head(
      ),
      body(
        div(
          h1(id := "title", "Heatmap"),
          for (result <- results.sortBy(-_.redness)
               if result.test.name != "Warmup") yield {
            div(
              h2(result.test.name),
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

}
