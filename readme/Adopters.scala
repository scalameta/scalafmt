package org.scalafmt.readme

import scalatags.Text.TypedTag

case class Adopter(name: String,
                   url: String,
                   description: Option[String] = None) {
  import scalatags.Text.all._
  def bullet: TypedTag[String] = li(
    a(href := url, name),
    description.fold("")(x => s": $x")
  )
}
object Adopters {

  val adopters = Seq[Adopter](
    Adopter("scalafmt",
            "http://scalafmt.org",
            Some("Code formatter for Scala")),
    Adopter("HERE", "http://here.com", None),
    Adopter("Letgo", "http://letgo.com", None),
    Adopter("Seventh Sense",
            "http://7thsense.io",
            Some("Predictive analytics for sales and marketing")),
    Adopter(
      "Teralytics",
      "http://teralytics.net",
      Some("We transform raw, human activity data into valuable insights."))
  )
}
