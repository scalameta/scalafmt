package org.scalafmt.readme

import scalatags.Text.TypedTag

case class Adopter(
    name: String,
    url: String,
    description: Option[String] = None
) {
  import scalatags.Text.all._
  def bullet: TypedTag[String] =
    li(
      a(href := url, name),
      description.fold("")(x => s": $x")
    )
}
object Adopters {

  val adopters = Seq[Adopter](
    Adopter(
      "scalafmt",
      "http://scalafmt.org",
      Some("Code formatter for Scala")
    ),
    Adopter(
      "sbt",
      "https://github.com/sbt/sbt",
      Some("The interactive build tool for Scala")
    ),
    Adopter(
      "scala-native",
      "https://github.com/scala-native/scala-native",
      Some("Your favourite language gets closer to bare metal.")
    ),
    Adopter("HERE", "http://here.com", None),
    Adopter("Letgo", "http://letgo.com", None),
    Adopter(
      "Seventh Sense",
      "http://7thsense.io",
      Some("Predictive analytics for sales and marketing")
    ),
    Adopter(
      "Teralytics",
      "http://teralytics.net",
      Some("We transform raw, human activity data into valuable insights.")
    ),
    Adopter(
      "Venatus Media",
      "https://venatusmedia.com",
      Some(
        "We represent some of the largest entertainment publishers and monetize billions of ads."
      )
    ),
    Adopter(
      "http4s",
      "http://http4s.org",
      Some("A minimal, idiomatic Scala interface for HTTP")
    ),
    Adopter(
      "Mendix",
      "https://mendix.com",
      Some(
        "The fastest and easiest platform to create and continuously improve mobile and web apps at scale."
      )
    ),
    Adopter(
      "Foursquare",
      "https://enterprise.foursquare.com",
      Some(
        "We use location intelligence to build meaningful consumer experiences and business solutions."
      )
    ),
    Adopter(
      "Codacy",
      "https://codacy.com",
      Some(
        "Codacy is an Automated Code Review Tool that monitors your technical debt, helps you improve your code quality, teaches best practices to your developers, and helps you save time in Code Reviews."
      )
    ),
    Adopter(
      "ZyseMe",
      "https://www.zyse.me/",
      Some(
        "We use ML to predict body measurements and automate pattern creation to integrate with production, allowing companies to produce and/or sell made-to-measure garments at scale."
      )
    )
  )
}
