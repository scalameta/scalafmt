import sbt._
import sbt.Keys._

object Dependencies {
  val scalametaV = "1.6.0"
  val scalatestV = "3.0.1"
  val scalariform = "0.1.8"
  val coursier = "1.0.0-M15-5"

  val scalameta = "org.scalameta"        %% "scalameta" % scalametaV
  val scalametaTestkit = "org.scalameta" %% "testkit"   % scalametaV
  val scalatest = "org.scalatest"        %% "scalatest" % scalatestV
}
