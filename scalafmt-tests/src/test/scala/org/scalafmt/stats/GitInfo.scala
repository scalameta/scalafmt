package org.scalafmt.stats

import scala.util.Try

// Name and email is anyways registered in the git log.

case class GitInfo(name: String, email: String, branch: String, commit: String)

object GitInfo {

  def exec(args: String*): String = {
    import sys.process._
    Try(args.!!.trim).getOrElse("ERROR")
  }

  lazy val user: String = exec("git", "config", "--get", "user.name")

  lazy val email: String = exec("git", "config", "--get", "user.email")

  val travis: Option[GitInfo] = for {
    isCi <- sys.env.get("CI") if isCi == "true"
  } yield {
    GitInfo("CI", "CI", currentBranch, currentCommit)
  }

  def apply(): GitInfo = travis.getOrElse {
    GitInfo(user, email, currentBranch, currentCommit)
  }

  def currentCommit = exec("git", "rev-parse", "HEAD")

  def currentBranch = exec("git", "rev-parse", "--abbrev-ref", "HEAD")
}
