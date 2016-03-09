package org.scalafmt.stats

// Name and email is anyways registered in the git log.

case class GitInfo(name: String, email: String, branch: String, commit: String)

object GitInfo {
  import sys.process._

  lazy val user = Seq("git", "config", "--get", "user.name").!!.trim

  lazy val email = Seq("git", "config", "--get", "user.email").!!.trim

  val travis: Option[GitInfo] = for {
    isTravis <- sys.env.get("TRAVIS") if isTravis == "true"
    isCi <- sys.env.get("CONTINUOUS_INTEGRATION") if isCi == "true"
    commit <- sys.env.get("TRAVIS_COMMIT")
  } yield {
    val branch =
      sys.env.get("TRAVIS_PULL_REQUEST").withFilter(_ != "false").map(x =>
        s"pull_request/$x").orElse(sys.env.get("TRAVIS_BRANCH"))
        .getOrElse("unknown")
    GitInfo("Travis", "Travis", branch, commit)
  }

  def apply(): GitInfo =
    travis.getOrElse {
      GitInfo(user, email, currentBranch, currentCommit)
    }

  def currentCommit = Seq("git", "rev-parse", "HEAD").!!.trim

  def currentBranch = Seq("git", "rev-parse", "--abbrev-ref", "HEAD").!!.trim
}
