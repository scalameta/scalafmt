package org.scalafmt.stats

// Name and email is anyways registered in the git log.
case class GitInfo(name: String, email: String, branch: String, commit: String)

object GitInfo {

  import sys.process._

  lazy val user = Seq("git", "config", "--get", "user.name").!!.trim

  lazy val email = Seq("git", "config", "--get", "user.email").!!.trim

  def apply(): GitInfo = GitInfo(user, email, currentBranch, currentCommit)

  def currentCommit = Seq("git", "rev-parse", "HEAD").!!.trim

  def currentBranch = Seq("git", "rev-parse", "--abbrev-ref", "HEAD").!!.trim

}

