package org.scalafmt

import java.util.concurrent.TimeUnit

import com.ibm.couchdb._
import com.typesafe.config.ConfigFactory
import net.ceedubs.ficus.Ficus._
import org.scalafmt.stats.GitInfo
import org.scalafmt.stats.TestStats

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scalaz.-\/
import scalaz.\/-

object Speed extends ScalaFmtLogger {
  lazy val config = ConfigFactory.load()
  lazy val connectionConfig = {
    import net.ceedubs.ficus.readers.ArbitraryTypeReader._
    config.as[Try[Connection]]("speed")
  }
  val dbName = "scalafmt-teststats"

  val typeMapping = TypeMapping(classOf[TestStats] -> "TestStats")

  def submit(results: Seq[Result],
             suiteName: String): Unit = connectionConfig match {
    case Failure(e) =>
      logger.warn(s"speed.scalafmt.org: ${e.getMessage}")
    case Success(connection) =>
      val stats = TestStats(results)
      Future(invidualReport(stats, connection))
      Future(comparisonReport(stats, "origin/master", connection))
      Report.write(results, suiteName)
  }

  private def invidualReport(stat: TestStats,
                  connection: Connection): Unit = {
    val t = Stopwatch()
    val actions = connection.db.docs.create(stat)
    actions.attemptRun match {
      case -\/(e) =>
        logger.warn("Unable to submit to speed.scalafmt.org", e)
      case f@ \/-(a) =>
        logger.debug(s"Submitted to speed.scalafmt.org (${t.elapsedMs}ms)")
    }
  }

  def comparisonReport(after: TestStats,
                       branch: String,
                       connection: Connection): Unit = {
    import sys.process._
    val t = Stopwatch()
    val commit = Seq("git", "rev-parse", branch).!!.trim

    val view = connection.db.query
      .view[(String, Long), TestStats](branch, "commits").get
      .endKey((commit, 0))
      .startKey((commit, Long.MaxValue))
      .descending(true)
      .limit(1)

    view.query.attemptRun match {
      case -\/(e) =>
        val currentBranch = GitInfo.currentBranch
        logger.warn(
          s"""Found no data for branch $branch? Try this:
              |$$ git checkout $branch
              |$$ sbt test
              |$$ git checkout $currentBranch
              |$$ sbt test""".stripMargin)
      case \/-(CouchKeyVals(_, _, Seq(CouchKeyVal(_, _, before)))) =>
        val report = Report.compare(before, after)
        val filename = "target/compare.html"
        FilesUtil.writeFile(filename, report)
        logger.debug(s"Compare results in $filename (${t.elapsedMs}ms)")
      case \/-(other) =>
        logger.warn(s"Unexpected view result $other ${other.rows}")
    }

  }

  case class Connection(hostname: String,
                        port: Int,
                        https: Boolean,
                        username: String,
                        password: String) {
    val couch = CouchDb(hostname, port, https = https, username, password)
    val db = couch.db(dbName, typeMapping)
  }

}
