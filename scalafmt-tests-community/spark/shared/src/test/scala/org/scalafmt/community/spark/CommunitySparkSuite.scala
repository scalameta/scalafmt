package org.scalafmt.community.spark

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunitySparkSuite(name: String)
    extends CommunityRepoSuite("https://github.com/apache/spark.git", name)

class CommunitySpark3_4Suite extends CommunitySparkSuite("spark-3.4") {

  override protected def totalStatesVisited: Option[Int] = Some(86403761)

  override protected def builds = Seq(getBuild("v3.4.1", dialects.Scala213, 2585))

}

class CommunitySpark3_5Suite extends CommunitySparkSuite("spark-3.5") {

  override protected def totalStatesVisited: Option[Int] = Some(91395500)

  override protected def builds = Seq(getBuild("v3.5.3", dialects.Scala213, 2756))

}
