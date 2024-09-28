package org.scalafmt.community.spark

import org.scalafmt.community.common.CommunityRepoSuite

import scala.meta._

abstract class CommunitySparkSuite(name: String)
    extends CommunityRepoSuite("https://github.com/apache/spark.git", name)

class CommunitySpark1_6Suite extends CommunitySparkSuite("spark-1.6") {

  override protected def builds =
    Seq(getBuild("v1.6.3", dialects.Scala213, 1444))

}

class CommunitySpark2_4Suite extends CommunitySparkSuite("spark-2.4") {

  override protected def builds =
    Seq(getBuild("v2.4.8", dialects.Scala213, 1931))

}

class CommunitySpark3_4Suite extends CommunitySparkSuite("spark-3.4") {

  override protected def builds =
    Seq(getBuild("v3.4.1", dialects.Scala213, 2585))

}

class CommunitySpark3_5Suite extends CommunitySparkSuite("spark-3.5") {

  override protected def builds =
    Seq(getBuild("v3.5.3", dialects.Scala213, 2756))

}
