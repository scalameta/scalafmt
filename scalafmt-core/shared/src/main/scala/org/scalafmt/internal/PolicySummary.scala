package org.scalafmt.internal

import org.scalafmt.util.LoggerOps

import scala.meta.classifiers._
import scala.meta.tokens.{Token => T}

class PolicySummary(policies: Array[Policy], val numPolicies: Int) {
  import LoggerOps._

  @inline
  def noDequeue: Boolean = exists(_.noDequeue)

  def combine(split: Split, nextft: FT): PolicySummary =
    if (nextft.right.is[T.EOF]) PolicySummary.empty
    else {
      // `policies` is kept sorted by rank; filtering expired entries preserves
      // that order, so we only need to (stable-)sort when adding a new policy.
      val res = new Array[Policy](1 + numPolicies)
      def add(p: Policy, idx: Int): Int = {
        val u = p.unexpired(split, nextft)
        if (u eq Policy.NoPolicy) idx else { res(idx) = u; idx + 1 }
      }
      val added = add(split.policy, 0)
      var ridx = 0
      var widx = added
      while (ridx < numPolicies) {
        widx = add(policies(ridx), widx)
        ridx += 1
      }
      if (added > 0 && widx > 1) java.util.Arrays
        .sort(res, 0, widx, PolicySummary.byRank)
      new PolicySummary(res, widx)
    }

  def execute(
      ft: FT,
      splits: Seq[Split],
      debug: Boolean = false,
  ): Seq[Split] = {
    val result = Decision(ft, splits)
    var idx = 0
    while (idx < numPolicies) {
      val policy = policies(idx)
      idx += 1
      val splits = policy.f.applyOrElse(result, (_: Decision) => null)
      if (splits ne null) {
        if (debug) logger.debug(s"$policy defined at $result")
        result.splits = splits
      }
    }
    result.splits
  }

  def exists(f: Policy => Boolean): Boolean = {
    var idx = 0
    while (idx < numPolicies) {
      if (f(policies(idx))) return true
      idx += 1
    }
    false
  }

  def toIterable: Iterable[Policy] = policies.view.take(numPolicies)
}

object PolicySummary {
  val empty = new PolicySummary(Array.empty, 0)

  private val byRank: java.util.Comparator[Policy] =
    (a, b) => Integer.compare(a.rank, b.rank)
}
