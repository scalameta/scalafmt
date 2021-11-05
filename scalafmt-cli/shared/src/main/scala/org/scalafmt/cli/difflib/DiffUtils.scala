package org.scalafmt.cli.difflib

import java.util

object DiffUtils {
  def generateUnifiedDiff(
      original: String,
      revised: String,
      originalLines: util.List[String],
      patch: Patch[String],
      contextSize: Int
  ): util.List[String] = {
    if (!patch.getDeltas.isEmpty) {
      val ret: util.List[String] = new util.ArrayList()
      ret.add("--- " + original)
      ret.add("+++ " + revised)
      val patchDeltas: util.List[Delta[String]] =
        new util.ArrayList(patch.getDeltas)
      val deltas: util.List[Delta[String]] = new util.ArrayList()

      var delta = patchDeltas.get(0)
      deltas.add(delta)

      if (patchDeltas.size() > 1) {
        for (i <- 1 until patchDeltas.size) {
          val position = delta.getOriginal.getPosition // store
          // the
          // current
          // position
          // of
          // the first Delta
          // Check if the next Delta is too close to the current
          // position.
          // And if it is, add it to the current set
          val nextDelta = patchDeltas.get(i)
          if (
            (position + delta.getOriginal.size + contextSize) >=
              (nextDelta.getOriginal.getPosition - contextSize)
          ) {
            deltas.add(nextDelta)
          } else { // if it isn't, output the current set,
            // then create a new set and add the current Delta to
            // it.
            val curBlock = processDeltas(originalLines, deltas, contextSize)
            ret.addAll(curBlock)
            deltas.clear()
            deltas.add(nextDelta)
          }
          delta = nextDelta
        }
      }
      val curBlock = processDeltas(originalLines, deltas, contextSize)
      ret.addAll(curBlock)
      ret
    } else {
      new util.ArrayList[String]()
    }
  }
  def diff(
      original: util.List[String],
      revised: util.List[String]
  ): Patch[String] =
    new MyersDiff[String]().diff(original, revised)

  private def processDeltas(
      origLines: util.List[String],
      deltas: util.List[Delta[String]],
      contextSize: Int
  ) = {
    val buffer = new util.ArrayList[String]
    var origTotal = 0 // counter for total lines output from Original
    var revTotal = 0
    var line = 0
    var curDelta = deltas.get(0)
    // NOTE: +1 to overcome the 0-offset Position
    var origStart = curDelta.getOriginal.getPosition + 1 - contextSize
    if (origStart < 1) origStart = 1
    var revStart = curDelta.getRevised.getPosition + 1 - contextSize
    if (revStart < 1) revStart = 1
    // find the start of the wrapper context code
    var contextStart = curDelta.getOriginal.getPosition - contextSize
    if (contextStart < 0) contextStart = 0 // clamp to the start of the file
    // output the context before the first Delta
    line = contextStart
    while ({
      line < curDelta.getOriginal.getPosition
    }) { //
      buffer.add(" " + origLines.get(line))
      origTotal += 1
      revTotal += 1

      line += 1
    }
    // output the first Delta
    buffer.addAll(getDeltaText(curDelta))
    origTotal += curDelta.getOriginal.getLines.size
    revTotal += curDelta.getRevised.getLines.size
    var deltaIndex = 1
    while ({
      deltaIndex < deltas.size
    }) { // for each of the other Deltas
      val nextDelta = deltas.get(deltaIndex)
      val intermediateStart =
        curDelta.getOriginal.getPosition + curDelta.getOriginal.getLines.size
      line = intermediateStart
      while ({
        line < nextDelta.getOriginal.getPosition
      }) { // output the code between the last Delta and this one
        buffer.add(" " + origLines.get(line))
        origTotal += 1
        revTotal += 1

        line += 1
      }
      buffer.addAll(getDeltaText(nextDelta)) // output the Delta

      origTotal += nextDelta.getOriginal.getLines.size
      revTotal += nextDelta.getRevised.getLines.size
      curDelta = nextDelta
      deltaIndex += 1
    }
    // Now output the post-Delta context code, clamping the end of the file
    contextStart =
      curDelta.getOriginal.getPosition + curDelta.getOriginal.getLines.size
    line = contextStart
    while ({
      (line < (contextStart + contextSize)) && (line < origLines.size)
    }) {
      buffer.add(" " + origLines.get(line))
      origTotal += 1
      revTotal += 1

      line += 1
    }
    // Create and insert the block header, conforming to the Unified Diff
    // standard
    val header = new StringBuffer
    header.append("@@ -")
    header.append(origStart)
    header.append(",")
    header.append(origTotal)
    header.append(" +")
    header.append(revStart)
    header.append(",")
    header.append(revTotal)
    header.append(" @@")
    buffer.add(0, header.toString)
    buffer
  }

  private def getDeltaText(delta: Delta[String]) = {
    import scala.collection.JavaConverters._
    val buffer = new util.ArrayList[String]
    for (line <- delta.getOriginal.getLines.asScala) {
      buffer.add("-" + line)
    }
    for (line <- delta.getRevised.getLines.asScala) {
      buffer.add("+" + line)
    }
    buffer
  }

}
