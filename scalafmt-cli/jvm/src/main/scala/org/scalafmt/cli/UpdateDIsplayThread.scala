package org.scalafmt.cli

import scala.collection.mutable.ArrayBuffer
import java.io.Writer
import scala.annotation.tailrec
import java.util.concurrent.ConcurrentHashMap

import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.TimeUnit

import org.scalafmt.cli.TermDisplay._

protected class UpdateDisplayThread(
    out: Writer,
    var fallbackMode: Boolean
) extends Thread("TermDisplay") {

  private val refreshInterval = 1000 / 60
  private val fallbackRefreshInterval = 1000

  import Terminal.Ansi

  setDaemon(true)

  private var width = 80
  private var currentHeight = 0

  private val q = new LinkedBlockingDeque[Message]

  def update(): Unit = {
    if (q.size() == 0)
      q.put(Message.Update)
  }

  def end(): Unit = {
    q.put(Message.Stop)
    join()
  }

  private val downloads = new ArrayBuffer[String]
  private val doneQueue = new ArrayBuffer[(String, Info)]
  val infos = new ConcurrentHashMap[String, Info]

  def newEntry(
      url: String,
      info: Info,
      fallbackMessage: => String
  ): Unit = {
    assert(!infos.containsKey(url))
    val prev = infos.putIfAbsent(url, info)
    assert(prev == null)

    if (fallbackMode) {
      // FIXME What about concurrent accesses to out from the thread above?
      out.write(fallbackMessage)
      out.flush()
    }

    downloads.synchronized {
      downloads.append(url)
    }

    update()
  }

  def removeEntry(
      url: String,
      success: Boolean,
      fallbackMessage: => String
  )(
      update0: Info => Info
  ): Unit = {
    downloads.synchronized {
      downloads -= url

      val info = infos.remove(url)

      if (success)
        doneQueue += (url -> update0(info))
    }

    if (fallbackMode && success) {
      // FIXME What about concurrent accesses to out from the thread above?
      out.write(fallbackMessage)
      out.flush()
    }

    update()
  }

  private def reflowed(url: String, info: Info) = {
    val extra = info match {
      case downloadInfo: DownloadInfo =>
        val pctOpt = downloadInfo.fraction.map(100.0 * _)

        if (downloadInfo.length.isEmpty && downloadInfo.downloaded == 0L)
          ""
        else {
          val pctOptStr =
            pctOpt.map(pct => f"$pct%.2f %%, ").toIterable.mkString
          val downloadInfoStr = downloadInfo.length.map(" / " + _).mkString
          s"($pctOptStr${downloadInfo.downloaded}$downloadInfoStr)"
        }

      case updateInfo: CheckUpdateInfo =>
        "Checking for updates"
    }

    val baseExtraWidth = width / 5

    val total = url.length + 1 + extra.length
    val (url0, extra0) =
      if (total >= width) { // or > ? If equal, does it go down 2 lines?
        val overflow = total - width + 1

        val extra0 =
          if (extra.length > baseExtraWidth)
            extra.take(
              (baseExtraWidth max (extra.length - overflow)) - 1
            ) + "…"
          else
            extra

        val total0 = url.length + 1 + extra0.length
        val overflow0 = total0 - width + 1

        val url0 =
          if (total0 >= width)
            url.take(
              ((width - baseExtraWidth - 1) max (url.length - overflow0)) - 1
            ) + "…"
          else
            url

        (url0, extra0)
      } else
        (url, extra)

    (url0, extra0)
  }

  private def truncatedPrintln(s: String): Unit = {

    out.clearLine(2)

    if (s.length <= width)
      out.write(s + "\n")
    else
      out.write(s.take(width - 1) + "…\n")
  }

  @tailrec private def updateDisplayLoop(lineCount: Int): Unit = {
    currentHeight = lineCount

    Option(q.poll(100L, TimeUnit.MILLISECONDS)) match {
      case None => updateDisplayLoop(lineCount)
      case Some(Message.Stop) => // poison pill
      case Some(Message.Update) =>
        val (done0, downloads0) = downloads.synchronized {
          val q = doneQueue.toVector
            .filter { case (url, _) =>
              !url.endsWith(".sha1") && !url.endsWith(".md5")
            }
            .sortBy { case (url, _) => url }

          doneQueue.clear()

          val dw = downloads.toVector
            .map { url => url -> infos.get(url) }
            .sortBy { case (_, info) => -info.fraction.sum }

          (q, dw)
        }

        for ((url, info) <- done0 ++ downloads0) {
          assert(info != null, s"Incoherent state ($url)")

          truncatedPrintln(url)
          out.clearLine(2)
          out.write(s"  ${info.display()}\n")
        }

        val displayedCount = (done0 ++ downloads0).length

        if (displayedCount < lineCount) {
          for (_ <- 1 to 2; _ <- displayedCount until lineCount) {
            out.clearLine(2)
            out.down(1)
          }

          for (_ <- displayedCount until lineCount)
            out.up(2)
        }

        for (_ <- downloads0.indices)
          out.up(2)

        out.left(10000)

        out.flush()
        Thread.sleep(refreshInterval)
        updateDisplayLoop(downloads0.length)
    }
  }

  @tailrec private def fallbackDisplayLoop(previous: Set[String]): Unit =
    Option(q.poll(100L, TimeUnit.MILLISECONDS)) match {
      case None => fallbackDisplayLoop(previous)
      case Some(Message.Stop) => // poison pill

        // clean up display
        for (_ <- 1 to 2; _ <- 0 until currentHeight) {
          out.clearLine(2)
          out.down(1)
        }
        for (_ <- 0 until currentHeight) {
          out.up(2)
        }

      case Some(Message.Update) =>
        val downloads0 = downloads.synchronized {
          downloads.toVector
            .map { url => url -> infos.get(url) }
            .sortBy { case (_, info) => -info.fraction.sum }
        }

        var displayedSomething = false
        for ((url, info) <- downloads0 if previous(url)) {
          assert(info != null, s"Incoherent state ($url)")

          val (url0, extra0) = reflowed(url, info)

          displayedSomething = true
          out.write(s"$url0 $extra0\n")
        }

        if (displayedSomething)
          out.write("\n")

        out.flush()
        Thread.sleep(fallbackRefreshInterval)
        fallbackDisplayLoop(previous ++ downloads0.map { case (url, _) =>
          url
        })
    }

  override def run(): Unit = {

    Terminal.consoleDim("cols") match {
      case Some(cols) =>
        width = cols
        out.clearLine(2)
      case None =>
        fallbackMode = true
    }

    if (fallbackMode)
      fallbackDisplayLoop(Set.empty)
    else
      updateDisplayLoop(0)
  }
}
