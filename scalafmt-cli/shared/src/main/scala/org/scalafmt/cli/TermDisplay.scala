package org.scalafmt.cli

/** This code is copy/pasted from (Apache 2 licence)
  * https://github.com/alexarchambault/coursier/blob/51fefe5c29d95752ce487f60d333b1f8a91dd1b0/cache/src/main/scala/coursier/TermDisplay.scala
  *
  * which in turn was copy/pasted from (MIT licence)
  * https://github.com/lihaoyi/Ammonite/blob/10854e3b8b454a74198058ba258734a17af32023/terminal/src/main/scala/ammonite/terminal/Utils.scala
  */
import java.io.File
import java.io.Writer
import java.util.concurrent._

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

object Terminal {

  private lazy val pathedTput =
    if (new File("/usr/bin/tput").exists()) "/usr/bin/tput" else "tput"

  def consoleDim(s: String): Option[Int] =
    if (System.getenv("TERM") == null) None
    else if (!new File("/dev/tty").exists()) None
    else {
      import sys.process._
      val nullLog = new ProcessLogger {
        def out(s: => String): Unit = {}
        def err(s: => String): Unit = {}
        def buffer[T](f: => T): T = f
      }
      Try(
        Process(Seq("bash", "-c", s"$pathedTput $s 2> /dev/tty")).!!(nullLog)
          .trim.toInt,
      ).toOption
    }

  implicit class Ansi(val output: Writer) extends AnyVal {
    private def control(n: Int, c: Char) = output.write("\u001b[" + n + c)

    /** Move up `n` squares
      */
    def up(n: Int): Unit = if (n > 0) control(n, 'A')

    /** Move down `n` squares
      */
    def down(n: Int): Unit = if (n > 0) control(n, 'B')

    /** Move left `n` squares
      */
    def left(n: Int): Unit = if (n > 0) control(n, 'D')

    /** Clear the current line
      *
      *   - n=0: clear from cursor to end of line
      *   - n=1: clear from cursor to start of line
      *   - n=2: clear entire line
      */
    def clearLine(n: Int): Unit = control(n, 'K')
  }

}

object TermDisplay extends TermUtils {

  def defaultFallbackMode: Boolean = {
    val env0 = sys.env.get("COURSIER_PROGRESS").map(_.toLowerCase).collect {
      case "true" | "enable" | "1" => true
      case "false" | "disable" | "0" => false
    }
    def compatibilityEnv = sys.env.contains("COURSIER_NO_TERM")

    def nonInteractive = noConsole

    def insideEmacs = sys.env.contains("INSIDE_EMACS")
    def ci = sys.env.contains("CI")

    val env = env0.getOrElse(compatibilityEnv)

    env || nonInteractive || insideEmacs || ci
  }

  private sealed abstract class Info extends Product with Serializable {
    def fraction: Option[Double]
    def display(): String
  }

  private case class DownloadInfo(
      downloaded: Long,
      previouslyDownloaded: Long,
      length: Option[Long],
      startTime: Long,
      updateCheck: Boolean,
  ) extends Info {

    /** 0.0 to 1.0 */
    def fraction: Option[Double] = length.map(downloaded.toDouble / _)

    def display(): String = {
      val decile = (10.0 * fraction.getOrElse(0.0)).toInt
      assert(decile >= 0)
      assert(decile <= 10)

      fraction.fold(" " * 6)(p => f"${100.0 * p}%5.1f%%") + " [" +
        "#" * decile + " " * (10 - decile) + "] " + downloaded +
        " source files formatted"
    }
  }

  private case class CheckUpdateInfo(
      currentTimeOpt: Option[Long],
      remoteTimeOpt: Option[Long],
      isDone: Boolean,
  ) extends Info {
    def fraction = None
    def display(): String =
      if (isDone) (currentTimeOpt, remoteTimeOpt) match {
        case (Some(current), Some(remote)) =>
          if (current < remote) s"Updated since ${formatTimestamp(
              current,
            )} (${formatTimestamp(remote)})"
          else if (current == remote)
            s"No new update since ${formatTimestamp(current)}"
          else s"Warning: local copy newer than remote one (${formatTimestamp(
              current,
            )} > ${formatTimestamp(remote)})"
        case (Some(_), None) =>
          // FIXME Likely a 404 Not found, that should be taken into account by the cache
          "No modified time in response"
        case (None, Some(remote)) => s"Last update: ${formatTimestamp(remote)}"
        case (None, None) => "" // ???
      }
      else currentTimeOpt match {
        case Some(current) =>
          s"Checking for updates since ${formatTimestamp(current)}"
        case None => "" // ???
      }
  }

  private sealed abstract class Message extends Product with Serializable
  private object Message {
    case object Update extends Message
    case object Stop extends Message
  }

  private val refreshInterval = 1000 / 60
  private val fallbackRefreshInterval = 1000

  private class UpdateDisplayThread(out: Writer, var fallbackMode: Boolean)
      extends Thread("TermDisplay") {

    import Terminal.Ansi

    setDaemon(true)

    private var width = 80
    private var currentHeight = 0

    private val q = new LinkedBlockingQueue[Message]

    def update(): Unit = if (q.size() == 0) q.put(Message.Update)

    def end(): Unit = {
      q.put(Message.Stop)
      join()
    }

    private val downloads = new ArrayBuffer[String]
    private val doneQueue = new ArrayBuffer[(String, Info)]
    val infos = new ConcurrentHashMap[String, Info]

    def newEntry(url: String, info: Info, fallbackMessage: => String): Unit = {
      assert(!infos.containsKey(url))
      val prev = infos.putIfAbsent(url, info)
      assert(prev == null)

      if (fallbackMode) {
        // FIXME What about concurrent accesses to out from the thread above?
        out.write(fallbackMessage)
        out.flush()
      }

      downloads.synchronized(downloads.append(url))

      update()
    }

    def removeEntry(url: String, success: Boolean, fallbackMessage: => String)(
        update0: Info => Info,
    ): Unit = {
      downloads.synchronized {
        downloads -= url

        val info = infos.remove(url)

        if (success) doneQueue += url -> update0(info)
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

          if (downloadInfo.length.isEmpty && downloadInfo.downloaded == 0L) ""
          else {
            val pctOptStr = pctOpt.map(pct => f"$pct%.2f %%, ").mkString
            val downloadInfoStr = downloadInfo.length.map(" / " + _).mkString
            s"($pctOptStr${downloadInfo.downloaded}$downloadInfoStr)"
          }

        case _: CheckUpdateInfo => "Checking for updates"
      }

      val baseExtraWidth = width / 5

      val total = url.length + 1 + extra.length
      val (url0, extra0) =
        if (total >= width) { // or > ? If equal, does it go down 2 lines?
          val overflow = total - width + 1

          val extra0 =
            if (extra.length > baseExtraWidth) extra
              .take(baseExtraWidth.max(extra.length - overflow) - 1) + "…"
            else extra

          val total0 = url.length + 1 + extra0.length
          val overflow0 = total0 - width + 1

          val url0 =
            if (total0 >= width) url.take(
              (width - baseExtraWidth - 1).max(url.length - overflow0) - 1,
            ) + "…"
            else url

          (url0, extra0)
        } else (url, extra)

      (url0, extra0)
    }

    private def truncatedPrintln(s: String): Unit = {

      out.clearLine(2)

      if (s.length <= width) out.write(s + "\n")
      else out.write(s.take(width - 1) + "…\n")
    }

    private def getDownloadInfos: Vector[(String, Info)] = downloads.toVector
      .map(url => url -> infos.get(url)).sortBy { case (_, info) =>
        -info.fraction.sum
      }

    @tailrec
    private def updateDisplayLoop(lineCount: Int): Unit = {
      currentHeight = lineCount

      Option(q.poll(100L, TimeUnit.MILLISECONDS)) match {
        case None => updateDisplayLoop(lineCount)
        case Some(Message.Stop) => // poison pill
        case Some(Message.Update) =>
          val (done0, downloads0) = downloads.synchronized {
            val q = doneQueue.toVector.filter { case (url, _) =>
              !url.endsWith(".sha1") && !url.endsWith(".md5")
            }.sortBy { case (url, _) => url }

            doneQueue.clear()

            val dw = getDownloadInfos

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

            for (_ <- displayedCount until lineCount) out.up(2)
          }

          for (_ <- downloads0.indices) out.up(2)

          out.left(10000)

          out.flush()
          Thread.sleep(refreshInterval)
          updateDisplayLoop(downloads0.length)
      }
    }

    @tailrec
    private def fallbackDisplayLoop(previous: Set[String]): Unit =
      Option(q.poll(100L, TimeUnit.MILLISECONDS)) match {
        case None => fallbackDisplayLoop(previous)
        case Some(Message.Stop) => // poison pill
          // clean up display
          for (_ <- 1 to 2; _ <- 0 until currentHeight) {
            out.clearLine(2)
            out.down(1)
          }
          for (_ <- 0 until currentHeight) out.up(2)

        case Some(Message.Update) =>
          val downloads0 = downloads.synchronized(getDownloadInfos)

          var displayedSomething = false
          for ((url, info) <- downloads0 if previous(url)) {
            assert(info != null, s"Incoherent state ($url)")

            val (url0, extra0) = reflowed(url, info)

            displayedSomething = true
            out.write(s"$url0 $extra0\n")
          }

          if (displayedSomething) out.write("\n")

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
        case None => fallbackMode = true
      }

      if (fallbackMode) fallbackDisplayLoop(Set.empty) else updateDisplayLoop(0)
    }
  }

}

object Cache {
  trait Logger {
    def startTask(url: String, file: File): Unit = {}
    def taskProgress(url: String): Unit = {}
    def completedTask(url: String, success: Boolean): Unit = {}
    def checkingUpdates(url: String, currentTimeOpt: Option[Long]): Unit = {}
  }
}

class TermDisplay(
    out: Writer,
    val fallbackMode: Boolean = TermDisplay.defaultFallbackMode,
) extends Cache.Logger {

  import TermDisplay._

  private val counter = new atomic.AtomicInteger()
  private val updateThread = new UpdateDisplayThread(out, fallbackMode)

  def init(): Unit = updateThread.start()

  def stop(): Unit = updateThread.end()

  override def startTask(msg: String, file: File): Unit = updateThread.newEntry(
    msg,
    DownloadInfo(0L, 0L, None, System.currentTimeMillis(), updateCheck = false),
    s"$msg\n",
  )

  def taskLength(
      url: String,
      totalLength: Long,
      alreadyDownloaded: Long,
  ): Unit = {
    val info = updateThread.infos.get(url)
    assert(info != null)
    val newInfo = info match {
      case info0: DownloadInfo => info0.copy(
          length = Some(totalLength),
          previouslyDownloaded = alreadyDownloaded,
        )
      case _ => throw new Exception(s"Incoherent display state for $url")
    }
    updateThread.infos.put(url, newInfo)

    updateThread.update()
  }
  override def taskProgress(url: String): Unit = {
    val downloaded = counter.incrementAndGet()
    val info = updateThread.infos.get(url)
    if (info != null) { // We might not want the progress bar.
      val newInfo = info match {
        case info0: DownloadInfo => info0.copy(downloaded = downloaded)
        case _ => throw new Exception(s"Incoherent display state for $url")
      }
      updateThread.infos.put(url, newInfo)

      updateThread.update()
    }
  }

  override def completedTask(url: String, success: Boolean): Unit = updateThread
    .removeEntry(url, success, s"$url\n")(x => x)

  override def checkingUpdates(
      url: String,
      currentTimeOpt: Option[Long],
  ): Unit = updateThread.newEntry(
    url,
    CheckUpdateInfo(currentTimeOpt, None, isDone = false),
    s"$url\n",
  )

}
