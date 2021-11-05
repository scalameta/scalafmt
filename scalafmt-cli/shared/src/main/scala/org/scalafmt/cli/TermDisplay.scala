package org.scalafmt.cli

/** This code is copy/pasted from (Apache 2 licence)
  * https://github.com/alexarchambault/coursier/blob/51fefe5c29d95752ce487f60d333b1f8a91dd1b0/cache/src/main/scala/coursier/TermDisplay.scala
  *
  * which in turn was copy/pasted from (MIT licence)
  * https://github.com/lihaoyi/Ammonite/blob/10854e3b8b454a74198058ba258734a17af32023/terminal/src/main/scala/ammonite/terminal/Utils.scala
  */
import scala.util.Try

import java.io.File
import java.io.Writer
import java.io.BufferedReader
import java.io.InputStreamReader

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
      Try {
        val processBuilder = new java.lang.ProcessBuilder()
        processBuilder.command(
          Seq("bash", "-c", s"$pathedTput $s 2> /dev/tty"): _*
        );
        val process = processBuilder.start()

        new BufferedReader(new InputStreamReader(process.getInputStream()))
          .readLine()
          .toInt
      }.toOption
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
      * n=0: clear from cursor to end of line n=1: clear from cursor to start of
      * line n=2: clear entire line
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
    def compatibilityEnv = sys.env.get("COURSIER_NO_TERM").nonEmpty

    def nonInteractive = noConsole

    def insideEmacs = sys.env.contains("INSIDE_EMACS")
    def ci = sys.env.contains("CI")

    val env = env0.getOrElse(compatibilityEnv)

    env || nonInteractive || insideEmacs || ci
  }

  private[cli] sealed abstract class Info extends Product with Serializable {
    def fraction: Option[Double]
    def display(): String
  }

  private[cli] case class DownloadInfo(
      downloaded: Long,
      previouslyDownloaded: Long,
      length: Option[Long],
      startTime: Long,
      updateCheck: Boolean
  ) extends Info {

    /** 0.0 to 1.0 */
    def fraction: Option[Double] = length.map(downloaded.toDouble / _)

    /** Byte / s */
    def rate(): Option[Double] = {
      val currentTime = System.currentTimeMillis()
      if (currentTime > startTime)
        Some(
          (downloaded - previouslyDownloaded).toDouble /
            (System.currentTimeMillis() - startTime) * 1000.0
        )
      else
        None
    }

    // Scala version of http://stackoverflow.com/questions/3758606/how-to-convert-byte-size-into-human-readable-format-in-java/3758880#3758880
    private def byteCount(bytes: Long, si: Boolean = false): String = {
      val unit = if (si) 1000 else 1024
      if (bytes < unit)
        bytes + " B"
      else {
        val exp = (math.log(bytes) / math.log(unit)).toInt
        val pre =
          (if (si) "kMGTPE"
           else "KMGTPE").charAt(exp - 1) +
            (if (si) ""
             else "i")
        f"${bytes / math.pow(unit, exp)}%.1f ${pre}B"
      }
    }

    def display(): String = {
      val decile = (10.0 * fraction.getOrElse(0.0)).toInt
      assert(decile >= 0)
      assert(decile <= 10)

      fraction.fold(" " * 6)(p => f"${100.0 * p}%5.1f%%") +
        " [" + ("#" * decile) + (" " * (10 - decile)) + "] " +
        downloaded + " source files formatted"
    }
  }

  private[cli] case class CheckUpdateInfo(
      currentTimeOpt: Option[Long],
      remoteTimeOpt: Option[Long],
      isDone: Boolean
  ) extends Info {
    def fraction = None
    def display(): String = {
      if (isDone)
        (currentTimeOpt, remoteTimeOpt) match {
          case (Some(current), Some(remote)) =>
            if (current < remote)
              s"Updated since ${formatTimestamp(current)} (${formatTimestamp(remote)})"
            else if (current == remote)
              s"No new update since ${formatTimestamp(current)}"
            else
              s"Warning: local copy newer than remote one (${formatTimestamp(current)} > ${formatTimestamp(remote)})"
          case (Some(_), None) =>
            // FIXME Likely a 404 Not found, that should be taken into account by the cache
            "No modified time in response"
          case (None, Some(remote)) =>
            s"Last update: ${formatTimestamp(remote)}"
          case (None, None) =>
            "" // ???
        }
      else
        currentTimeOpt match {
          case Some(current) =>
            s"Checking for updates since ${formatTimestamp(current)}"
          case None =>
            "" // ???
        }
    }
  }

  private[cli] sealed abstract class Message extends Product with Serializable
  private[cli] object Message {
    case object Update extends Message
    case object Stop extends Message
  }

}

object Cache {
  trait Logger {
    def foundLocally(url: String, f: File): Unit = {}
    def startTask(url: String, file: File): Unit = {}
    def taskProgress(url: String, downloaded: Long): Unit = {}
    def completedTask(url: String, success: Boolean): Unit = {}
    def checkingUpdates(url: String, currentTimeOpt: Option[Long]): Unit = {}
  }
}

class TermDisplay(
    out: Writer,
    val fallbackMode: Boolean = TermDisplay.defaultFallbackMode
) extends Cache.Logger {

  import TermDisplay._

  private val updateThread = new UpdateDisplayThread(out, fallbackMode)

  def init(): Unit = {
    updateThread.start()
  }

  def stop(): Unit = {
    updateThread.end()
  }

  override def startTask(msg: String, file: File): Unit =
    updateThread.newEntry(
      msg,
      DownloadInfo(
        0L,
        0L,
        None,
        System.currentTimeMillis(),
        updateCheck = false
      ),
      s"$msg\n"
    )

  def taskLength(
      url: String,
      totalLength: Long,
      alreadyDownloaded: Long
  ): Unit = {
    val info = updateThread.infos.get(url)
    assert(info != null)
    val newInfo = info match {
      case info0: DownloadInfo =>
        info0.copy(
          length = Some(totalLength),
          previouslyDownloaded = alreadyDownloaded
        )
      case _ =>
        throw new Exception(s"Incoherent display state for $url")
    }
    updateThread.infos.put(url, newInfo)

    updateThread.update()
  }
  override def taskProgress(url: String, downloaded: Long): Unit = {
    val info = updateThread.infos.get(url)
    if (info != null) { // We might not want the progress bar.
      val newInfo = info match {
        case info0: DownloadInfo =>
          info0.copy(downloaded = downloaded)
        case _ =>
          throw new Exception(s"Incoherent display state for $url")
      }
      updateThread.infos.put(url, newInfo)

      updateThread.update()
    }
  }

  override def completedTask(url: String, success: Boolean): Unit =
    updateThread.removeEntry(url, success, s"$url\n")(x => x)

  override def checkingUpdates(
      url: String,
      currentTimeOpt: Option[Long]
  ): Unit =
    updateThread.newEntry(
      url,
      CheckUpdateInfo(currentTimeOpt, None, isDone = false),
      s"$url\n"
    )

}
