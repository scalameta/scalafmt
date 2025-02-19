package org.scalafmt.cli

/** This code is copy/pasted from (Apache 2 licence)
  * https://github.com/alexarchambault/coursier/blob/51fefe5c29d95752ce487f60d333b1f8a91dd1b0/cache/src/main/scala/coursier/TermDisplay.scala
  *
  * which in turn was copy/pasted from (MIT licence)
  * https://github.com/lihaoyi/Ammonite/blob/10854e3b8b454a74198058ba258734a17af32023/terminal/src/main/scala/ammonite/terminal/Utils.scala
  */

import org.scalafmt.sysops.PlatformRunOps

import java.io.File
import java.io.Writer
import java.util.concurrent.atomic

object Terminal {

  private lazy val pathedTput =
    if (new File("/usr/bin/tput").exists()) "/usr/bin/tput" else "tput"

  def consoleDim(s: String): Option[Int] =
    if (System.getenv("TERM") == null) None
    else if (!new File("/dev/tty").exists()) None
    else PlatformRunOps
      .runArgv(Seq("bash", "-c", s"$pathedTput $s 2> /dev/tty"), None)
      .map(_.trim.toInt).toOption

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

private[cli] object TermDisplay extends TermUtils {

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

  private val refreshInterval = 1000 / 60
  private val fallbackRefreshInterval = 1000

  private case class Counts(good: Int, fail: Int, changed: Boolean) {
    def show(
        total: Int,
        okShow: Char,
    )(sbPct: StringBuilder, sbShow: StringBuilder): Unit = {
      def pct(cnt: Int) = 100.0 * cnt / total

      val pctGood = pct(good)
      sbPct.append(f"$pctGood%5.1f%%")

      var idx = 0
      val decGood = (pctGood / 10).toInt
      while (idx < decGood) {
        sbShow.append(okShow)
        idx += 1
      }
      val decDone = (pct(good + fail) / 10).toInt
      while (idx < decDone) {
        sbShow.append('-')
        idx += 1
      }
    }

    def showFallback(total: Int)(implicit sb: StringBuilder): Unit = {
      def pct(cnt: Int) = f"${100.0 * cnt / total}%.1f%%"
      sb.append(" +").append(pct(good)).append("/").append(good).append(" -")
        .append(pct(fail)).append("/").append(fail)
    }
  }

  private class UpdateCounter {
    // completed but not all updated
    private val numGood = new atomic.AtomicInteger(0)
    // failed but not all updated
    private val numFail = new atomic.AtomicInteger(0)
    // already updated
    private val numDone = new atomic.AtomicInteger(-1)

    def done(ok: Boolean): Unit = (if (ok) numGood else numFail).getAndIncrement()

    def get(): Counts = {
      val good = numGood.get()
      val fail = numFail.get()
      val newdone = good + fail
      val done = numDone.getAndSet(newdone)
      Counts(good, fail, done < newdone)
    }
  }

}

private[cli] class TermDisplay(
    out: Writer,
    msg: String,
    todo: Int,
    var fallbackMode: Boolean,
) {

  import TermDisplay._
  import Terminal.Ansi

  private var width = 80

  private val scheduler = new PlatformPollingScheduler
  private var polling: PollingScheduler.Cancelable = _
  private val isStarted = new atomic.AtomicBoolean(false)
  private val counter = new UpdateCounter

  private def shouldUpdate(ending: Boolean): Option[Counts] = {
    val counts = counter.get()
    if (ending || counts.changed) Some(counts) else None
  }

  def end(): Unit = if (isStarted.compareAndSet(true, false)) {
    polling.cancel()
    if (fallbackMode) processUpdateFallback(ending = true)
    else processUpdate(ending = true)
  }

  def done(ok: Boolean): Unit = counter.done(ok)

  private def processUpdateFallback(): Unit =
    processUpdateFallback(ending = false)
  private def processUpdateFallback(ending: Boolean): Unit = shouldUpdate(ending)
    .foreach { counts =>
      implicit val sb = new StringBuilder()
      sb.append("(total ").append(todo)
      sb.append(", done ")
      counts.showFallback(todo)
      sb.append(')')
      val extra = sb.result()
      val url = msg

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

      out.append(url0).append(' ').append(extra0).append('\n').flush()
    }

  private def truncatedPrintln(s: String): Unit = {
    out.clearLine(2)
    if (s.length <= width) out.append(s)
    else out.append(s, 0, width - 1).append('…')
    out.append('\n')
  }

  private def processUpdate(): Unit = processUpdate(ending = false)
  private def processUpdate(ending: Boolean): Unit = shouldUpdate(ending)
    .foreach { counts =>
      val sb = new StringBuilder()
      val sbShow = new StringBuilder()

      sb.append("  ")
      counts.show(todo, '#')(sb, sbShow)
      sb.append(" [").append(sbShow)

      var padding = 10 - sbShow.length
      while (padding > 0) { sb.append(' '); padding -= 1 }
      sb.append("] ").append(todo).append(" source files (failed")
      if (counts.fail > 0) sb.append(' ').append(counts.fail)
      else sb.append(" none")
      sb.append(')')

      truncatedPrintln(msg)
      out.clearLine(2)
      truncatedPrintln(sb.result())

      if (!ending) {
        out.clearLine(2)
        out.down(1)
        out.clearLine(2)
        out.down(1)
        out.up(2)
        out.left(10000)
      }

      out.flush()
    }

  def start(): Unit = if (isStarted.compareAndSet(false, true)) {
    Terminal.consoleDim("cols") match {
      case Some(cols) =>
        width = cols
        out.clearLine(2)
      case None => fallbackMode = true
    }
    polling =
      if (!fallbackMode) scheduler.start(refreshInterval)(processUpdate)
      else {
        out.append(msg).append('\n').flush()
        scheduler.start(fallbackRefreshInterval)(processUpdateFallback)
      }
  }

}
