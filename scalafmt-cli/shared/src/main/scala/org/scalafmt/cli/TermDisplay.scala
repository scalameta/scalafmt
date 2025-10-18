package org.scalafmt.cli

/** This code is copy/pasted from (Apache 2 licence)
  * https://github.com/alexarchambault/coursier/blob/51fefe5c29d95752ce487f60d333b1f8a91dd1b0/cache/src/main/scala/coursier/TermDisplay.scala
  *
  * which in turn was copy/pasted from (MIT licence)
  * https://github.com/lihaoyi/Ammonite/blob/10854e3b8b454a74198058ba258734a17af32023/terminal/src/main/scala/ammonite/terminal/Utils.scala
  */

import org.scalafmt.sysops.PlatformRunOps

import java.io.{File, Writer}
import java.util.concurrent.atomic

object Terminal {

  private lazy val pathedTput =
    if (new File("/usr/bin/tput").exists()) "/usr/bin/tput" else "tput"

  def consoleDim(s: String): Option[Int] =
    if (System.getenv("TERM") == null) None
    else if (!new File("/dev/tty").exists()) None
    else PlatformRunOps
      .runArgv(Seq("bash", "-c", s"$pathedTput $s 2> /dev/tty"), None)
      .map(_.headOption.map(_.trim.toInt)).toOption.flatten

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

  private def fill(cnt: Int, ch: Char)(implicit sb: StringBuilder): Unit = {
    var idx = 0
    while (idx < cnt) {
      idx += 1
      sb.append(ch)
    }
  }

  private case class Counts(good: Int, fail: Int, changed: Boolean) {
    def done = good + fail
    def show(
        okShow: Char,
        label: String,
        prevStage: Option[Counts],
        nextStage: Option[Counts],
    )(total: Int, sbRest: StringBuilder)(implicit sbShow: StringBuilder): Unit = {
      def dec(cnt: Int) = 10 * cnt / total
      val decDone = dec(done) - nextStage.fold(0)(x => dec(x.done))
      val decGood = dec(good) - nextStage.fold(0)(x => dec(x.good))
      val decFail = decDone - decGood
      if (decFail <= 0) fill(decDone, okShow)
      else { fill(decGood, okShow); fill(decFail, '-') }

      sbRest.append(f"$label ${100.0 * good / total}%.1f%% $good")
      val stageFail = fail - prevStage.fold(0)(_.fail)
      if (stageFail > 0) sbRest.append('/').append(stageFail)
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
  private val readCounter = new UpdateCounter
  private val formatCounter = new UpdateCounter
  private val writeCounter = new UpdateCounter

  def end(): Unit = if (isStarted.compareAndSet(true, false)) {
    polling.cancel()
    processUpdate(ending = true)
  }

  def doneRead(ok: Boolean): Unit = readCounter.done(ok)
  def doneFormat(ok: Boolean): Unit = formatCounter.done(ok)
  def doneWrite(ok: Boolean): Unit = writeCounter.done(ok)

  private def truncatedPrintln(s: String): Unit = {
    out.clearLine(2)
    if (s.length <= width) out.append(s)
    else out.append(s, 0, width - 1).append('…')
    out.append('\n')
  }

  private def processUpdate(): Unit = processUpdate(ending = false)
  private def processUpdate(ending: Boolean): Unit = {
    val wcounts = writeCounter.get()
    val fcounts = formatCounter.get()
    val rcounts = readCounter.get()
    val ok = ending || rcounts.changed || fcounts.changed || wcounts.changed
    if (!ok) return

    implicit val sb = new StringBuilder()
    val sbRest = new StringBuilder()

    val useFirstLine = msg.length + 13 <= width
    val sbSecondLine = if (useFirstLine) sbRest else sb
    sbSecondLine.append("  ")

    if (useFirstLine) sb.append(msg).append(' ')
    sb.append('[')
    val sbLen = sb.length
    wcounts.show('#', "out", Some(fcounts), None)(todo, sbRest)
    fcounts.show('>', ", fmt", Some(rcounts), Some(wcounts))(todo, sbRest)
    rcounts.show('+', ", in", None, Some(fcounts))(todo, sbRest)
    fill(sbLen + 10 - sb.length, ' ')
    sb.append(']')

    truncatedPrintln(if (useFirstLine) sb.result() else msg)
    out.clearLine(2)

    if (sbSecondLine ne sbRest) sbSecondLine.append(' ').append(sbRest)
    val left = todo - rcounts.done
    if (left > 0) sbSecondLine.append(" todo=").append(left)
    truncatedPrintln(sbSecondLine.result())

    if (!ending && !fallbackMode) {
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
    if (fallbackMode) out.append(msg).append('\n').flush()
    val freq = if (fallbackMode) fallbackRefreshInterval else refreshInterval
    polling = scheduler.start(freq)(processUpdate)
  }

}
