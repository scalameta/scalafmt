// declared in org.scalafmt.cli to subclass ScalafmtRunner (its `run` is
// private[cli]) and reuse the real getInputMethods/runInputs.
package org.scalafmt.cli

import org.scalafmt.{Formatted, Scalafmt => CoreScalafmt, Versions}
import org.scalafmt.config.{ProjectFiles, ScalafmtConfig}
import org.scalafmt.sysops.{AbsoluteFile, PlatformFileOps, PlatformRunOps}

import scala.meta.{Dialect, Source, dialects, XtensionParseInputLike}

import java.nio.file.{Files, Path, Paths}
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.{Callable, ExecutorService, Executors, TimeUnit}

import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContextExecutorService, Future}
import scala.concurrent.duration.Duration

import metaconfig.Configured
import org.openjdk.jmh.annotations._

/** Benchmarks the CLI work AROUND formatting: file discovery, reading, writing,
  * and the `runInputs` orchestration (futures + thread pools). The formatter
  * itself is replaced by identity, so what remains is exactly "the stuff
  * outside formatting" that [[MicroBenchmark]] deliberately excludes.
  *
  * Methods decompose the wrapper:
  *   - `discover` — run the finder (recursive walk + path matching)
  *   - `read` — read every discovered file (async NIO), as the pipeline does
  *   - `write` — write every file's cached content to a temp dir (write I/O)
  *   - `pipeline` — the real `runInputs` with an identity formatter: read +
  *     orchestration + change-compare (write is skipped since nothing changes,
  *     i.e. the common "already-formatted" steady state). `pipeline - read`
  *     is the orchestration overhead.
  *
  * Read-only w.r.t. the corpus (writes target a temp dir). Corpus defaults to
  * this repo's `scalafmt-core` sources; point at a real repo with
  * `-Dscalafmt.bench.corpus=<dir>` (e.g. ../scala-js) and cap with
  * `-Dscalafmt.bench.maxFiles=<n>`.
  */
@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime)) @OutputTimeUnit(TimeUnit.MILLISECONDS)
class WrapperBenchmark extends ScalafmtRunner {

  // unused: only here to satisfy the abstract private[cli] member
  override private[cli] def run(o: CliOptions, m: String): Future[ExitCode] =
    ExitCode.Ok.future

  @Param(Array("false", "true"))
  var async: String = _

  // recursive = walk the tree (Files.find); git = `git ls-files` subprocess.
  // Real big repos set project.git=true, so `git` is their real discovery path.
  @Param(Array("recursive", "git"))
  var fetch: String = _

  // `newlines.source`: real repos use keep (scala-js) / fold (scalafmt,
  // scalameta); classic is the default full-reflow. Drives the search cost for
  // pipelineFormat, so parse-vs-search can be split at a realistic style.
  @Param(Array("keep", "fold", "classic"))
  var style: String = _

  // thread count for formatContentsPar (the parallel-scaling curve)
  @Param(Array("10"))
  var threads: String = _

  private implicit val ec: scala.concurrent.ExecutionContext =
    PlatformRunOps.parasiticExecutionContext

  private var options: CliOptions = _
  private var cfg: ScalafmtConfig = _
  private implicit var dialect: Dialect = _
  private var matcher: Path => Boolean = _
  private var inputs: Seq[InputMethod] = _
  private var contents: Seq[(Path, String)] = _ // for the write bench
  private var writeDir: Path = _
  private var parPool: ExecutorService = _ // reused; pool churn would skew fast ops

  @Setup(Level.Trial)
  def setup(): Unit = {
    val corpus = WrapperBenchmark.resolveCorpus()
    val isGit = fetch == "git"
    // git mode: discovery is `git ls-files` (git already omits target/untracked).
    // recursive mode: walk the tree; exclude generated/vcs via filters.
    val projectLine =
      if (isGit) "project.git = true"
      else """project.excludeFilters = ["/target/", "/\\.git/"]"""
    val sourceLine =
      if (style == "classic") "" else s"newlines.source = $style\n"
    val conf = Files.createTempFile("scalafmt-wrapper", ".scalafmt.conf")
    Files.write(
      conf,
      s"""|version = ${Versions.stable}
          |runner.dialect = scala213
          |$sourceLine$projectLine
          |""".stripMargin.getBytes("UTF-8"),
    )
    // git mode discovers all tracked files (no positional); recursive walks the
    // given dir
    val target = if (isGit) Seq.empty else Seq(corpus.toString)
    val args = target ++ Seq(
      "--config",
      conf.toString,
      "--quiet",
      "--non-interactive",
      "--test",
    ) ++ (if (async == "true") Seq("--async-format") else Seq.empty)
    val parsed = Cli.getConfig(CliOptions.default, args: _*)
      .getOrElse(sys.error("bad cli options"))
    // git ls-files must run in the repo, so point the CLI's cwd there
    options =
      if (isGit) parsed
        .copy(common = parsed.common.copy(cwd = Some(AbsoluteFile(corpus))))
      else parsed

    cfg = options.scalafmtConfig match {
      case Configured.Ok(c) => c
      case Configured.NotOk(e) => sys.error(e.msg)
    }
    dialect = dialects.Scala213
    matcher = ProjectFiles.FileMatcher(cfg.project, options.customExcludes)
      .matchesPath
    // Cap count and per-file size: real trees have a few 100+KB files whose
    // cold best-first search dwarfs everything and prevents jmh from warming
    // up. maxBytes keeps the set to typical files; discover/read/write are
    // unaffected by size, but pipelineFormat needs a bounded, uniform set.
    val maxFiles = sys.props.get("scalafmt.bench.maxFiles").map(_.toInt)
      .getOrElse(Int.MaxValue)
    val maxBytes = sys.props.get("scalafmt.bench.maxBytes").map(_.toLong)
      .getOrElse(Long.MaxValue)
    inputs = getInputMethods(options, matcher)
      .filter(im => Files.size(im.path) <= maxBytes).take(maxFiles)
    require(inputs.nonEmpty, s"no files under $corpus")

    contents = Await.result(
      Future.sequence(inputs.map(im => im.readInput(options).map(im.path -> _))),
      Duration.Inf,
    )
    writeDir = Files.createTempDirectory("scalafmt-wrapper-out")
    parPool = Executors.newFixedThreadPool(threads.toInt)
  }

  // the read/write pools are non-daemon; without this the forked VM can't exit
  @TearDown(Level.Trial)
  def teardown(): Unit = {
    Seq(
      PlatformRunOps.inputExecutionContext,
      PlatformRunOps.outputExecutionContext,
      PlatformRunOps.formatExecutionContext,
    ).foreach {
      case es: ExecutionContextExecutorService => es.shutdownNow()
      case _ =>
    }
    if (parPool ne null) parPool.shutdownNow()
  }

  @Benchmark
  def discover(): Int = getInputMethods(options, matcher).size

  @Benchmark
  def read(): Int = Await.result(
    Future.sequence(inputs.map(_.readInput(options).map(_.length))),
    Duration.Inf,
  ).sum

  @Benchmark
  def write(): Int = {
    val fs = contents.iterator.zipWithIndex.map { case ((_, text), i) =>
      PlatformFileOps
        .writeFileAsync(writeDir.resolve(s"f$i.scala"), text)(options.encoding)
    }
    Await.result(Future.sequence(fs.toSeq), Duration.Inf)
    contents.size
  }

  /** wrapper only: read + orchestration + compare, no parse, no format */
  @Benchmark
  def pipeline(): ExitCode = Await.result(
    runInputs(options, inputs, "bench") { case (code, _) => Right(code) },
    Duration.Inf,
  )

  /** wrapper + scalameta parse (no format). `pipelineParse - pipeline` = parse */
  @Benchmark
  def pipelineParse(): ExitCode = Await.result(
    runInputs(options, inputs, "bench") { case (code, _) =>
      code.parse[Source]; Right(code)
    },
    Duration.Inf,
  )

  /** wrapper + parse + real format. Returns Right(code) (original) so write &
    * the O(n·m) --test unified diff are skipped — matching the raw-pool
    * baselines, which discard the formatted result. `pipelineFormatDiff` adds
    * the diff back. */
  @Benchmark
  def pipelineFormat(): ExitCode = Await.result(
    runInputs(options, inputs, "bench") { case (code, path) =>
      CoreScalafmt.formatCode(code, cfg, options.range, path.toString).formatted
      Right(code)
    },
    Duration.Inf,
  )

  /** as pipelineFormat but returns the formatted code, so --test computes a
    * unified diff per changed file. pipelineFormatDiff - pipelineFormat = diff */
  @Benchmark
  def pipelineFormatDiff(): ExitCode = Await.result(
    runInputs(options, inputs, "bench") { case (code, path) =>
      CoreScalafmt.formatCode(code, cfg, options.range, path.toString)
        .formatted match {
        case x: Formatted.Success => Right(x.formattedCode)
        case _ => Right(code)
      }
    },
    Duration.Inf,
  )

  private def fmt(path: Path, code: String): Unit = {
    CoreScalafmt.formatCode(code, cfg, options.range, path.toString).formatted
    ()
  }

  /** format pre-read contents on ONE thread — baseline for formatContentsPar,
    * with no read I/O or runInputs in the way. */
  @Benchmark
  def formatContentsSeq(): Int = {
    contents.foreach { case (p, c) => fmt(p, c) }
    contents.size
  }

  /** format the same pre-read contents on a raw 10-thread pool. If this hits
    * ~Ncores while runInputs' pipeline doesn't, the CLI orchestration is the
    * bottleneck; if this is ALSO ~1×, Scalafmt.formatCode serializes internally.
    */
  @Benchmark
  def formatContentsPar(): Int = {
    val futs = contents.map { case (p, c) =>
      parPool.submit(new Callable[Unit] { def call(): Unit = fmt(p, c) })
    }
    futs.foreach(_.get())
    contents.size
  }

  /** read (real readInput) + format on a raw `threads`-pool — same work as
    * runInputs but with clean orchestration. If this scales like
    * formatContentsPar but pipelineFormat doesn't, runInputs' orchestration is
    * the culprit; if this ALSO stalls, the async-read machinery is.
    */
  @Benchmark
  def readFormatContentsPar(): Int = {
    val futs = inputs.map { im =>
      parPool.submit(new Callable[Unit] {
        def call(): Unit =
          fmt(im.path, Await.result(im.readInput(options), Duration.Inf))
      })
    }
    futs.foreach(_.get())
    inputs.size
  }

  /** launch ALL reads up front (as runInputs does, flooding the input pool),
    * then format on a clean `threads`-pool. If this drops toward runInputs'
    * ~1.15× while readFormatContentsPar (bounded reads) stays ~2×, the up-front
    * read fan-out (chunked async I/O stealing cores) is runInputs' problem.
    */
  @Benchmark
  def readAllThenFormatPar(): Int = {
    val reads = inputs.map(im => (im.path, im.readInput(options)))
    val futs = reads.map { case (p, r) =>
      parPool.submit(new Callable[Unit] {
        def call(): Unit = fmt(p, Await.result(r, Duration.Inf))
      })
    }
    futs.foreach(_.get())
    inputs.size
  }

  /** read + format every file on ONE thread (no pools): sequential baseline.
    * `formatSequential / pipelineFormat` = the parallel speedup the pipeline
    * actually delivers across cores.
    */
  @Benchmark
  def formatSequential(): Int = {
    inputs.foreach { im =>
      val code = Await.result(im.readInput(options), Duration.Inf)
      CoreScalafmt.formatCode(code, cfg, options.range, im.path.toString)
        .formatted
    }
    inputs.size
  }

}

object WrapperBenchmark {

  private val skipDirs = Set("target", ".git", "scala3")

  private def resolveCorpus(): Path = {
    val prop = sys.props.get("scalafmt.bench.corpus").filter(_.nonEmpty)
    val rel = Seq("scalafmt-core", "shared", "src", "main", "scala")
    val candidates = prop.map(Paths.get(_)).toSeq ++ Seq(
      Paths.get(rel.head, rel.tail: _*),
      Paths.get("..", rel: _*),
    )
    candidates.find(Files.isDirectory(_))
      .getOrElse(sys.error(s"corpus not found; tried ${candidates.mkString(", ")}"))
  }
}
