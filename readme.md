# scalafmt [![codecov.io](https://codecov.io/github/olafurpg/scalafmt/coverage.svg?branch=master)](https://codecov.io/github/olafurpg/scalafmt?branch=master) [![Build Status](https://travis-ci.org/olafurpg/scalafmt.svg?branch=master)](https://travis-ci.org/olafurpg/scalafmt) [![Join the chat at https://gitter.im/olafurpg/scalafmt](https://badges.gitter.im/olafurpg/scalafmt.svg)](https://gitter.im/olafurpg/scalafmt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Latest version](https://index.scala-lang.org/olafurpg/scalafmt/scalafmt/latest.svg?color=orange)](https://index.scala-lang.org/olafurpg/scalafmt/scalafmt-core)

### [User documentation][docs]
Head over to [the user docs][docs] for instructions on how to install scalafmt. To build the docs yourself, `sbt readme/run` will produce the docs which you can open with `open readme/target/scalatex/index.html`.

## Hacking on scalafmt

I hope to make scalafmt a community effort so that the project has a
[truck number](https://en.wikipedia.org/wiki/Bus_factor) bigger than 1.
I've tried to do my best to concisely summarise how I work on scalafmt.
For any questions, don't hesitate to ask on gitter.

### Tips

- `core/testOnly org.scalafmt.FormatTests`: runs the fast, most relevant, unit tests.
    * After each run, a performance report is generated in `target/index.html`. 
    I usually keep a browser tab open at `localhost:3000/target/index.html`
    along with this background process:
    `browser-sync start --server --files "target/*.html"`.
    See [Browsersync](https://www.browsersync.io/).
- `core/testOnly org.scalafmt.FidelityTest`: runs the formatter on all files in
  this project.
- `run-benchmarks.sh` script to run jmh benchmarks.
- `core/test:runMain  org.scalafmt.FormatExperimentApp`:
  1. Downloads a ~20mb tar
     ([repos.tar.gz](https://github.com/olafurpg/scalafmt/releases/tag/v0.1.4))
     that contains ~28.000 Scala source files from public Github repos,
  2. untars,
  3. runs scalafmt on a subset of the files, specified in `FormatExperiment`.
     Runs various property based checks under timing constraints (around 10s per file),
  4. prints summary report to console.
- instructions for the tests [are here](core/src/test/resources).

[docs]: http://scalafmt.org

### Tutorial: add spacesInParentheses flag

I want to add a new configuration flag `spacesInParentheses` so that function
applications look like this

```scala
// Before
function(a, b, c)
// After
function( a, b, c )
```

- Clone the repo, cd into it, start `sbt` and then run `~core/testOnly org.scalafmt.FormatTests`.
  The unit tests should pass.
- Open `ScalafmtStyle.scala` and add a member `spacesInParentheses: Boolean`.
  Now you get a compiler error. Fix `ScalafmtStyle.default` style so that it
  has `spacesInParentheses = true`. The code should compile now and all tests 
  should pass.
- Open `core/src/test/resources/unit/Hacking.stat` and you will see a test like this

```scala
40 columns                              |
<<< SKIP Spaces in parentheses
function(a, b, c)
>>>
function( a, b, c )
```
- The column limit is 40 characters, because the test is in the `unit` directory.
- The test does not run because its name starts with SKIP.
- Change SKIP to ONLY, save and sbt should now only run this test and give you
  a failing output like this:

```scala
[debug] FormatWriter.scala:85                     NoSplit:56(cost=0, indents=[], NoPolicy) 0 0
[debug] FormatWriter.scala:85                     NoSplit:56(cost=0, indents=[], NoPolicy) 0 8
[debug] FormatWriter.scala:85     function        NoSplit:281(cost=0, indents=[], NoPolicy) 0 9
[debug] FormatWriter.scala:85     (               NoSplit:448(cost=0, indents=[4], P:412(D=true)) 4 10
[debug] FormatWriter.scala:85     a               NoSplit:494(cost=0, indents=[], NoPolicy) 4 11
[debug] FormatWriter.scala:85     ,               Space:514(cost=0, indents=[], NoPolicy) 4 13
[debug] FormatWriter.scala:85     b               NoSplit:494(cost=0, indents=[], NoPolicy) 4 14
[debug] FormatWriter.scala:85     ,               Space:514(cost=0, indents=[], NoPolicy) 4 16
[debug] FormatWriter.scala:85     c               NoSplit:1005(cost=0, indents=[], NoPolicy) 4 17
[debug] FormatWriter.scala:85     )               Newline:60(cost=0, indents=[], NoPolicy) 0 0
[debug] FormatWriter.scala:127    Total cost: 0
[debug] FormatTests.scala:90      Split(line=56, count=1), Split(line=448, count=1), Split(line=514, count=1)
[debug] FormatTests.scala:91      Total explored: 12
[info] FormatTests:
[info] - unit/Hacking.stat: Spaces in parentheses                               | *** FAILED *** (278 milliseconds)
[info]   ===========
[info]   => Obtained
[info]   ===========
[info]   function(a, b, c)
[info]
[info]
[info]   =======
[info]   => Diff
[info]   =======
[info]   -function(a, b, c)
[info]   +function( a, b, c ) (FormatTests.scala:33)
[info] Run completed in 443 milliseconds.
[info] Total number of tests run: 1
[info] Suites: completed 1, aborted 0
[info] Tests: succeeded 0, failed 1, canceled 0, ignored 0, pending 0
[info] *** 1 TEST FAILED ***
[error] Failed tests:
```

- Cool. The lines printed out from FormatWriter are interesting.
  Look at the first line, it has `NoSplit:56(cost=0 ...)`.
  This means that between the beginning of the file and the `function` token is a 
  NoSplit that origins from line 56 in `core/src/main/scala/org/scalafmt/internal/Router.scala`.
  Lets find that line:
  
```scala
  private def getSplits(formatToken: FormatToken): Seq[Split] = {
    val leftOwner = owners(formatToken.left)
    val rightOwner = owners(formatToken.right)
    val newlines = newlinesBetween(formatToken.between)
    formatToken match {
      case FormatToken(_: BOF, _, _) =>
        Seq(
            Split(NoSplit, 0) // <--- Line 56
        )
      ...
```

- BOF stands for "beginning of file".
- Look at `NoSplit:281`

```scala
      // Opening ( with no leading space.
      case FormatToken(
          _: `this` | _: Ident | _: `]` | _: `}` | _: `)`, _: `(` | _: `[`, _)
          if noSpaceBeforeOpeningParen(rightOwner) && {
            leftOwner.parent.forall {
              // infix applications have no space.
              case _: Type.ApplyInfix | _: Term.ApplyInfix => false
              case parent => true
            }
          } =>
        Seq(
            Split(NoSplit, 0) // <-- Line 281
        )
```

- Looks a lot more scary than line 56. What's happening is that we're matching on
  a FormatToken, which has three members:
  
```scala
case class FormatToken(left: Token, right: Token, between: Vector[Whitespace])
```

- We matched on line 281 because left was `function` which is of type `Ident`
  (identifier) and right was the opening parenthesis of type `<backtick>(<backtick>`.
- Let's look at `NoSplit:448` then, we want to put a space there instead of a
  NoSplit.


```scala
        Seq(
            Split(modification, // <-- line 448
                  0,
                  policy = singleLine(6),
                  ignoreIf = !fitsOnOneLine)
              .withOptimalToken(expirationToken)
              .withIndent(indent, close, Left),
```

- `modification` is a variable, it's defined like this:

```scala
        val modification =
          if (right.isInstanceOf[Comment]) newlines2Modification(between)
          else NoSplit
```

- Let's modify it into this:

```scala
        val modification =
          if (right.isInstanceOf[Comment]) newlines2Modification(between)
          else if (style.spacesInParentheses) Space
          else NoSplit
```

- Save and see the test run.

```scala
[debug] FormatWriter.scala:85                     NoSplit:56(cost=0, indents=[], NoPolicy) 0 0
[debug] FormatWriter.scala:85                     NoSplit:56(cost=0, indents=[], NoPolicy) 0 8
[debug] FormatWriter.scala:85     function        NoSplit:281(cost=0, indents=[], NoPolicy) 0 9
[debug] FormatWriter.scala:85     (               Space:448(cost=0, indents=[4], P:412(D=true)) 4 11
[debug] FormatWriter.scala:85     a               NoSplit:494(cost=0, indents=[], NoPolicy) 4 12
[debug] FormatWriter.scala:85     ,               Space:514(cost=0, indents=[], NoPolicy) 4 14
[debug] FormatWriter.scala:85     b               NoSplit:494(cost=0, indents=[], NoPolicy) 4 15
[debug] FormatWriter.scala:85     ,               Space:514(cost=0, indents=[], NoPolicy) 4 17
[debug] FormatWriter.scala:85     c               NoSplit:1005(cost=0, indents=[], NoPolicy) 4 18
[debug] FormatWriter.scala:85     )               Newline:60(cost=0, indents=[], NoPolicy) 0 0
[debug] FormatWriter.scala:127    Total cost: 0
[debug] FormatTests.scala:90      Split(line=56, count=1), Split(line=448, count=1), Split(line=514, count=1)
[debug] FormatTests.scala:91      Total explored: 12
[info] FormatTests:
[info] - unit/Hacking.stat: Spaces in parentheses                               | *** FAILED *** (298 milliseconds)
[info]   ===========
[info]   => Obtained
[info]   ===========
[info]   function( a, b, c)
[info]
[info]
[info]   =======
[info]   => Diff
[info]   =======
[info]   -function( a, b, c)
[info]   +function( a, b, c ) (FormatTests.scala:33)
```

- Neat, there's a space after the opening `(`! Only missing space before closing parenthesis.
- Look at line 1005

```scala
      case FormatToken(_, _: `]` | _: `)`, _) =>
        Seq(
            Split(NoSplit, 0) // <- line 1005
        )
```

- Change the line to this

```scala
            Split(if (style.spacesInParentheses) Space else NoSplit, 0)
```

- Save and yay, the test passes now.
- Open `Hacking.stat` again and remove ONLY from the test name so it looks
  like this `<<< Spaces in parentheses`.
- Run the tests. Awww, 295 failing tests.
- Look carefully through the diffs check if the output looks nice.
- Change spacesInParentheses to false in the default style.
- Create a new directory in `core/src/test/resources` named spaces (it already exists).
- Move `unit/Hacking.stat` to `spaces/Spaces.stat`.
- Run the test and you will get an error like
```
[trace] Stack trace suppressed: run last core/test:testOnly for the full output.
[error] Could not run test org.scalafmt.FormatTests: org.scalafmt.Error$UnknownStyle: Don't understand style spaces
```
- Open the file `core/src/test/scala/org/scalafmt/util/HasTests.scala` and
  look at the `file2style` method.

```scala
  def file2style(filename: String): ScalafmtStyle =
    filename.split("/").reverse(1) match {
      case "unit" => ScalafmtStyle.unitTest40
      case "default" | "standard" | "scala" => ScalafmtStyle.unitTest80
      case "scalajs" => ScalafmtStyle.scalaJs
      case "stripMargin" => ScalafmtStyle.default
      case "align" =>
        ScalafmtStyle.default.copy(alignTokens = AlignToken.default)
      case style => throw UnknownStyle(style)
    }
```

- Add a case like this

```scala
      case "spaces" =>
        ScalafmtStyle.default.copy(spacesInParentheses = true)
```

- Run the tests. Yay everything seems to be working fine.
- The last thing to do is to add spacesInParentheses to the CLI flags.
- Open `cli/src/main/scala/org/scalafmt/cli/Cli.scala`
- Add a new flag option like this
```scala
    opt[Boolean]("spacesInParentheses") action { (bool, c) =>
      c.copy(style = c.style.copy(spacesInParentheses = bool))
    } text s"See ScalafmtConfig scaladoc."
```
- Clean up your branch and open a PR. I'm sure there will be lots
  of cases that still need more fixing but this is a great proof-of-concept.
  I recommend you get some feedback.
  
## Contribution Guidelines

Shamelessly stolen from [lihaoyi/ammonite](https://github.com/lihaoyi/Ammonite).

- **All code PRs should come with**: a meaningful description, inline-comments
  for important things, unit tests (positive and negative), and a green build
  in [CI](https://travis-ci.org/olafurpg/scalafmt). Note, the tests format
  1.2 million lines of code twice which takes ~1hr to run on travis.
- **Format your code with the lastest release of scalafmt, default style.**.
- **PRs for features should generally come with *something* added to the
  [Documentation](https://olafurpg.github.io/scalafmt)**, so people can discover
  that it exists. The docs are written in `readme/Readme.scalatex`.
- **Be prepared to discuss/argue-for your changes if you want them merged**!
  You will probably need to refactor so your changes fit into the larger
  codebase - **If your code is hard to unit test, and you don't want to unit
  test it, that's ok**. But be prepared to argue why that's the case!
- **It's entirely possible your changes won't be merged**, or will get ripped
  out later. This is also the case for my changes, as the Author!
- **Even a rejected/reverted PR is valuable**! It helps explore the solution
  space, and know what works and what doesn't. For every line in the repo, at
  least three lines were tried, committed, and reverted/refactored, and more
  than 10 were tried without committing.
- **Feel free to send Proof-Of-Concept PRs** that you don't intend to get merged.

## Acknowledgements

<a href="http://www.ej-technologies.com/products/jprofiler/overview.html">
  <img src="https://www.ej-technologies.com/images/product_banners/jprofiler_large.png" align="right" />
 </a>
[JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html) generously supports scalafmt with its full-featured Java Profiler.
