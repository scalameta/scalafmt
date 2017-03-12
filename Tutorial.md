# Hacking on scalafmt

I hope to make scalafmt a community effort so that the project has a
[truck number](https://en.wikipedia.org/wiki/Bus_factor) bigger than 1.
I've tried to do my best to concisely summarise how I work on scalafmt.
For any questions, don't hesitate to ask on gitter.

## Tutorial: add spacesInParentheses flag

**Last updated: January 6 2016.**

After completing the tutorial, you should have a diff looking like
[this commit](https://github.com/scalameta/scalafmt/commit/d12acecfb374da6059be252e1703f274ea0eed74).

I want to add a new configuration flag `spacesInParentheses` so that function
applications look like this

```scala
// Before
function(a, b, c)
// After
function( a, b, c )
```
- `git clone https://github.com/scalameta/scalafmt.git`
- `cd scalafmt`
- `sbt ~core/testOnly org.scalafmt.FormatTests`.
  The unit tests should pass. Keep this process running.
- Open the project in your favorite editor.
- Open [Spaces.scala](core/src/main/scala/org/scalafmt/config/Spaces.scala)
  and add a member `inParentheses: Boolean = true`.
- Save the file. The code should re-compile and all tests should pass.
- Open [Hacking.stat](core/src/test/resources/spaces/Hacking.stat) and you will
  see a unit test like this

```scala
maxColumn = 40
<<< SKIP Spaces in parentheses
function(a, b, c)
>>>
function( a, b, c )
```
- The column limit is 40 characters, you can put any configuration at the top of
  the file.
- The test does not run because its name starts with SKIP.
- Change SKIP to ONLY, save and sbt should now only run this test and give you
  a failing output like this:

```scala
[debug] FormatWriter.scala:118                    NoSplit:72(cost=0, indents=[], NoPolicy) 0 0
[debug] FormatWriter.scala:118                    NoSplit:72(cost=0, indents=[], NoPolicy) 0 8
[debug] FormatWriter.scala:118    function        NoSplit:356(cost=0, indents=[], NoPolicy) 0 9
[debug] FormatWriter.scala:118    (               NoSplit:630(cost=0, indents=[], P:612(D=false)) 0 10
[debug] FormatWriter.scala:118    a               NoSplit:703(cost=0, indents=[], NoPolicy) 0 11
[debug] FormatWriter.scala:118    ,               Space:752(cost=0, indents=[], NoPolicy) 0 13
[debug] FormatWriter.scala:118    b               NoSplit:703(cost=0, indents=[], NoPolicy) 0 14
[debug] FormatWriter.scala:118    ,               Space:752(cost=0, indents=[], NoPolicy) 0 16
[debug] FormatWriter.scala:118    c               NoSplit:1276(cost=0, indents=[], NoPolicy) 0 17
[debug] FormatWriter.scala:118    )               Newline:76(cost=0, indents=[], NoPolicy) 0 0
[debug] FormatWriter.scala:169    Total cost: 0
[debug] FormatTests.scala:96      Split(line=630, count=2), Split(line=647, count=2), Split(line=639, count=2)
[debug] FormatTests.scala:97      Total explored: 24
[info] FormatTests:
[info] - spaces/Hacking.stat: Spaces in paretheses                             | *** FAILED *** (275 milliseconds)
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
[info]   +function( a, b, c ) (FormatTests.scala:30)
```

- Cool. The lines printed out from FormatWriter are interesting.
  Look at the first line, it has `NoSplit:72(cost=0 ...)`.
  This means that between the beginning of the file and the `function` token is a 
  `NoSplit` (not space or newline) that origins from line 72 in
  [Router.scala](core/src/main/scala/org/scalafmt/internal/Router.scala#L72).
  Lets find that line:
  
```scala
    formatToken match {
      case FormatToken(_: BOF, _, _) =>
        Seq(
            Split(NoSplit, 0) // <--- Line 72
        )
      ...
```

- BOF stands for "beginning of file".
- We want a space after the opening parentheses, so let's take a look at
  [line 630](core/src/main/scala/org/scalafmt/internal/Router.scala#L630).

```scala
Seq(
  Split(modification, 0, policy = noSplitPolicy) // <- line 630
    .withOptimalToken(expirationToken, killOnFail = false)
    .withIndent(noSplitIndent, close, Right),
```
- `modification` is a variable, it's defined like this:

```scala
val modification =
  if (right.is[Comment]) newlines2Modification(between)
  else NoSplit
```

- Let's modify it into this:

```scala
val modification =
  if (right.is[Comment]) newlines2Modification(between)
  else if (style.spaces.inParentheses) Space
  else NoSplit
```

- Save and see the test run.

```scala
[info]   =======
[info]   => Diff
[info]   =======
[info]   -function( a, b, c)
[info]   +function( a, b, c ) (FormatTests.scala:30)
```

- Neat, there's a space after the opening `(`! Only missing space before closing parenthesis.
- Look at line 1276.

```scala
Split(NoSplit, 0)  // <- line 1276
```

- Change the line to this

```scala
Split(if (style.spaces.inParentheses) Space else NoSplit, 0)
```

- Save and yay, the test passes now.
- Open `Hacking.stat` again and remove ONLY from the test name.
- Run the tests. Awww, 479 failing tests.
- Look carefully through the diffs check if the output looks nice.
- Poke around in Router.scala and polish the setting until you're happy with the
  output in the failing tests.
- Add new tests in Hacking.stat for cases you didn't think of. 
- Change spacesInParentheses to false by default.
- Clean up your branch and open a PR. I'm sure there will be lots
  of cases that still need more fixing but this is a great start.
  I recommend you get some feedback.
  
