---
id: configuration
title: Configuration
---

Configuration for scalafmt is defined in a plain text file `.scalafmt.conf`
using [HOCON](https://github.com/lightbend/config) syntax.

Here is an example `.scalafmt.conf`:

```scala config
align = more    // For pretty alignment.
maxColumn = 100 // For my wide 30" display.
```

## Most popular

### `maxColumn`

```scala mdoc:defaults
maxColumn
```

- Keep in mind that 80 characters fit perfectly on a split laptop screen with
  regular resolution.
- GitHub mobile view only shows 80 characters and sometimes you might review
  code on your phone.
- Consider refactoring your code before of choosing a value above 100.

### `docstrings`

```scala mdoc:defaults
docstrings
```

```scala mdoc:scalafmt
docstrings = ScalaDoc
---
/** Align by second asterisk.
 *
 */
```

```scala mdoc:scalafmt
docstrings = JavaDoc
---
/** Align by first asterisk.
  *
  */
```

### `assumeStandardLibraryStripMargin`

```scala mdoc:defaults
assumeStandardLibraryStripMargin
```

If `true`, the margin character `|` is aligned with the opening triple quote
`"""` in interpolated and raw string literals.

```scala mdoc:scalafmt
assumeStandardLibraryStripMargin = true
---
val example1 =
  s"""Examples:
  |  * one
  |  * two
  |  * $three
  |""".stripMargin
```

The pipe character can immediately follow the opening `"""`

```scala mdoc:scalafmt
assumeStandardLibraryStripMargin = true
---
val example2 =
  s"""|Examples:
  |  * one
  |  * two
  |  * $three
  |""".stripMargin
```

## Edition

The `edition` setting allows Scalafmt users to stay on older default settings
while upgrading to the latest Scalafmt release.

The use-case for configuring the `edition` setting is when you want to upgrade
to the latest Scalafmt version (for example, to enjoy a bug fix in the parser)
but need more time to deal with the latest changes in the formatting output.

The end goal for users should be to remove the `edition` setting from
`.scalafmt.conf` in order to enjoy the latest improvements to the formatting
output.

The `edition` setting is formatted as `"$year-$month"` and should be interpreted
as: "use the default settings at that given date". The latest edition is selected by default.

Example:

```scala config
edition = 2019-10 // default settings from October 2019 should be used 
```

## Indentation

### `continuationIndent.callSite`

```scala mdoc:defaults
continuationIndent.callSite
```

Example:

```scala mdoc:scalafmt
continuationIndent.callSite = 2
---
function(
argument1, // indented by 2
""
)
```

### `continuationIndent.defnSite`

```scala mdoc:defaults
continuationIndent.defnSite
```

Same as `continuationIndent.callSite` except for definition site. Example:

```scala mdoc:scalafmt
continuationIndent.defnSite = 4
---
def function(
argument1: Type1 // indented by 4
): ReturnType
```

## Alignment

Default: **some**

Align has several nested fields, which you can customize. However, it comes with
four possible presets: none, some, more, & most.

### `align`

#### `align=none`

```scala mdoc:scalafmt
align = none
---
x match { // false for case arrows
  case 2  => 22 // also comments!
  case 22 => 222 // don't align me!
}
```

> **Pro tip**: Enable this setting to minimize git diffs/conflicts from
> renamings and other refactorings, without having to ignore whitespace changes
> in diffs or use `--ignore-all-space` to avoid conflicts when git merging or
> rebasing.

#### `align=some`

```scala mdoc:scalafmt
align = some
---
x match { // true for case arrows
  case 2 => 22
  case 22 => 222
}

val x = 2 // false for assignment
val xx = 22

case object B extends A // false for `extends`
case object BB extends A
```

#### `align=more`

```scala mdoc:scalafmt
align = more
---
val x = 2 // true for assignment
val xx = 22

case object B extends A // true for `extends`
case object BB extends A

q -> 22 // true for various infix operators
qq -> 3   // and also comments!

for {
  x <- List(1) // true for alignment enumerator
  yyy <- List(2)
} yield x ** yyy

x match { // true for multiple tokens across multiple lines
  case 1 => 1 -> 2 // first
  case 11 => 11 -> 22 // second

  // A blank line separates alignment blocks.
  case `ignoreMe` => 111 -> 222
}

// Align assignments of similar type.
def name = column[String]("name")
def status = column[Int]("status")
val x = 1
val xx = 22

// Align sbt module IDs.
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-compiler" % scalaVersion.value,
  "com.lihaoyi" %% "sourcecode" % "0.1.1"
)
```

#### `align=most`

```scala mdoc:scalafmt
align = most
---
for {
  // align <- with =
  x <- List()
  yyy = 2
  // aligns body by arrow
  zzz <- new Integer {
    def value = 3
  }
} yield x
```

> **Note**. Only for the truest vertical aligners. Feel free to open PR enabling
> more crazy vertical alignment here. Expect changes.

### `align.tokens`

Default: **[caseArrow]**

An align token is a pair of `code`, which is the string literal of an operator
of token, and `owner`, which is the kind of the closest tree node that owns that
token. If no `owner` is provided, then all tree kinds will be matched.

```scala mdoc:scalafmt
align.tokens = [{code = "=>", owner = "Case"}]
---
x match {
  case 1 => 1 -> 2
  case 11 => 11 -> 22
}
```

```scala mdoc:scalafmt
align.tokens = [
  {code = "%", owner = "Term.ApplyInfix"},
  {code = "%%", owner = "Term.ApplyInfix"}
]
---
val x = List(
"org.scala-lang" %% "scala-compiler" % scalaVersion.value,
"com.lihaoyi" %% "sourcecode" % "0.1.1"
)
```

To find the `owner` part for a custom tree, depend on Scalameta and use
`scala.meta.Tree.productPrefix` from the (for example, Ammonite) REPL.

```scala
@ import $ivy.`org.scalameta:scalameta_2.12:@SCALAMETA_VERSION@`, scala.meta._
@ val termMatch = q"x match { case 2 => foo(bar) }"
termMatch: Term.Match = x match {
 case 2 =>
   foo(bar)
}
@ termMatch.structure
res0: String = """
Term.Match(Term.Name("x"), Seq(Case(Lit.Int(2), None, Term.Apply(Term.Name("foo"), Seq(Term.Name("bar"))))))
"""

@ termMatch.productPrefix
res1: String = "Term.Match"
```

### `align.tokens.add`

Usage is identical to `align.tokens`, but token pairs will be _added_ to
defaults rather than replacing them.

### `align.arrowEnumeratorGenerator`

```scala mdoc:defaults
align.arrowEnumeratorGenerator
```

```scala mdoc:scalafmt
align.arrowEnumeratorGenerator = false
---
for {
  x <- new Integer {
     def value = 2
     }
} yield x
```

```scala mdoc:scalafmt
align.arrowEnumeratorGenerator = true
---
for {
  x <- new Integer {
     def value = 2
     }
} yield x
```

### `align.openParenCallSite`

```scala mdoc:defaults
align.openParenCallSite
```

> Default changed from `true` to `false` in v1.6.

```scala mdoc:scalafmt
align.openParenCallSite = true
---
foo(arg1, arg2)

function(arg1, // align by (
    arg2,
 arg3)

function(
  argument1,
  argument2)
```

```scala mdoc:scalafmt
align.openParenCallSite = false
---
foo(arg1, arg2)

function(arg1, // no align by (
    arg2,
 arg3)

function(
  argument1,
  argument2)
```

### `align.openParenDefnSite`

```scala mdoc:defaults
align.openParenDefnSite
```

> Default changed from `true` to `false` in v1.6.

```scala mdoc:scalafmt
align.openParenDefnSite = true
---
class IntString(int: Int, string: String)

class IntStringLong(int: Int,
    string: String,
  long: Long)
```

```scala mdoc:scalafmt
align.openParenDefnSite = false
---
class IntString(int: Int, string: String)

class IntStringLong(
      int: Int,
    string: String,
  long: Long
)
```

## Newlines

The `newlines.*` options are used to configure when and where `scalafmt` should
insert newlines.

> You might be interested in the [Vertical Multiline](#vertical-multiline)
> section.

### `newlines.alwaysBeforeTopLevelStatements`

```scala mdoc:defaults
newlines.alwaysBeforeTopLevelStatements
```

```scala mdoc:scalafmt
newlines.alwaysBeforeTopLevelStatements = false
---
import org.scalafmt
package core { // no newline added here
  object O { // nor here
    val x1 = 1
    val x2 = 2
    def A = "A"
    def B = "B"
  }
}
```

```scala mdoc:scalafmt
newlines.alwaysBeforeTopLevelStatements = true
---
import org.scalafmt
package core {
  object O {
    val x1 = 1
    val x2 = 2
    def A = "A"
    def B = "B"
  }
}
```

### `newlines.alwaysBeforeElseAfterCurlyIf`

```scala mdoc:defaults
newlines.alwaysBeforeElseAfterCurlyIf
```

```scala mdoc:scalafmt
newlines.alwaysBeforeElseAfterCurlyIf = true
---
if (someCond) {
  foo()
} else {
  bar()
}
```

```scala mdoc:scalafmt
newlines.alwaysBeforeElseAfterCurlyIf = false
---
if (someCond) {
  foo()
}
else {
  bar()
}
```

### `newlines.avoidEmptyLinesAroundBlock`

Removes empty lines at at the beginning and end of blocks (true by default)

```scala mdoc:defaults
newlines.avoidEmptyLinesAroundBlock
```

```scala mdoc:scalafmt
newlines.avoidEmptyLinesAroundBlock = true
---
def f(): Unit = {

  println("hello")

}
```

```scala mdoc:scalafmt
newlines.avoidEmptyLinesAroundBlock = false
---
def f(): Unit = {
  println("hello")
}
```

## Rewrite Rules

To enable a rewrite rule, add it to the config like this
`rewrite.rules = [SortImports]`.

### `AvoidInfix`

```scala mdoc:scalafmt
rewrite.rules = [AvoidInfix]
---
a success b
a error (b, c)
a map { x =>
  x + 2
}
"o" % "a" % "v" c(D)
future recover {
  case e: Err => 0
} map (_.toString)
```

### `ExpandImportSelectors`

```scala mdoc:scalafmt
rewrite.rules = [ExpandImportSelectors]
---
import a.{
    b,
    c
  }, h.{
    k, l
  }
import d.e.{f, g}
import a.{
    foo => bar,
    zzzz => _,
    _
  }
```

### `RedundantBraces`

> Warning. This rewrite can cause non-idempotent formatting, see
> [#1055](https://github.com/scalameta/scalafmt/issues/1055).

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
---
def foo = {
  List(1, 2, 3).sum
}
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.stringInterpolation = true
---
q"Hello ${name}"
```

Configuration options and default values:

// TODO(olafur): multiline defaults

- `rewrite.redundantBraces.maxLines = 100`
- `rewrite.redundantBraces.includeUnitMethods = true`
- `rewrite.redundantBraces.stringInterpolation = true`
- `rewrite.redundantBraces.generalExpressions = false` (disabled by default due
  to #1147)

### `RedundantParens`

```scala mdoc:scalafmt
rewrite.rules = [RedundantParens]
---
for {
  a <- b
  if (a.nonEmpty)
} yield a
```

### `SortModifiers`

Modifiers are sorted based on the given order. Affects modifiers of the
following definitions: trait, class, object, type, and val+var, both as fields
and class parameters.

```scala mdoc:scalafmt
rewrite.rules = [SortModifiers]
---
final lazy private implicit val x = 42
lazy final implicit private val y = 42
```

```scala mdoc:scalafmt
rewrite.rules = [SortModifiers]
---
class Test(
    implicit
    final private val i1: Int,
    private final val i2: String
)
```

```scala mdoc:scalafmt
rewrite.rules = [SortModifiers]
---
sealed protected[X] trait ADT
final private case object A1 extends ADT
private final case class A2(a: Int)
    extends ADT
```

If you choose the non-default sort order then you have to specify all eight
modifiers in the order you wish to see them. Hint: since some modifiers are
mutually exclusive, you might want to order them next to each other.

```scala mdoc:scalafmt
rewrite.rules = [SortModifiers]
rewrite.sortModifiers.order = [
  "implicit", "final", "sealed", "abstract",
  "override", "private", "protected", "lazy"
]
---
override implicit final val x = 2
```

### `PreferCurlyFors`

Replaces parentheses into curly braces in for comprehensions that contain
multiple enumerator generators.

```scala mdoc:scalafmt
rewrite.rules = [PreferCurlyFors]
---
for(a <- as; b <- bs if b > 2)
 yield (a, b)
```

### `SortImports`

The imports are sorted by the groups: symbols, lower-case, upper-case.

```scala mdoc:scalafmt
rewrite.rules = [SortImports]
---
import foo.{Zilch, bar, Random, sand}
```

### `AsciiSortImports`

The imports are sorted by their Ascii codes

```scala mdoc:scalafmt
rewrite.rules = [AsciiSortImports]
---
import foo.{~>, `symbol`, bar, Random}
```

## Vertical Multiline

Since: v1.6.0.

If enabled this formats methods such that parameters are on their own line
indented by [`continuationIndent.defnSite`](#continuationindentdefnsite).
Separation between parameter groups are indented by two spaces less than
`continuationIndent.defnSite`. The return type is on its own line at the end.

> This formatting is only triggered if the method definition exceeds the
> maxColumn value in width or if the number of arguments to the method exceeds
> the [`verticalMultiline.arityThreshold`](#verticalmultilinearitythreshold).

### `verticalMultiline.arityThreshold`

```scala mdoc:defaults
verticalMultiline.arityThreshold
```

```scala mdoc:scalafmt
verticalMultiline.atDefnSite = true
verticalMultiline.arityThreshold = 2
---
case class Foo(x: String)
case class Bar(x: String, y: String)
object A {
  def foo(x: String, y: String)
  def hello(how: String)(are: String)(you: String) = how + are + you
}
```

### `verticalMultiline.newlineAfterOpenParen`

```scala mdoc:defaults
verticalMultiline.newlineAfterOpenParen
```

```scala mdoc:scalafmt
continuationIndent.defnSite = 2
verticalMultiline.atDefnSite = true
verticalMultiline.arityThreshold = 2
verticalMultiline.newlineAfterOpenParen = true
---
def other(a: String, b: String)(c: String, d: String) = a + b + c
```

### `verticalMultiline.newlineBeforeImplicitKW`

```scala mdoc:defaults
verticalMultiline.newlineBeforeImplicitKW
```

```scala mdoc:scalafmt
maxColumn = 60
verticalMultiline.atDefnSite = true
verticalMultiline.newlineBeforeImplicitKW = true
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

## Disabling Formatting

<!-- TODO: document dynamic configuration: https://github.com/scalameta/scalafmt/pull/464 -->

### // format: off

Disable formatting for specific regions of code by wrapping them in
`// format: off` blocks:

```scala mdoc:scalafmt
---
// format: off
val identity = Array(1, 0, 0,
                     0, 1, 0,
                     0, 0, 1)
// format: on
```

### Project

Configure which source files should be formatted in this project.

```conf
# Only format files tracked by git.
project.git = true
# manually exclude files to format.
project.excludeFilters = [
   regex1
   regex2
]
# manually include files to format.
project.includeFilters = [
  regex1
  regex2
]
```

## Miscellaneous

### `binPack.literalArgumentLists`

```scala mdoc:defaults
binPack.literalArgumentLists
```

```scala mdoc:scalafmt
binPack.literalArgumentLists = true
---
val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1,
  0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
```

```scala mdoc:scalafmt
binPack.literalArgumentLists = false
---
val secret: List[Bit] = List(0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1)
```

### `includeCurlyBraceInSelectChains`

```scala mdoc:defaults
includeCurlyBraceInSelectChains
```

```scala mdoc:scalafmt
includeCurlyBraceInSelectChains = true
---
List(1).map { x =>
    x + 2
  }
  .filter(_ > 2)
```

```scala mdoc:scalafmt
includeCurlyBraceInSelectChains = false
---
List(1)
  .map { x =>
    x + 2
  }
  .filter(_ > 2)
```

### `includeNoParensInSelectChains`

```scala mdoc:defaults
includeNoParensInSelectChains
```

```scala mdoc:scalafmt
includeNoParensInSelectChains = true
---
List(1).toIterator.buffered
  .map(_ + 2)
  .filter(_ > 2)
```

```scala mdoc:scalafmt
includeNoParensInSelectChains = false
---
List(1).toIterator.buffered.map(_ + 2).filter(_ > 2)
```

### `optIn.breakChainOnFirstMethodDot`

```scala mdoc:defaults
optIn.breakChainOnFirstMethodDot
```

```scala mdoc:scalafmt
optIn.breakChainOnFirstMethodDot = false
---
// collapse into a single line
foo
  .map(_ + 1)
  .filter(_ > 2)
```

```scala mdoc:scalafmt
optIn.breakChainOnFirstMethodDot = true
---
foo
  .map(_ + 1) // preserve existing newlines
  .filter(_ > 2)
```

## Other

To find all available configuration options, it's best to browse the source code
of Scalafmt. A good place to start is `ScalafmtConfig`. Observe that this
listing below is the top-level, there are more configuration options if you
visited nested fields like `spaces` and `newlines`.

```scala mdoc:defaults:all

```
