---
id: configuration
title: Configuration
---

Configuration for scalafmt is defined in a plain text file `.scalafmt.conf`
using [HOCON](https://github.com/typesafehub/config) syntax.

To reuse your configuration with IntelliJ, `.scalafmt.conf` must be placed in
the root directory of your project.

Here is an example `.scalafmt.conf`:

```yaml
align = true    # For pretty alignment.
maxColumn = 100 # For my wide 30" display.
```

> A note of warning. I personally use the default style, which means that the
> default style is by far the most tested and supported style. Most of the
> configuration flags are quite innocent, while some of them work very
> differently (esp. Scala.js). It is very difficult to guarantee that all
> configurations options play nicely together so I recommend you try not to go
> too crazy on this part.

The following sections describe the most common configuration options.

> If you are using scalafmt as a Standalone library, you can pass in a
> `ScalafmtConfig` instance, which is set to `ScalafmtStyle.default` by default.

## Most popular

### `maxColumn`

```scala vork:defaults
maxColumn
```

- Keep in mind that 80 characters fit perfectly on a split laptop screen with
  regular resolution.
- Github mobile view only shows 80 characters and sometimes you might review
  code on your phone.
- Consider refactoring your code before of choosing a value above 100.

### `docstrings`

```scala vork:defaults
docstrings
```

```scala vork:scalafmt
docstrings = ScalaDoc
---
/** Align by second asterisk.
  *
  */
```

```scala vork:scalafmt
docstrings = JavaDoc
---
/** Align by first asterisk.
 *
 */
```

### `assumeStandardLibraryStripMargin`

```scala vork:defaults
assumeStandardLibraryStripMargin
```

> May cause non-idempotent formatting in rare cases, see
> https://github.com/scalameta/scalafmt/issues/192.

If `true`, the margin character `|` is aligned with the opening triple quote
`"""` in interpolated and raw string literals.

```scala vork:scalafmt
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

```scala vork:scalafmt
assumeStandardLibraryStripMargin = true
---
val example2 =
  s"""|Examples:
      |  * one
      |  * two
      |  * $three
      |""".stripMargin
```

## Indentation

### `continuationIndent.callSite`

```scala vork:defaults
continuationIndent.callSite
```

Example:

```scala vork:scalafmt
continuationIndent.defnSite = 2
---
function(
  argument1, // indented by 2
  ""
)
```

### `continuationIndent.defnSite`

```scala vork:defaults
continuationIndent.defnSite
```

Same as `continuationIndent.callSite` except for definition site. Example:

```scala vork:scalafmt
continuationIndent.defnSite = 4
---
def function(
argument1: Type1 // indented by 4
): ReturnType
```

## Alignment

Default: **some**

Align has several nested fields, which you can customize. However, it comes with
four possible defaults: none, some, more, & most.

### `align`

#### `align=some`

```scala vork:scalafmt
align = some
---
x match { // true for case arrows
  case 2  => 22
  case 22 => 222
}

def foo(a: Int, // true for defn site open paren
        b: String): Int
foo(a: Int, // true for call site open paren
    b: String): Int

val x = 2 // false for assignment
val xx = 22

case object B extends A // false for `extends`
case object BB extends A
```

#### `align=none`

```scala vork:scalafmt
align = none
---
x match { // false for case arrows
  case 2  => 22 // also comments!
  case 22 => 222 // don't align me!
}

def foo(a: Int, // false for defn site
        b: String): Int
foo(a: Int, // false for call site
    b: String): Int
```

> **Pro tip**: Enable this setting to minimize git diffs/conflicts from
> renamings and other refactorings.

#### `align=more`

```scala vork:scalafmt
align = more
---
val x = 2 // true for assignment
val xx = 22

case object B extends A // false for `extends`
case object BB extends A

q -> 22 // true for various infix operators
qq -> 3 // and also comments!

for {
  x <- List(1) // true for alignment enumerator
  yy <- List(2)
} yield x ** xx

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

```scala vork:scalafmt
align = most
---
for {
  // align <- with =
  x <- List()
  yy = 2
  // aligns body by arrow
  zzz <- new Integer {
    def value = 3
  }
} yield x
```

> **Note**. Only for the truest vertical aligners. This is a new option, feel
> free to open PR enabling more crazy vertical alignment here. Expect changes.

### `align.tokens`

Default: **[caseArrow]**

An align token is a pair of `code`, which is the string literal of an operator
of token, and `owner`, which is the kind of the closest tree node that owns that
token. If no `owner` is provided, then all tree kinds will be matched.

```scala vork:scalafmt
align.tokens = [{code = "=>", owner = "Case"}]
---
x match {
  case 1 => 1 -> 2
  case 11 => 11 -> 22
}
```

```scala vork:scalafmt
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
`scala.meta.Tree.productPrefix` from the the (for example, Ammonite) REPL.

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

### `align.arrowEnumeratorGenerator`

```scala vork:defaults
align.arrowEnumeratorGenerator
```

```scala vork:scalafmt
align.arrowEnumeratorGenerator = false
---
for {
  x <- new Integer {
    def value = 2
  }
} yield x
```

```scala vork:scalafmt
align.arrowEnumeratorGenerator = true
---
for {
  x <- new Integer {
    def value = 2
  }
} yield x
```

### `align.openParenCallSite`

```scala vork:defaults
align.openParenCallSite
```

```scala vork:scalafmt
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

```scala vork:scalafmt
align.openParenCallSite = false
---
foo(arg1, arg2)
function(
  arg1, // no align by (
  arg2,
  arg3)
function(
  argument1,
  argument2)
```

### `align.openParenDefnSite`

```scala vork:defaults
align.openParenDefnSite
```

```scala vork:scalafmt
align.openParenDefnSite = true
---
class IntString(int: Int, string: String)

class IntStringLong(int: Int,
                    string: String,
                    long: Long)
```

```scala vork:scalafmt
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

// TODO(gabro): link to Vertical Multiline section

> You might be interested in the Vertical Multiline section.

### `newlines.alwaysBeforeTopLevelStatements`

```scala vork:defaults
newlines.alwaysBeforeTopLevelStatements
```

```scala vork:scalafmt
newlines.alwaysBeforeTopLevelStatements = false
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

```scala vork:scalafmt
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

```scala vork:defaults
newlines.alwaysBeforeElseAfterCurlyIf
```

```scala vork:scalafmt
newlines.alwaysBeforeElseAfterCurlyIf = true
---
if(someCond) {
  foo()
} else {
  bar()
}
```

```scala vork:scalafmt
newlines.alwaysBeforeElseAfterCurlyIf = false
---
if(someCond) {
  foo()
} else {
  bar()
}
```

## Rewrite Rules

To enable a rewrite rule, add it to the config like this
`rewrite.rules = [SortImports]`.

### `AvoidInfix`

```scala vork:scalafmt
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

```scala vork:scalafmt
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

```scala vork:scalafmt
rewrite.rules = [RedundantBraces]
---
def foo = {
  List(1, 2, 3).sum
}
```

```scala vork:scalafmt
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

```scala vork:scalafmt
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

```scala vork:scalafmt
rewrite.rules = [SortModifiers]
---
final lazy private implicit val x = 42
lazy final implicit private val y = 42
```

```scala vork:scalafmt
rewrite.rules = [SortModifiers]
---
class Test(
  implicit
  final private val i1: Int,
  private final val i2: String
)
```

```scala vork:scalafmt
rewrite.rules = [SortModifiers]
---
sealed protected[X] trait ADT
final private case object A1 extends ADT
private final case class A2(x: Int) extends ADT
```

If you choose the non-default sort order then you have to specify all eight
modifiers in the order you wish to see them. Hint: since some modifiers are
mutually exclusive, you might want to order them next to each other.

Example config:

```scala
rewrite.rules = [SortModifiers]
rewrite.sortModifiers.order = [
  "implicit", "final", "sealed", "abstract",
  "override", "private", "protected", "lazy"
]
```

### `PreferCurlyFors`

Replaces parentheses into curly braces in for comprehensions that contain
multiple enumerator generators.

```scala vork:scalafmt
rewrite.rules = [PreferCurlyFors]
---
for(a <- as; b <- bs if b > 2)
 yield (a, b)
```

### `SortImports`

The imports are sorted by the groups: symbols, lower-case, upper-case.

```scala vork:scalafmt
rewrite.rules = [SortImports]
---
import foo.{Zilch, bar, Random, sand}
```

### `AsciiSortImports`

The imports are sorted by their Ascii codes

```scala vork:scalafmt
rewrite.rules = [AsciiSortImports]
---
import foo.{~>, `symbol`, bar, Random}
```

## Vertical Multiline

Since: v1.6.0

If enabled this formats methods such that parameters are on their own line
indented by [`continuationIndent.defnSite`](#continuationindentdefnsite).
Separation between parameter groups are indented by two spaces less than
`continuationIndent.defnSite`. The return type is on its own line at then end.

> This formatting is only triggered if the method definition exceeds the
> maxColumn value in width or if the number of arguments to the method exceeds
> the [`verticalMultiline.arityThreshold`](#verticalmultilinearitythreshold).

### `verticalMultiline.arityThreshold`

```scala vork:defaults
verticalMultiline.arityThreshold
```

```scala vork:scalafmt
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

```scala vork:defaults
verticalMultiline.newlineAfterOpenParen
```

```scala vork:scalafmt
continuationIndent.defnSite = 2
verticalMultiline.atDefnSite = true
verticalMultiline.arityThreshold = 2
verticalMultiline.newlineAfterOpenParen = true
---
def other(a: String, b: String)(c: String, d: String) = a + b + c
```

### `verticalMultiline.newlineAfterOpenParen`

```scala vork:defaults
verticalMultiline.newlineAfterOpenParen
```

```scala vork:scalafmt
maxColumn = 60
verticalMultiline.atDefnSite = true
verticalMultiline.newlineBeforeImplicitKW = true
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

## Disabling Formatting

### // format: off

Disable formatting for specific regions of code by wrapping them in
`// format: off` blocks:

```scala vork:scalafmt
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

```scala vork:defaults
binPack.literalArgumentLists
```

```scala vork:scalafmt
binPack.literalArgumentLists = true
---
val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1,
  0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
```

```scala vork:scalafmt
binPack.literalArgumentLists = false
---
val secret: List[Bit] = List(0, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1)
```

### `includeCurlyBraceInSelectChains`

```scala vork:defaults
includeCurlyBraceInSelectChains
```

```scala vork:scalafmt
includeCurlyBraceInSelectChains = true
---
List(1)
  .map { x =>
    x + 2
  }
  .filter(_ > 2)
```

```scala vork:scalafmt
includeCurlyBraceInSelectChains = false
---
List(1)
  .map { x =>
    x + 2
  }
  .filter(_ > 2)
```

### `optIn.breakChainOnFirstMethodDot`

```scala vork:defaults
optIn.breakChainOnFirstMethodDot
```

```scala
// original
foo
  .map(_ + 1)
  .filter(_ > 2)
```

```scala vork:scalafmt
optIn.breakChainOnFirstMethodDot = true
---
foo
  .map(_ + 1)
  .filter(_ > 2)
```

```scala vork:scalafmt
optIn.breakChainOnFirstMethodDot = false
---
foo
  .map(_ + 1)
  .filter(_ > 2)
```

## Other

To find all available configuration options, it's best to browse the source code
of Scalafmt. A good place to start is `ScalafmtConfig`. Observe that this
listing below is the top-level, there are more configuration options if you
visited nested fields like `spaces` and `newlines`.

```scala vork:defaults:all

```
