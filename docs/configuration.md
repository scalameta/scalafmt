---
id: configuration
title: Configuration
---

Configuration for scalafmt is defined in a plain text file `.scalafmt.conf`
using [HOCON](https://github.com/lightbend/config) syntax.

Here is an example `.scalafmt.conf`:

```scala config
align.preset = more    // For pretty alignment.
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

This parameter simply says the `.stripMargin` method was not redefined
by the user to assign special meaning to indentation preceding
the `|` character. Hence, that indentation can be modified.

```scala mdoc:defaults
assumeStandardLibraryStripMargin
```

```scala mdoc:defaults
align.stripMargin
```

If `true`, lines starting with the margin character `|` (or another if
specified in the `.stripMargin(...)` call) will be indented differently.

If `align.stripMargin` is true, they will align with the opening triple-quote
`"""` in interpolated and raw string literals. Otherwise, they will be indented
relative to the _start_ of the opening line.

```scala mdoc:scalafmt
assumeStandardLibraryStripMargin = true
align.stripMargin = true
---
val example1 =
  s"""Examples:
  |  * one
  |  * two
  |  * $three
  |""".stripMargin
```

```scala mdoc:scalafmt
assumeStandardLibraryStripMargin = true
align.stripMargin = false
---
val example1 =
  s"""|Examples:
      |  * one
      |  * two
      |  * $three
      |""".stripMargin
```

The pipe character can immediately follow the opening `"""`

```scala mdoc:scalafmt
assumeStandardLibraryStripMargin = true
align.stripMargin = true
---
val example2 =
  s"""|Examples:
  |  * one
  |  * two
  |  * $three
  |""".stripMargin
```

## Presets

Some sections provide preset values to set multiple parameters at once.
These are always accessed via the `preset` key of the appropriate section,
including top-level.

### Top-level presets

- `preset=default`: this preset is implicit and sets all values to their defaults.
- `preset=IntelliJ`: this preset is defined as

```
    preset = default
    continuationIndent.defnSite = 2
    optIn.configStyleArguments = false
```

- `preset=defaultWithAlign`: this preset is defined as

```
    preset = default
    align.preset = more
```

- `preset=Scala.js`: this preset is defined as

```
    preset = default
    binPack.preset = true
    align.ifWhileOpenParen = false
    continuationIndent.callSite = 4
    docstrings = JavaDoc
    importSelectors = binPack
    newlines {
      neverInResultType = true
      neverBeforeJsNative = true
      sometimesBeforeColonInMethodReturnType = false
    )
    runner.optimizer {
      forceConfigStyleOnOffset = 500
      forceConfigStyleMinArgCount = 5
    }
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

#### `align.preset=none`

```scala mdoc:scalafmt
align.preset = none
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

> Starting with the introduction of `align.stripMargin` parameter in v2.5.0,
> one must explicitly enable it to get earlier behaviour of `align.preset=none`.
> See [assumeStandardLibraryStripMargin](#assumestandardlibrarystripmargin).

#### `align.preset=some`

```scala mdoc:scalafmt
align.preset = some
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

#### `align.preset=more`

```scala mdoc:scalafmt
align.preset = more
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

#### `align.preset=most`

```scala mdoc:scalafmt
align.preset = most
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

### `align.stripMargin`

See [assumeStandardLibraryStripMargin](#assumestandardlibrarystripmargin).

```scala mdoc:defaults
align.stripMargin
```

This functionality is enabled in all presets except `align.preset=none` where it was
disabled since the parameter's introduction in v2.5.0.

## Newlines

The `newlines.*` options are used to configure when and where `scalafmt` should
insert newlines.

> You might be interested in the [Vertical Multiline](#vertical-multiline)
> section.

### `newlines.source`

> Since v2.5.0.

This parameter controls the general approach to line breaks, and whether to take into
account existing newlines in the source. The default value (if the parameter is not
specified) is the _classic_, original way. Below are the alternatives.

> These alternatives are EXPERIMENTAL and might change in the future without
> regard to any `edition` settings, until fully released (and this message deleted).

#### `newlines.source=keep`

This approach attempts to preserve line breaks in the input whenever possible.

#### `newlines.source=fold,unfold`

These two approaches _completely ignore_ existing line breaks, except around comments
and blank lines (i.e., multiple consecutive newlines).

> Might require increasing runner limits
> (`runner.optimizer.maxVisitsPerToken`, possibly even `runner.maxStateVisits`),
> to avoid _SearchStateExploded_ exceptions.

`fold` attempts to remove line breaks whenever possible resulting in a more
horizontal, or vertically compact look.

`unfold`, on the other hand, is intended for those who prefer a more vertical,
or horizontally compact look.

Both settings attempt to play nice with other parameters, but some combinations
are prohibited and will result in an error.

### `danglingParentheses`

While this parameter is not technically under the `newlines` section, it
logically belongs there.

#### `danglingParentheses.defnSite`

```scala mdoc:defaults
danglingParentheses.defnSite
```

```scala mdoc:scalafmt
danglingParentheses.defnSite = true
danglingParentheses.callSite = false
maxColumn=25
---
object a {
  // defnSite
  def method(a: Int, b: String): Boolean

  // callSite
  method(argument1, argument2)
}
```

#### `danglingParentheses.callSite`

```scala mdoc:defaults
danglingParentheses.callSite
```

```scala mdoc:scalafmt
danglingParentheses.defnSite = false
danglingParentheses.callSite = true
maxColumn=25
---
object a {
  // defnSite
  def method(a: Int, b: String): Boolean

  // callSite
  method(argument1, argument2)
}
```

#### `danglingParentheses.exclude`

> Since v2.5.0.

When the appropriate `danglingParentheses` flag (e.g., `defnSite`) has been set,
this parameter can be used to limit contexts where dangling is applied
(currently, `class`, `trait` and `def` are supported).

```scala mdoc:defaults
danglingParentheses.exclude
```

```scala mdoc:scalafmt
continuationIndent.defnSite = 2
danglingParentheses.defnSite = true
danglingParentheses.exclude = [def]
---
def other(a: String, b: String)(c: String, d: String) = a + b + c
other(a, b)(c, d)
```

### `newlines.topLevelStatements`

> Since v2.5.0.

This parameter, together with its companions below, controls whether to
enforce a blank line before and/or after a top-level statement spanning
a certain number of lines.

> This parameter will not cause any blank lines to be removed.

```scala mdoc:defaults
newlines.topLevelStatements
```

```scala mdoc:scalafmt
newlines.topLevelStatements = []
---
import org.scalafmt
package core { // no newline added here
  class C1 {}
  object O { // nor here
    val x1 = 1
    val x2 = 2
    def A = "A"
    def B = "B"
  } // nor here
  class C2 {}
}
```

```scala mdoc:scalafmt
newlines.topLevelStatements = [before]
---
import org.scalafmt
package core {
  class C1 {}
  object O {
    val x1 = 1
    val x2 = 2
    def A = "A"
    def B = "B"
  }
  class C2 {}
}
```

```scala mdoc:scalafmt
newlines.topLevelStatements = [after]
---
import org.scalafmt
package core {
  class C1 {}
  object O {
    val x1 = 1
    val x2 = 2
    def A = "A"
    def B = "B"
  }
  class C2 {}
}
```

```scala mdoc:scalafmt
newlines.topLevelStatements = [before,after]
---
import org.scalafmt
package core {
  class C1 {}
  object O {
    val x1 = 1
    val x2 = 2
    def A = "A"
    def B = "B"
  }
  class C2 {}
}
```

### `newlines.topLevelStatementsMinBreaks`

> Since v2.5.0.

This parameter sets the minimum of line breaks between the first and last
line of a top-level statement (i.e., one less than the number of lines
the statement spans). For instance, `newlines.topLevelStatementsMinBreaks=0`
will apply to all top-level statements, whereas 1 will require at least one
line break (or a multi-line statement).

```scala mdoc:defaults
newlines.topLevelStatementsMinBreaks
```

```scala mdoc:scalafmt
newlines.topLevelStatements = [before,after]
newlines.topLevelStatementsMinBreaks = 0
---
package core {
  object O {
    val x1 = 1
    val x2 = 2
    def A =
      "A"
    def B = {
      "B"
    }
  }
}
```

```scala mdoc:scalafmt
newlines.topLevelStatements = [before,after]
newlines.topLevelStatementsMinBreaks = 2
---
import org.scalafmt
package core {
  object O {
    val x1 = 1
    val x2 = 2
    def A =
      "A"
    def B = {
      "B"
    }
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

### `newlines.afterCurlyLambda`

```scala mdoc:defaults
newlines.afterCurlyLambda
```

```scala mdoc:scalafmt
newlines.afterCurlyLambda = never
---
something.map { x =>



  f(x)
}

something.map { x => f(x) }
```

```scala mdoc:scalafmt
newlines.afterCurlyLambda = always
---
something.map { x =>



  f(x)
}

something.map { x => f(x) }
```

```scala mdoc:scalafmt
newlines.afterCurlyLambda = preserve
---
something.map { x =>



  f(x)
}

something.map { x => f(x) }
```

```scala mdoc:scalafmt
newlines.afterCurlyLambda = squash
---
something.map { x =>



  f(x)
}

something.map { x => f(x) }
```

### Newlines around `implicit` parameter list modifier

> Since v2.5.0.

```scala mdoc:defaults
newlines.implicitParamListModifier
```

#### Before

> If set, forces newline before `implicit`. Otherwise, newline can still be
> added if the keyword would overflow the line.

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifier = [before]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### After

> If set, forces newline after `implicit`. Otherwise, newline can still be added
> unless `before` is true, or the entire implicit parameter list fits on a line,
> or config style is false.

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifier = [after]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Both before and after

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifier = [before,after]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Neither before nor after

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifier = []
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### With `optIn.configStyleArguments`

```scala mdoc:scalafmt
maxColumn = 60
optIn.configStyleArguments = true
newlines.implicitParamListModifier = [after]
---
def format(code: String, age: Int)(
   implicit ev: Parser, c: Context
): String
```

### `newlines.afterInfix`

> Since v2.5.0.

This parameter (and its companions) controls formatting around infix
expressions.

> The default value depends on `newlines.source` (see below).

#### `newlines.afterInfix=keep`

This approach preserves line breaks in the input. This is the original
behaviour, and default for `newlines.source=classic,keep`.

#### `newlines.afterInfix=many,some`

These approaches _completely ignore_ existing newlines around infix, always use
a space before an infix operator and occasionally break after it. `some` is
default for `newlines.source=fold`, and `many` for `newlines.source=unfold`.

> Might require increasing runner limits
> (`runner.optimizer.maxVisitsPerToken`, possibly even `runner.maxStateVisits`),
> to avoid _SearchStateExploded_ exceptions.

`some` will introduce fewer line breaks than `many`. Both will attempt to break after
[higher-precedence operators](https://scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations),
and both will _always_ break before an expression enclosed in matching parentheses.

#### `newlines.afterInfixMaxCountPerFile`

```scala mdoc:defaults
newlines.afterInfixMaxCountPerFile
```

If the total number of infix operations in the _entire file_ exceeds
`newlines.afterInfixMaxCountPerFile`, the formatter automatically switches to
`newlines.afterInfix=keep` for this file.

#### `newlines.afterInfixMaxCountPerExprForSome`

```scala mdoc:defaults
newlines.afterInfixMaxCountPerExprForSome
```

If `newlines.afterInfix` is set to `some` and the number of infix operations in a
_given expression sequence_ (top-level or enclosed in parens/braces) exceeds
`newlines.afterInfixMaxCountPerExprForSome`, the formatter switches to `many`
for that sequence only.

#### `newlines.afterInfixBreakOnNested`

```scala mdoc:defaults
newlines.afterInfixBreakOnNested
```

If enabled, will force line breaks around a nested parenthesized
sub-expression in a multi-line infix expression.

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

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
---
List(1, 2, 3).map { x => x + 1 }
```

Entire power of `RedundantBraces` can be accessed with
`newlines.afterCurlyLambda=squash`. It will try to squash lambda body in one
line and then replace braces with parens:

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
newlines.afterCurlyLambda=squash
---
List(1, 2, 3).map { x =>
  x + 1
}

List(1, 2, 3).map { x =>
  println("you can't squash me!")
  x + 1
}
```

Configuration options and default values:

// TODO(olafur): multiline defaults

- `rewrite.redundantBraces.maxLines = 100`
- `rewrite.redundantBraces.includeUnitMethods = true`
- `rewrite.redundantBraces.methodBodies = true`
- `rewrite.redundantBraces.stringInterpolation = true`
- `rewrite.redundantBraces.generalExpressions = false` (disabled by default due
  to #1147)
- `rewrite.redundantBraces.parensForOneLineApply`
  - since v2.4.2
  - by default, `true` in edition 2020-01
  - turns `foo { bar => baz }` into `foo(bar => baz)`

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

### Trailing commas

> See [SIP](https://docs.scala-lang.org/sips/trailing-commas.html)

The rule handles how trailing commas are treated in case of a dangling closing
delimiter (parenthesis or bracket for definitions or invocations, brace for
import statements only).

Regardless of the setting, trailing commas are always removed if the closing
delimiter is not dangling (i.e., follows the final argument without a line
break).

> This logic is not triggered via the `rewrite.rules` parameter, but by setting
> `trailingCommas`.

```scala mdoc:defaults
trailingCommas
```

#### Trailing commas: `never`

Makes sure there are no trailing commas:

```scala mdoc:scalafmt
trailingCommas = never
---
import a.{
  b,
  c,
}
def method1(
  a: Int,
  b: Long,
) = {}
def method2(
  a: Int,
  b: Long*,
) = {}
def method3(
  a: Int,
) = {}
method1(
  a,
  b,
)
method2(
  a,
  b: _*,
)
method3(
  a,
)
```

#### Trailing commas: `preserve`

Keeps any trailing commas:

```scala mdoc:scalafmt
trailingCommas = preserve
---
import a.{
  b,
  c,
}
def method1(
  a: Int,
  b: Long,
) = {}
def method2(
  a: Int,
  b: Long*
) = {}
def method3(
  a: Int,
) = {}
method1(
  a,
  b
)
method2(
  a,
  b: _*,
)
method3(
  a,
)
```

#### Trailing commas: `always`

Makes sure there are trailing commas:

```scala mdoc:scalafmt
trailingCommas = always
---
import a.{
  b,
  c
}
def method1(
  a: Int,
  b: Long
) = {}
def method2(
  a: Int,
  b: Long*
) = {}
def method3(
  a: Int
) = {}
method1(
  a,
  b
)
method2(
  a,
  b: _*
)
method3(
  a
)
```

#### Trailing commas: `multiple`

> Since v2.5.0.

Makes sure there are trailing commas for multiple-argument expressions only,
except when the last argument is repeated:

```scala mdoc:scalafmt
trailingCommas = multiple
---
import a.{
  b,
  c
}
def method1(
  a: Int,
  b: Long
) = {}
def method2(
  a: Int,
  b: Long*,
) = {}
def method3(
  a: Int,
) = {}
method1(
  a,
  b
)
method2(
  a,
  b: _*,
)
method3(
  a,
)
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

### `verticalMultiline.excludeDanglingParens`

> This parameter has been deprecated, please use
> [danglingParentheses.exclude](#danglingparenthesesexclude). Keep in mind,
> though, that the new parameter is empty by default while the old one isn't, so
> to use empty exclude list, one must set the old
> `verticalMultiline.excludeDanglingParens=[]`.

```scala mdoc:defaults
verticalMultiline.excludeDanglingParens
```

```scala mdoc:scalafmt
continuationIndent.defnSite = 2
verticalMultiline.excludeDanglingParens = [def]
verticalMultiline.atDefnSite = true
verticalMultiline.arityThreshold = 2
verticalMultiline.newlineAfterOpenParen = true
---
def other(a: String, b: String)(c: String, d: String) = a + b + c
other(a, b)(c, d)
```

### Vertical multiline with `implicit` parameter lists

> Also see the general section on
> [implicit parameter lists](#newlines-around-implicit-parameter-list-modifier).

#### Before only

```scala mdoc:scalafmt
maxColumn = 60
verticalMultiline.atDefnSite = true
newlines.implicitParamListModifier = [before]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### After only

```scala mdoc:scalafmt
maxColumn = 60
verticalMultiline.atDefnSite = true
newlines.implicitParamListModifier = [after]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Before and after

```scala mdoc:scalafmt
maxColumn = 60
verticalMultiline.atDefnSite = true
newlines.implicitParamListModifier = [before,after]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

## Disabling or customizing formatting

### For code block

There is a possibility to override scalafmt config for a specific code with
`// scalafmt: {}` comment:

```scala mdoc:scalafmt
---
// scalafmt: { align.preset = most, danglingParentheses.preset = false }
libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % scalametaV,
  "org.scalacheck" %% "scalacheck" % scalacheckV)

// scalafmt: { align.preset = some, danglingParentheses.preset = true } (back to defaults)
libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % scalametaV,
  "org.scalacheck" %% "scalacheck" % scalacheckV)
```

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

### `fileOverride`

> Since v2.5.0.

Allows specifying an additional subset of parameters for each file matching
a [PathMatcher](https://docs.oracle.com/javase/8/docs/api/java/nio/file/FileSystem.html#getPathMatcher-java.lang.String-)
pattern. For instance,

```
align.preset = none
fileOverride {
  "glob:**/*.sbt" {
    align.preset = most
  }
  "glob:**/src/test/scala/**/*.scala" {
    maxColumn = 120
    binPack.unsafeCallSite = true
  }
}
```

uses `align.preset=none` for all files except `.sbt` for which `align.preset=most`
will apply. It will also use different parameters for test suites.

> This parameter does not modify which files are formatted.

## Literals

> Since v2.5.0.

Scalafmt allows flexible configuration of [Integer](https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html#integer-literals)
and [Floating Point](https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html#integer-literals) literals formatting.

Default formatting:

```scala mdoc:scalafmt
---
123l
0XFff
0x1Abl
10E-1
10e-1D
```

Each `literals.*` setting has three available options:
`Upper`, `Lower`, `Unchanged`.

### `literals.long`

```scala mdoc:defaults
literals.long
```

Responsible for the case of `Long` literals suffix `L`

```scala mdoc:scalafmt
literals.long=Upper
---
123l
```

### `literals.float`

```scala mdoc:defaults
literals.float
```

Responsible for the case of `Float` literals suffix `F`

```scala mdoc:scalafmt
literals.float=Lower
---
42.0F
```

### `literals.double`

```scala mdoc:defaults
literals.double
```

Responsible for the case of `Double` literals suffix `D`

```scala mdoc:scalafmt
literals.double=Lower
---
42.0d
```

### `literals.hexPrefix`

```scala mdoc:defaults
literals.hexPrefix
```

Responsible for the case of hex integer literals prefix `0x`

```scala mdoc:scalafmt
literals.hexPrefix=Lower
---
0X123
```

### `literals.hexDigits`

```scala mdoc:defaults
literals.hexDigits
```

Responsible for the case of hex integer literals digits

```scala mdoc:scalafmt
literals.hexDigits=Lower
literals.long=Upper
---
0xaAaA
0xaAaAl
```

### `literals.scientific`

```scala mdoc:defaults
literals.scientific
```

Responsible for the case of `Double` literals exponent part

```scala mdoc:scalafmt
literals.scientific=Upper
literals.float=Lower
---
10e-1
10e-1f
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

### `optIn.forceBlankLineBeforeDocstring`

If true, always insert a blank line before docstrings;  
If false, preserves blank line only if one exists before.

```scala mdoc:defaults
optIn.forceBlankLineBeforeDocstring
```

```scala mdoc:scalafmt
optIn.forceBlankLineBeforeDocstring = true
---
object Stuff {
  /** Some function */
  def hello = ()
}
```

```scala mdoc:scalafmt
optIn.forceBlankLineBeforeDocstring = false
---
object Stuff {
  /** Some function */
  def hello = ()
}
```

### `rewriteTokens`

Map of tokens to rewrite. For example, Map("⇒" -> "=>") will rewrite unicode
arrows to regular ascii arrows.

```scala mdoc:defaults
rewriteTokens
```

```scala mdoc:scalafmt
rewriteTokens = {
  "⇒": "=>"
  "→": "->"
  "←": "<-"
}
---
val tuple = "a" → 1
val lambda = (x: Int) ⇒ x + 1
for {
  a ← Option(1)
  b ← Option(2)
} yield a + b
```

## Edition

Editions are no longer used. They're kept for backwards compatibility with old
configuration files but new changes to the default Scalafmt formatting behavior
will not respect the `edition` setting.

## Other

To find all available configuration options, it's best to browse the source code
of Scalafmt. A good place to start is `ScalafmtConfig`. Observe that this
listing below is the top-level, there are more configuration options if you
visited nested fields like `spaces` and `newlines`.

```scala mdoc:defaults:all

```
