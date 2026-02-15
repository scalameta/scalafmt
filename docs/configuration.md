---
id: configuration
title: Configuration
---

> If you have very specific preferences for configuring your formatting,
> please make sure you have read this **entire** document, and identified every single
> parameter available, along with their interactions.
>
> Many parameters do not have ultimate authority and might be overridden by
> (possibly, default) values of other parameters.

Configuration for scalafmt is defined in a plain text file `.scalafmt.conf`
using [HOCON](https://github.com/lightbend/config) syntax.

Here is an example `.scalafmt.conf`:

```scala config
version = @STABLE_VERSION@    // mandatory
runner.dialect = scala213    // mandatory, see below for available dialects

// here are some examples of optional settings
align.preset = more    // For pretty alignment.
maxColumn = 1234
```

> 🚧 Before using specific configuration make sure that your project agrees on the standards. Settings such as `maxColumn` could be a source of issues if different tools such as an IDE uses a different default value.

## Most popular

### `maxColumn`

```scala mdoc:defaults
maxColumn
```

- Keep in mind that 80 characters fit perfectly on a split laptop screen with
  regular resolution.
- GitHub mobile view only shows 80 characters and sometimes you might review
  code on your phone.
- Consider refactoring your code before choosing a value above 100.

### `assumeStandardLibraryStripMargin`

This parameter simply says the `.stripMargin` method was not redefined by the
user to assign special meaning to indentation preceding the `|` character.
Hence, that indentation can be modified.

```scala mdoc:defaults
assumeStandardLibraryStripMargin
align.stripMargin
```

If `true`, lines starting with the margin character `|` (or another if specified
in the `.stripMargin(...)` call) will be indented differently.

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

## Version

The `version` parameter specifies the release of the formatter to be used.
If the version requested is different from the version of the installed
formatter, the correct release will be downloaded dynamically.

> Since v3.1.0, the `version` parameter is required to be specified explicitly.

Since that requires access to artifact repositories, please see more on that on the
[Installation](installation.md#using-custom-repositories-with-cli) page.

## Scala Dialects

The formatter supports various dialects defined and supported by `scalameta` parser. However,
for any given file, one and only one dialect is supported, and `runner.dialect` must be
used to select it.

> Since v3.1.0, the `runner.dialect` parameter is required to be specified explicitly.

Available dialects are:

- Scala 2
  - `scala211`
  - `scala212`
  - `scala212source3`
  - `scala213`
  - `scala213source3`
- Scala 3
  - `scala3`: most recent release
  - `Scala3Future`: scala3 plus experimental, unreleased features
  - specific versions from `Scala30` to `Scala36`
- sbt-specific
  - `sbt0137`
  - `sbt1`

You can also specify `runner.dialect` for a subset of files using [fileOverride](#fileoverride):

```
fileOverride {
  "glob:**/scala3-subproject/src/main/scala/**" {
    runner.dialect = scala3
  }
}
```

### `runner.dialectOverride`

> Since v3.6.0

Using this section, you can explicitly set
[scalameta Dialect fields](https://github.com/scalameta/scalameta/blob/main/scalameta/dialects/shared/src/main/scala/scala/meta/Dialect.scala)
(directly or via their associated `withXxxYyy` methods).

```scala mdoc:scalafmt
runner.dialect = scala213
runner.dialectOverride.withAllowToplevelTerms = true
runner.dialectOverride.withAllowEndMarker = true
runner.dialectOverride.allowSignificantIndentation = true
---
// top-level def: unsupported by scala213
def foo = // significant indent: unsupported by scala213
  bar
  baz
end foo // end marker: unsupported by scala213
```

### `runner.dialectFeatures`

This setting contains a list of language features which might affect formatting. Some features
will automatically be detected if the source file contains, at the top level, an appropriate
import statement `import scala.language.XXX` (exactly like this, no relative imports).

Features include:

- `relaxedLambdaSyntax` (since v3.10.7):
  this feature allows formatting of fewer-braces lambdas on a single line
  (see [SIP-75](https://docs.scala-lang.org/scala3/reference/experimental/relaxed-lambdas.html))
  - automatic detection expects the `experimental.relaxedLambdaSyntax` language feature

```scala mdoc:scalafmt
runner.dialect = scala3
---
// will not be a one-liner, dialect doesn't allow
foo.map: x => x + 1
```

```scala mdoc:scalafmt
runner.dialect = scala3
---
// will not be a one-liner, import allows but is relative
import language.experimental.relaxedLambdaSyntax
foo.map: x => x + 1
```

```scala mdoc:scalafmt
runner.dialect = scala3
runner.dialectFeatures = [relaxedLambdaSyntax]
---
// will be a one-liner, config allows
foo.map: x =>
  x + 1
```

```scala mdoc:scalafmt
runner.dialect = scala3
---
// will be a one-liner, import allows
import scala.language.experimental.relaxedLambdaSyntax
foo.map: x =>
  x + 1
```

### Scala 3

Since v3.0.0, `scalafmt` supports Scala 3 features that can be enabled by changing
the dialect of the `scalameta` parser.

```
runner.dialect = scala3
---
open class MyOpenClass()
val myQuote = '{ expr }
val mySplice = ${ expr }
enum class Weekday {
  case Monday, Tuesday, Wednesday, Thursday, Friday
}
```

Please also see [rewrite rules](#scala3-rewrites) for Scala 3.

### Scala 2 with `-Xsource:3`

Also since v3.0.0, if using `-Xsource:3` option for Scala 2 compiler, you can change the
dialect to `Scala213Source3`, which will allow to format some of the new syntax backported
from Scala 3.

## Presets

Some sections provide preset values to set multiple parameters at once. These
are always accessed via the `preset` key of the appropriate section, including
top-level.

### Top-level presets

#### `preset=default`

This preset is implicit and sets all values to their defaults.

#### `preset=IntelliJ`

This preset is defined as

```
    preset = default
    indent.defnSite = 2
    optIn.configStyleArguments = false
```

#### `preset=defaultWithAlign`

This preset is defined as

```
    preset = default
    align.preset = more
```

#### `preset=Scala.js`

This preset intends to approximate the
[style used for `scala.js`](https://github.com/scala-js/scala-js/blob/main/CODINGSTYLE.md).

It uses modified detection of [config-style formatting](#newlinesconfigstylexxxsiteprefer):

- [according to Sébastien Doeraene](https://github.com/scala-js/scala-js/pull/4522#issuecomment-879168123),
  config-style should be driven solely by presence of a dangling closing parenthesis
- to achieve that, use a combination of
  [`danglingParentheses.xxxSite = false`](#danglingparenthesescallsite) and
  [`newlines.configStyle.xxxSite.prefer = true`](#newlinesconfigstylexxxsiteprefer)

The preset itself is defined as:

```
    preset = default
    binPack.preset = true
    align {
      openParenCtrlSite = false
      arrowEnumeratorGenerator = false
      tokens = [ caseArrow ]
    }
    danglingParentheses {
      callSite = false
      defnSite = false
    }
    docstrings.style = Asterisk
    indent.callSite = 4
    newlines {
      avoidInResultType = true
      neverBeforeJsNative = true
      sometimesBeforeColonInMethodReturnType = false
    }
    runner.optimizer {
      callSite {
        minSpan = 500
        minCount = 5
      }
      defnSite {
        minSpan = 500
        minCount = 5
      }
    }
```

### Appending to preset collections

If, instead of redefining the default or preset value of a list or a map parameter,
you'd like to append to it, use the following syntax:

```
// set
a.b.c.list = [ ... ]
a.b.c.dict = { ... }

// append; "+" must be the only key
a.b.c.list."+" = [ ... ]
a.b.c.dict."+" = { ... }
```

## Indentation

### `indent.main`

> Since v3.0.0.

This parameter controls the primary code indentation. Various context-specific
overrides are defined below, within this section.

```scala mdoc:defaults
indent.main
```

### `indent.significant`

> Since v3.0.0.

This parameter controls the amount of significant indentation used when
[optional braces](https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html)
rules apply.

By default, equals to `indent.main`.

```scala mdoc:scalafmt
runner.dialect = scala3
indent.main = 2
indent.significant = 3
---
object a {
  if (foo)
    bar
  else
    baz

  if foo then
    bar
    bar
  else
    baz
    baz
}
```

### `indent.callSite`

This parameter controls indentation of method call arguments.

```scala mdoc:defaults
indent.callSite
```

Example:

```scala mdoc:scalafmt
indent.callSite = 2
---
function(
argument1, // indented by 2
""
)
```

### `indent.binPackCallSite`

This parameter controls indentation of method call arguments when they are
binpacked (that is, spanning multiple lines but not formatted using config
style). Defaults to `indent.callSite`.

> Since v3.8.3.

Example:

```scala mdoc:scalafmt
newlines.configStyle.callSite.prefer = false
danglingParentheses.callSite = false
binPack.callSite = always
indent.callSite = 2
indent.binPackCallSite = 4
---
func(1, // next indented by 4
  "")
func( // next indented by 2
    1,
    "")
```

### `indent.ctrlSite`

This parameter controls indentation within control expressions (`if/while/etc`).
If not set, the value of `indent.callSite` applies.

> Since v3.0.0.

```scala mdoc:scalafmt
indent.callSite = 2
indent.ctrlSite = 4
---
if (
 foo && // indented by 4
 bar
) {
  baz
}
```

```scala mdoc:scalafmt
indent.callSite = 2
---
if (
 foo && // indented by 2
 bar
) {
  baz
}
```

### `indent.defnSite`

```scala mdoc:defaults
indent.defnSite
```

Same as `indent.callSite` except for definition site. Example:

```scala mdoc:scalafmt
indent.defnSite = 4
---
def function(
parameter1: Type1 // indented by 4
): ReturnType
```

### `indent.binPackDefnSite`

This parameter controls indentation of method parameters when they are
binpacked (that is, spanning multiple lines but not formatted using config
style). Defaults to `indent.defnSite`.

> Since v3.8.3.

Example:

```scala mdoc:scalafmt
newlines.configStyle.defnSite.prefer = false
danglingParentheses.defnSite = false
binPack.defnSite = always
indent.defnSite = 2
indent.binPackDefnSite = 4
---
def func(a: Int, // next indented by 4
  b: String)
def func( // next indented by 2
    a: Int,
    b: String)
```

### `indent.ctorSite`

> Since v2.5.0.

Applies to constructors. Defaults to `indent.defnSite`.

```scala mdoc:scalafmt
indent.ctorSite = 4
indent.defnSite = 2
---
class A(
 field1: Type1 // indented by 4
) {
 def function2(
  parameter1: Type1 // indented by 2
 ): ReturnType = None
}
```

### `indent.matchSite`

> Since v3.4.4.

If set, applies custom indentation to `case` clauses in `match` expressions.

```scala mdoc:defaults
indent.matchSite
```

```scala mdoc:scalafmt
maxColumn = 20
indent.matchSite = 0
runner.dialect = scala3
---
object a:
  x match
    case _: Aaaaaa |
      _: Bbbbbb |
      _: Cccccc =>
  end match
```

### `indent.caseSite`

> Since v3.0.0.

Applies indentation to patterns in `case` clauses.

```scala mdoc:defaults
indent.caseSite
```

```scala mdoc:scalafmt
maxColumn = 20
indent.caseSite = 5
---
x match {
  case _: Aaaaaa |
      _: Bbbbbb |
      _: Cccccc =>
}
```

### `indent.extendSite`

```scala mdoc:defaults
indent.extendSite
```

This parameter defines indentation used for the `extends A with B` or `derives A, B` sequences
in a template (class, trait, object, enum, etc.).

```scala mdoc:scalafmt
indent.extendSite = 4
maxColumn = 20
---
trait Foo extends A {
  def foo: Boolean = true
}
```

### `indent.withSiteRelativeToExtends`

> Since v2.5.0.

This parameter defines _additional_ indentation used for the `with` elements of an
`extends A with B` sequence in a template.

```scala mdoc:defaults
indent.withSiteRelativeToExtends
```

```scala mdoc:scalafmt
indent.extendSite = 4
indent.withSiteRelativeToExtends = 2
maxColumn = 30
---
trait Foo extends A with B with C with D with E {
  def foo: Boolean = true
}
```

### `indent.commaSiteRelativeToExtends`

> Since v3.0.0

This parameter defines _additional_ indentation used for the post-comma elements
of an `extends A, B` or `derives A, B` sequences of a template.

Added to support Scala 3, which allows to specify multiple parents with a comma.

```scala mdoc:defaults
indent.commaSiteRelativeToExtends
```

```scala mdoc:scalafmt
runner.dialect = scala3
indent.extendSite = 4
indent.commaSiteRelativeToExtends = 4
maxColumn = 20
---
trait Foo extends A, B, C, D, E {
  def foo: Boolean = true
}
```

### `indent.extraBeforeOpenParenDefnSite`

> Since v3.0.0

This parameter applies to definitions and sets extra indentation (relative to
the indentation of the body) used for parameter groups when

- the definition has a body (that needs differentiating from)
- [newlines.beforeOpenParenDefnSite](#newlinesbeforeopenparenxxxsite) is set

```scala mdoc:defaults
indent.extraBeforeOpenParenDefnSite
```

```scala mdoc:scalafmt
maxColumn = 25
newlines.beforeOpenParenDefnSite = fold
---
case class fooClass
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String)
abstract class fooClass
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String) {
  def fooDef
    (foo1: String)
    (foo2: String, foo3: String)
    (foo5: String)
}
def fooDef
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String) = {
  // body
}
```

```scala mdoc:scalafmt
maxColumn = 25
indent.extraBeforeOpenParenDefnSite = 2
newlines.beforeOpenParenDefnSite = fold
---
case class fooClass
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String)
abstract class fooClass
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String) {
  def fooDef
    (foo1: String)
    (foo2: String, foo3: String)
    (foo5: String)
}
def fooDef
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String) = {
  // body
}
```

```scala mdoc:scalafmt
maxColumn = 25
indent.extraBeforeOpenParenDefnSite = -1
newlines.beforeOpenParenDefnSite = fold
---
case class fooClass
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String)
abstract class fooClass
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String) {
  def fooDef
    (foo1: String)
    (foo2: String, foo3: String)
    (foo5: String)
}
def fooDef
  (foo1: String)
  (foo2: String, foo3: String)
  (foo5: String) = {
  // body
}
```

#### `indent.relativeToLhsLastLine`

When the left-hand side of an infix or `match` expression is itself broken over
several lines, with the last line indented relative to the first line, this flag
determines whether the indent is relative to the first or the last line.

This parameter takes a list of values including:

- `match`: applies to match expressions
- `infix`: applies to infix expressions

```scala mdoc:defaults
indent.relativeToLhsLastLine
```

```scala mdoc:scalafmt
indent.relativeToLhsLastLine = []
---
foo // c1
  .bar match {
    case baz => qux
  }
foo // c1
  .bar infix {
    case baz => qux
  }
```

```scala mdoc:scalafmt
indent.relativeToLhsLastLine = [match, infix]
---
foo // c1
  .bar match {
  case baz => qux
}
foo // c1
  .bar infix {
  case baz => qux
}
```

#### `indent.fewerBraces`

This parameter controls whether extra `indent.main` is added
to the sole argument of a method call using the "fewer braces"
syntax. The following values are supported:

- `always`: always applies extra indent
- `never`: doesn't apply any extra indent
- `beforeSelect`: applies extra indent only to fewer-braces
  expressions followed by a `.select`

```scala mdoc:defaults
indent.fewerBraces
```

> In Scala 3.3.0, only `never` provides compiler-compatible code.
> Other options will work in 3.3.1-RC1 and later
> (see [Parser section](https://github.com/lampepfl/dotty/releases/tag/3.3.1-RC1)).
> Also, `never` is implicitly forced if indentation width is less than 2.

```scala mdoc:scalafmt
runner.dialect = Scala3
indent.significant = 3
indent.fewerBraces = always
---
bar:
  2 + 2

foo.bar:
  2 + 2

foo:
  2 + 2
.bar:
  3 + 3
.baz // c
.qux
```

```scala mdoc:scalafmt
runner.dialect = Scala3
indent.significant = 3
indent.fewerBraces = never
---
bar:
  2 + 2

foo.bar:
  2 + 2

foo:
  2 + 2
.bar:
  3 + 3
.baz // c
.qux
```

```scala mdoc:scalafmt
runner.dialect = Scala3
indent.significant = 3
indent.fewerBraces = beforeSelect
---
bar:
  2 + 2

foo.bar:
  2 + 2

foo:
  2 + 2
.bar:
  3 + 3
.baz // c
.qux
```

### `indent.afterInfixSite`

This parameter controls indentation after an infix operator (not including
assignments). Defaults to `indent.main`.

> Since v3.8.3.

### Indent for `binPack.callSite`

Normally, even when binpacking, there's a new level of indentation added for
each opening parenthesis starting a nested argument clause (regardless whether
the first argument follows the opening parenthesis on the same line or on a
separate one); the parameters below modify this behaviour.

See also [`indent.binPackCallSite`](#indentbinpackcallsite).

#### `binPack.indentCallSiteOnce`

When this parameter is enabled, only one level is added to the outermost call,
regardless of the number of nested parentheses.

```scala mdoc:defaults
binPack.indentCallSiteOnce
```

With the parameter enabled:

```scala mdoc:scalafmt
binPack.callSite = true
binPack.indentCallSiteOnce = true
indent.callSite = 2
maxColumn = 20
---
foo(bar1(baz1(qux1, qux2), baz2), bar2(baz3, baz4))
```

With the parameter disabled:

```scala mdoc:scalafmt
binPack.callSite = true
binPack.indentCallSiteOnce = false
indent.callSite = 2
maxColumn = 20
---
foo(bar1(baz1(qux1, qux2), baz2), bar2(baz3, baz4))
```

#### `binPack.indentCallSiteSingleArg`

When this parameter is disabled, no indentation is added for same-line single-arg
cases; the assumption is that if the argument expression spans multiple lines,
it will introduce its own indentation.

```scala mdoc:defaults
binPack.indentCallSiteSingleArg
```

With the parameter enabled:

```scala mdoc:scalafmt
binPack.callSite = true
binPack.indentCallSiteSingleArg = true
indent.callSite = 2
maxColumn = 20
---
foo(bar(baz.qux(xyz + zyx)))
foo(bar((_, _) =>
  baz { qux =>
    noop
  } baz { qux =>
    noop
  } baz //
    { qux =>
      noop
    } baz { qux =>
      noop
    }))
```

With the parameter disabled:

```scala mdoc:scalafmt
binPack.callSite = true
binPack.indentCallSiteSingleArg = false
indent.callSite = 2
maxColumn = 20
---
foo(bar(baz.qux(xyz + zyx)))
foo(bar((_, _) =>
  baz { qux =>
    noop
  } baz { qux =>
    noop
  } baz //
    { qux =>
      noop
    } baz { qux =>
      noop
    }))
```

### `indent.infix`

> Prior to v3.8.4, this section was called `indentOperator`.
> Since v3.10.4, it takes a list of alternative configurations.

Normally, the first eligible break _inside_ a chain of infix operators is
indented by 2.

This group of parameters allows overriding which infix operators, and in which
context, are eligible to be exempted from this, with indentation _omitted_.

This overall setting is a list of alternative entries:

- each entry defines several constraints, and at least one entry must have all
  of its constraints satisfied for an infix operator to be exempted
- therefore, if you wish to disable this functionality, set `indent.infix = []`
- if you have only one entry, you can specify `indent.infix { ... }` instead of
  `indent.infix = [ { ... } ]`

```scala mdoc:defaults
indent.infix
```

#### `indent.infix`: `exemptScope`

Added in 3.4.0, this parameter determines when an infix operator can be exempted from applying
continuation indentation. Since 3.10.4, it takes multiple values, and expects each to apply.
If you have only one value, you may choose to specify `exemptScope = <value>`
instead of `exemptScope = [ <value> ]`.
If the list is empty, this constraint is satisfied, and any otherwise eligible infix operator
is exempted.

It accepts the following values, to determine the context in which infix operators are eligible
to be exempted from the default indentation rule:

- `oldTopLevel` (default): "top-level" infix operators
  - the definition of top-level historically refers to infix expressions whose direct parent is
    a block (typically as the last statement in that block), `if/while` (as condition or body),
    or case clause (as pattern or body)
  - this value replaced deprecated `indentOperator.topLevelOnly=true`
  - this approach is also somewhat inconsistent with what it was intended to accomplish, and
    kept only for backwards compatibility; please consider using one of the alternatives
- `aloneEnclosed`: infix operators which are enclosed in braces or parens as the only statement
  in a block or body of a braces-enclosed lambda function; an `if/while` condition; the only
  argument of a method call; or similar;
  - it also includes a few scenarios where parens can be omitted, such case clause patterns,
    conditions in new scala3 `if-then` and `while-do` syntax, etc.
  - however, block braces are not optional
- `aloneArgOrBody`: infix operators as an `if/while` condition; an argument of a method call; the
  only statement in a block; entire body of an assignment, case clause, control statement, etc;
  - it is intended to help implement a requirement of the
    [scala-js coding style](https://github.com/scala-js/scala-js/blob/main/CODINGSTYLE.md#long-expressions-with-binary-operators).
- `all`: all infix operators
  - this value replaced deprecated `indentOperator.topLevelOnly=false`
- `notAssign` (since v3.8.4): any non-assignment operator
  - this value expanded upon deprecated `verticalAlignMultilineOperators`
    which now simply maps to `{ exemptScope = notAssign, excludeRegex = ".*" }`
- `notWithinAssign` (since v3.8.4): any infix not part of a larger assignment expression

```scala mdoc:scalafmt
indent.infix = [{ exemptScope = [oldTopLevel] }]
---
function(
  a &&
    b,
  a &&
    b
)
function(a &&
    b)(a &&
    b)
function(
  a &&
    b
)(
  a &&
    b
)
function {
  a &&
    b
}
```

```scala mdoc:scalafmt
indent.infix = [{ exemptScope = [all] }]
---
function(
  a &&
    b,
  a &&
    b
)
function(a &&
    b)(a &&
    b)
function(
  a &&
    b
)(
  a &&
    b
)
function {
  a &&
    b
}
```

```scala mdoc:scalafmt
indent.infix = [{ exemptScope = [aloneEnclosed] }]
---
function(
  a &&
    b,
  a &&
    b
)
function(a &&
    b)(a &&
    b)
function(
  a &&
    b
)(
  a &&
    b
)
function {
  a &&
    b
}
```

```scala mdoc:scalafmt
indent.infix = [{ exemptScope = [aloneArgOrBody] }]
---
function(
  a &&
    b,
  a &&
    b
)
function(a &&
    b)(a &&
    b)
function(
  a &&
    b
)(
  a &&
    b
)
function {
  a &&
    b
}
```

#### `indent.infix`: `excludeRegex`

Defines a regular expression for excluded infix operators. If an eligible
operator matches, it will not be indented.

In v3.1.0, this parameter was renamed from `indentOperator.exclude`.

#### `indent.infix`: `includeRegex`

Defines a regular expression for included infix operators. If an eligible
operator matches and is not excluded explicitly by
[indent.infix.excludeRegex](#indentinfixexcluderegex), it be will indented.

In v3.1.0, due to conflict with built-in HOCON keyword, this parameter was
renamed from `indentOperator.include`.

#### `indent.infix` presets

- `default`
  - use defaults for all fields
- `spray` (also `akka`)
  - set `indent.infix = [{ excludeRegex = "^$"` }]
    and `indent.infix = [{ includeRegex = "^.*=$"` }]

## Alignment

Alignment describes formatting which inserts additional spaces to align certain
tokens on different lines vertically.

Apart from a few special cases, the way alignment works is as follows:

- for each line, alignment stops are identified, by looking up each token in
  `align.tokens` (matching token, owner and, if specified, the owner's parent)
- for two candidate lines, respective alignment stops are compared (first stop
  on one line to the first one on the other, etc); the only exception are the
  single-line comments which are compared regardless of their stop position
- two tokens will match if:
  - both tokens have the same token category; a token's category is the value
    associated with its type in `align.tokenCategory` mapping or, if missing,
    its type
  - both owners have the same tree category; similarly, a tree's category is the
    value for its type in `align.treeCategory` mapping or the type itself
  - both owners belong to the same "statement container"; this is determined
    internally and usually selects the nearest containing block, template,
    match, argument or parameter group.
- for each token that has matches in the surrounding lines:
  - we'll determine the amount of extra space needed to be added _before_
    that token, to align it _on the right_ with matching tokens
  - however, if there was no space before the token, and `align.delayUntilSpace`
    is set, that extra space will be added to the next space on its line, thus
    aligning subsequent token _on the left_.

Align has several nested fields, which you can customize. However, it comes with
four possible presets: none, some, more, & most.

### `align.preset`

Default: **some**

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

> Starting with the introduction of `align.stripMargin` parameter in v2.5.0, one
> must explicitly enable it to get earlier behaviour of `align.preset=none`. See
> [assumeStandardLibraryStripMargin](#assumestandardlibrarystripmargin).

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
maxColumn = 80
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

> **Note**. Only for the truest vertical aligners.

### `align.tokens`

Default: **[caseArrow]**

An align token contains a `code` (the string literal of an operator of token) and a list of
`owners`; each owner entry in turn contains an optional `regex` (the kind of the closest tree
node that owns that token), and a list of `parents` (to match the tree containing the owner of
the token).

> To find the `owner` part for a custom tree, look for its type prefix using
> [Scastie Playground](https://scalameta.org/docs/trees/scastie.html) or
> [AST Explorer](https://scalameta.org/docs/trees/astexplorer.html).

> The special code `//` is used for single-line comments. Also, since v3.3.1, this
> includes multi-line comments `/* ... */` which do not themselves contain newlines
> but are followed by one (i.e., can trivially be changed to a `//` comment).

```scala mdoc:scalafmt
align.tokens = [{
  code = "=>"
  owners = [{
    regex = "Case"
  }]
}]
---
x match {
  case 1 => 1 -> 2
  case 11 => 11 -> 22
}
```

```scala mdoc:scalafmt
align.tokens = [
  {
    code = "%"
    owners = [{
      regex = "Term.ApplyInfix"
    }]
  }, {
    code = "%%"
    owners = [{
      regex = "Term.ApplyInfix"
    }]
  }
]
---
val x = List(
"org.scala-lang" %% "scala-compiler" % scalaVersion.value,
"com.lihaoyi" %% "sourcecode" % "0.1.1"
)
```

```scala mdoc:scalafmt
align.tokens."+" = [{
  code = ":"
  owners = [{
    regex = "Term\\.Param"
    parents = [ "Ctor\\.Primary" ]
  }]
}]
---
case class Foo(
  firstParam: Int,
  secondParam: String,
  thirdParam: Boolean
) {
  def Foo(
    firstParam: Int,
    secondParam: String,
    thirdParam: Boolean
  ) = ???
}
```

```scala mdoc:scalafmt
align.tokens."+" = [{
  code = ":"
  owners = [{
    parents = [ "Defn\\." ]
  }]
}]
---
case class Foo(
  firstParam: Int,
  secondParam: String,
  thirdParam: Boolean
) {
  def Foo(
    firstParam: Int,
    secondParam: String,
    thirdParam: Boolean
  ) = ???
}
```

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

### `align.closeParenSite`

This parameter controls whether to align the closing parentheses _when_ we
aligned the opening one (see `openParenXxxSite` parameters below) and the
respective `danglingParentheses.xxxSite` is set.

> Since v3.0.0.

```scala mdoc:defaults
align.closeParenSite
```

```scala mdoc:scalafmt
align.closeParenSite = true
align.openParenCallSite = true
danglingParentheses.callSite = true
---
function(arg1, // align by (
    arg2,
 arg3)
```

```scala mdoc:scalafmt
align.closeParenSite = false
align.openParenCallSite = true
danglingParentheses.callSite = true
---
function(arg1, // align by (
    arg2,
 arg3)
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

### `align.openBracketCallSite`

> Since v3.0.4.

If set explicitly, will be used for the left bracket in type arguments,
instead of [`align.openParenCallSite`](#alignopenparencallsite).

### `align.openParenCtrlSite`

This parameter controls alignment after `(` in `if/while/for`.

```scala mdoc:defaults
align.openParenCtrlSite
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

### `align.openBracketDefnSite`

> Since v3.0.4.

If set explicitly, will be used for the left bracket in type parameters,
instead of [`align.openParenDefnSite`](#alignopenparendefnsite).

### `align.openParenTupleSite`

This parameter controls aligning of tuples to their opening parenthesis. If not
specified, will use the value of `align.openParenCallSite`.

> Since v3.0.0.

```scala mdoc:scalafmt
maxColumn = 10
align.openParenCallSite = false
align.openParenTupleSite = true
---
object a {
  foo(bar, baz)
  (bar, baz)
}
```

```scala mdoc:scalafmt
maxColumn = 10
align.openParenCallSite = true
---
object a {
  foo(bar, baz)
  (bar, baz)
}
```

### `align.beforeOpenParenXxxSite`

Aligns parameter groups (not parameters within a group) if using
[`newlines.beforeOpenParenXxxSite`](#newlinesbeforeopenparenxxxsite).
Requires [`align.closeParenSite`](#aligncloseparensite).

> Since v3.3.2.

```scala mdoc:defaults
align.beforeOpenParenCallSite
align.beforeOpenParenDefnSite
```

### `align.stripMargin`

See [assumeStandardLibraryStripMargin](#assumestandardlibrarystripmargin).

```scala mdoc:defaults
align.stripMargin
```

This functionality is enabled in all presets except `align.preset=none` where it
was disabled since the parameter's introduction in v2.5.0.

### `align.multiline`

This flag controls whether to include multiline statements in the search for
tokens to align, with the following values:

- `false`: align tokens only on adjacent lines
- `true`: align consecutive statements even if tokens to align are not on adjacent lines

> Since v2.5.0.

```scala mdoc:defaults
align.multiline
```

```scala mdoc:scalafmt
align.preset = more
align.multiline = true
---
for {
  a <- aaa
  bbb <- bb
  cccccc <- c {
    3
  }
  dd <- ddddd
} yield ()
```

```scala mdoc:scalafmt
align.preset = more
align.multiline = false
---
for {
  a <- aaa
  bbb <- bb
  cccccc <- c {
    3
  }
  dd <- ddddd
} yield ()
```

### `align.allowOverflow`

If this flag is set, as long as unaligned lines did not overflow, we will not check
whether alignment causes any lines to overflow the [`maxColumn`](#maxcolumn) setting.

> Since v3.0.0.

```scala mdoc:defaults
align.allowOverflow
```

It's also enabled by default in `align.preset = most`.

### `align.inInterpolation`

If this flag is set, and breaks within interpolated code are allowed
(see [`newlines.inInterpolation`](#newlinesininterpolation), then the
interpolated code and the closing `}` will be indented relative to the opening `${`.

> Since v3.4.0.

```scala mdoc:defaults
align.inInterpolation
```

Keep in mind that this option might lead to line overflow via "stacking":

```scala mdoc:scalafmt
maxColumn = 30
align.inInterpolation = true
newlines.inInterpolation = oneline
---
object a {
  s"""
    |foo1 ${quxQux(bazBaz, barBar)} foo2 ${quxQux(bazBaz, barBar)} foo3 ${quxQux(bazBaz, barBar)} foo4
    |""".stripMargin
}
```

vs

```scala mdoc:scalafmt
maxColumn = 30
align.inInterpolation = false
newlines.inInterpolation = oneline
---
object a {
  s"""
    |foo1 ${quxQux(bazBaz, barBar)} foo2 ${quxQux(bazBaz, barBar)} foo3 ${quxQux(bazBaz, barBar)} foo4
    |""".stripMargin
}
```

### `align.delayUntilSpace`

If this flag is set, the formatter will not forcefully pull apart two successive
non-whitespace tokens that would otherwise be formatted without a space between
them.

Instead, the extra alignment spaces will be added to the next space on the same line.

> Since v3.7.13. Prior to that, this behaviour was always enabled.

```scala mdoc:defaults
align.delayUntilSpace
```

```scala mdoc:scalafmt
align.preset = more
align.delayUntilSpace = true
align.tokens."+" = [ { code = ":" }, { code = "(" }, { code = ")" }, { code = "=" } ]
---
object a {
  def meeethod1(pram1: AnyRef): Any = ???
  def methd2(paaaaaram2: Any): Any = ???
  def meth3(param333333: Any): Any = ???
  def md4(param4: Any): Any = ???
}
```

vs

```scala mdoc:scalafmt
align.preset = more
align.delayUntilSpace = false
align.tokens."+" = [ { code = ":" }, { code = "(" }, { code = ")" }, { code = "=" } ]
---
object a {
  def meeethod1(pram1: AnyRef): Any = ???
  def methd2(paaaaaram2: Any): Any = ???
  def meth3(param333333: Any): Any = ???
  def md4(param4: Any): Any = ???
}
```

## Newlines

The `newlines.*` options are used to configure when and where `scalafmt` should
insert newlines.

> You might be interested in the [Vertical Multiline](#vertical-multiline)
> section.

### `newlines.source`

> Since v2.5.0.

This parameter controls the general approach to line breaks, and whether to take
into account existing newlines in the source. The default value (if the
parameter is not specified) is the _classic_, original way. Below are the
alternatives.

#### `newlines.source=keep`

This approach attempts to preserve line breaks in the input whenever possible.

> Keep in mind that other parameters _will_ take precedence if they require
> (or prohibit) breaks in specific places (and that includes their default
> values).
>
> For instance, default for [`binPack.callSite`](#binpackxxxsite)
> will not allow multiple arguments per line in a multiline expression.

#### `newlines.source=fold,unfold`

These two approaches _completely ignore_ existing line breaks, except around
comments and blank lines (i.e., multiple consecutive newlines).

> Might require increasing
> [optimizer limits](#route-search-optimizations-giving-up),
> to avoid _SearchStateExploded_ exceptions.

`fold` attempts to remove line breaks whenever possible resulting in a more
horizontal, or vertically compact look.

`unfold`, on the other hand, is intended for those who prefer a more vertical,
or horizontally compact look.

Both settings attempt to play nice with other parameters, but some combinations
are prohibited and will result in an error.

### `newlines.topLevelStatementBlankLines`

> Since v3.0.0.

This parameter controls when to add blank lines before and/or after a top-level
statement, defined as:

- a member of a package or template (or extension group within a template)
- nesting is allowed if parent is also a top-level statement

Special cases:

- the rules do _not_ directly apply to package statements at the top of the source file; however,
  if this parameter is non-empty, there will be at least one blank line before the first
  non-package statement, and possibly more if the rules match that statement
- end markers are handled through a setting for the statement they mark
- imports and exports are processed as a group of consecutive statements
- also see [Newlines around package or template body](#newlines-around-package-or-template-body)
- if `allowNonTop` is enabled, this rule will also apply to some other statements

> This parameter might reduce the number of blank lines but will not eliminate
> them completely unless corresponding value is negative.

Each entry on this list consists of the following fields (except `blanks`, all are used to match
whether the rule should apply):

- `regex`
  - a regular expression to match the type of the statement
  - if unspecified, will match all valid statements
  - see [align.tokens](#aligntokens) for instructions on how to find the type
- `parents` (since v3.10.4):
  - a list of regular expressions to match the parent of the statement
  - if unspecified or empty, matches any
- `maxNest` and (since v3.1.2) `minNest`
  - basically, limits indentation level (not actual indentation) of a statement
  - unindented statements (under source-level unindented package) have
    nest level of 0, those under them are 1 etc.
  - if unspecified, will match any nesting level
- `minBreaks` (default: 1)
  - sets the minimum number of line breaks between the first and last line of
    a statement (i.e., one less than the number of lines the statement spans).
  - for instance, `minBreaks=0` will apply to all statements, whereas 1 will
    require at least one line break (that is, a multi-line statement).
- (since v3.8.3) `minBlankGaps` and `maxBlankGaps`
  - sets the range on the number of blank-line gaps (regardless of the number of
    lines in each gap) that would have been [output](#advanced-formatting-process)
    prior to this rule.
  - if a limit is unspecified, will not constrain on the corresponding end
- (since v3.10.4) `allowNonTop` (default: false)
  - allows applying this particular entry to:
    - a top-level statement that has a non-top-level parent
    - any matching statement if `regex` or `parents` are specified
- `blanks`
  - if omitted while the entry matches, serves to exclude another entry
  - `before`: number of lines to be added before a matching statement; if
    negative (v3.0.1), lines will be removed unless before a comment
  - `after`: number of lines to be added after a matching statement
    - for instance, if a `package` matches, this controls how many lines need
      to be added after _all statements_ of a package, not after the first
      line which declares the package name
    - same logic as above applies to negative values
  - `beforeEndMarker`:
    - end markers themselves will not be matched against any rule; blanks before
      them will come from `beforeEndMarker` and blanks after from `after`
  - `beforeAll` and `afterAll` (v3.0.1): if set explicitly, replaces the
    respective `before` or `after` value before the first or after the last
    statement of a template or indented package; otherwise, the `before` or
    `after` value will be capped at 1
  - can be specified as a single integer, to set just `before` and `after` to
    the same value:

```
// these two are equivalent
newlines.topLevelStatementBlankLines = [
  { blanks { before = 1, after = 1, beforeEndMarker = 0 } }
]
newlines.topLevelStatementBlankLines = [
  { blanks = 1 }
]
```

If you'd like to override or exclude some cases, add them explicitly:

```
newlines.topLevelStatementBlankLines = [
  { maxNest = 0, blanks = 2 } // uses 2 blanks for all unindented statements
  { regex = "^Import" } // excludes import groups; blanks are not specified
]

```

> If multiple entries match a statement, an entry with the lowest `minBreaks`
> will be selected. Since we'll be adding lines, this will increase the span
> of the statement and might potentially lead to another entry, with a higher
> `minBreaks`, to match as well, which is undesirable.

```scala mdoc:defaults
newlines.topLevelStatementBlankLines
```

```scala mdoc:scalafmt
newlines.topLevelStatementBlankLines = []
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
newlines.topLevelStatementBlankLines = [
  {
    blanks { before = 1 }
  }
]
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
newlines.topLevelStatementBlankLines = [
  {
    blanks { after = 1 }
  }
]
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
newlines.topLevelStatementBlankLines = [
  {
    blanks = 1
  }
]
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
newlines.topLevelStatementBlankLines = [
  {
    minBreaks = 0
    blanks = 1
  }
]
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
newlines.topLevelStatementBlankLines = [
  {
    minBreaks = 2
    blanks = 1
  }
]
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

### Newlines around package or template body

> Since v3.0.0.

This group of parameters controls whether to enforce a blank line before the first
or after the last statement of a package or template (i.e., body of a class, object,
trait, enum).

> These parameters will not cause any blank lines to be removed.

#### `newlines.topLevelBodyIfMinStatements`

`topLevelBodyIfMinStatements` can be `before` and/or `after`, while
`topLevelBodyMinStatements` limits when the rule is applied.

```scala mdoc:defaults
newlines.topLevelBodyIfMinStatements
newlines.topLevelBodyMinStatements
```

```scala mdoc:scalafmt
newlines.topLevelBodyIfMinStatements = []
---
import org.scalafmt
package core {
  class C1 {
    def one = 1
  }
  object O1 {
    val one = 1
    def two = 2
  }
  class C2 {}
}
```

```scala mdoc:scalafmt
newlines.topLevelBodyIfMinStatements = [before]
---
import org.scalafmt
package core {
  class C1 {
    def one = 1
  }
  object O1 {
    val one = 1
    def two = 2
  }
  class C2 {}
}
```

```scala mdoc:scalafmt
newlines.topLevelBodyIfMinStatements = [after]
---
package core {
  class C1 {
    def one = 1
  }
  object O1 {
    val one = 1
    def two = 2
  }
  class C2 {}
}
```

```scala mdoc:scalafmt
newlines.topLevelBodyIfMinStatements = [before,after]
---
import org.scalafmt
package core {
  class C1 {
    def one = 1
  }
  object O1 {
    val one = 1
    def two = 2
  }
  class C2 {}
}
```

#### `newlines.beforeTemplateBodyIfBreakInParentCtors`

This parameter will force a blank line before the first statement of a template body if the
token _before_ `extends` and the `{` (or, in scala3, `:`) token are not on the same line.

```scala mdoc:defaults
newlines.beforeTemplateBodyIfBreakInParentCtors
```

```scala mdoc:scalafmt
newlines.source = keep
newlines.beforeTemplateBodyIfBreakInParentCtors = true
---
package core {
  class C1 extends S { // no breaks between "C1" and "{"
    def one = 1
  }
  class C1(
    param: Int
  ) extends S { // no breaks between ")" and "{"
    def one = 1
  }
  class C1 extends S { // no breaks between "C1" and "=>"
    self =>
    def one = 1
  }
  class C1
    extends S { // break between "C1" and "{"
    def one = 1
  }
}
```

```scala mdoc:scalafmt
newlines.source = keep
newlines.beforeTemplateBodyIfBreakInParentCtors = false
---
package core {
  class C1 extends S { // no breaks between "C1" and "{"
    def one = 1
  }
  class C1(
    param: Int
  ) extends S { // no breaks between ")" and "{"
    def one = 1
  }
  class C1 extends S { // no breaks between "C1" and "=>"
    self =>
    def one = 1
  }
  class C1
    extends S { // break between "C1" and "{"
    def one = 1
  }
}
```

### `newlines.beforeMultiline`

> Since v2.7.0

This parameter controls whether to force a new line before a multi-line body of
`case/if/def/val` and how to format it if the space is allowed. (For
additional control with assignment expressions, please also see
[`newlines.forceBeforeMultilineAssign`](#newlinesforcebeforemultilineassign) below.)

It accepts the same values as [`newlines.source`](#newlinessource) (and defaults
to that parameter's setting).

NB: for breaks before parameters of a multi-line lambda, use `multiline` with
[`newlines.beforeCurlyLambdaParams`](#newlinesbeforecurlylambdaparams).

```scala mdoc:scalafmt
newlines.beforeMultiline = unfold
---
a match {
  // had space after "=>"
  case a => if (step != 0)
      d.name should be("dir" + step)
  // had newline after "=>"
  case a =>
    if (step != 0)
      d.name should be("dir" + step)
}
```

```scala mdoc:scalafmt
newlines.beforeMultiline = fold
---
a match {
  // had space after "=>"
  case a => if (step != 0)
      d.name should be("dir" + step)
  // had newline after "=>"
  case a =>
    if (step != 0)
      d.name should be("dir" + step)
}
```

```scala mdoc:scalafmt
newlines.beforeMultiline = keep
---
a match {
  // had space after "=>"
  case a => if (step != 0)
      d.name should be("dir" + step)
  // had newline after "=>"
  case a =>
    if (step != 0)
      d.name should be("dir" + step)
}
```

```scala mdoc:scalafmt
# newlines.beforeMultiline = classic
---
a match {
  // had space after "=>"
  case a => if (step != 0)
      d.name should be("dir" + step)
  // had newline after "=>"
  case a =>
    if (step != 0)
      d.name should be("dir" + step)
}
```

### `newlines.forceBeforeMultilineAssign`

> Since v3.0.0

This section controls whether to force a break before a multi-line body of an
assignment expression unless it can be formatted on a single line (or is enclosed
in braces). By default, the rule is disabled. It takes precedence
over `newlines.beforeMultiline` settings.

It can take the following values:

- `never`: the rule is disabled
- `any`: applies to any assignment expression (`def`, assignment to
  a `var`, default value of a method parameter, etc.)
- `def`: applies only to definitions which can potentially be parameterized
  (`def`, `macro`, `given` alias, etc.)
- `anyMember`: applies to members of a `class/trait/object`
- `topMember`: applies to members of a `class/trait/object` which itself
  can only be nested within a sequence of `class/trait/object` definitions

It replaces deprecated `newlines` parameters `beforeMultilineDef=unfold` and
`alwaysBeforeMultilineDef=true` which, if this parameter is not set,
map to `def`.

```scala mdoc:scalafmt
maxColumn = 17
newlines.forceBeforeMultilineAssign = def
---
class A {
  // break, allows params (even if it doesn't define any)
  def foo = func(foo, bar)
  // no break, doesn't allow params
  val foo = func(foo, bar)
  def foo = {
    def a = func(foo, bar)
    val a = func(foo, bar)
  }
}
```

```scala mdoc:scalafmt
maxColumn = 19
newlines.forceBeforeMultilineAssign = topMember
---
class A {
  class B {
    // break, a top member
    def foo = func(foo, bar)
    // break, a top member
    val foo = func(foo, bar)
  }
  def foo = {
    // no break, not a member
    def a = func(foo, bar)
    // no break, not a member
    val a = func(foo, bar)
    new A with B {
      // no break, not a top member
      def foo = func(foo, bar)
      // no break, not a top member
      val foo = func(foo, bar)
    }
  }
}
```

```scala mdoc:scalafmt
maxColumn = 17
newlines.forceBeforeMultilineAssign = never
---
// all disabled, no breaks
class A {
  def foo = func(foo, bar)
  val foo = func(foo, bar)
  def foo = {
    def a = func(foo, bar)
    val a = func(foo, bar)
  }
}
```

### `newlines.forceBeforeAssign`

> Since v3.5.9

```scala mdoc:defaults
newlines.forceBeforeAssign
```

This parameter takes precedence over
[`newlines.forceBeforeMultilineAssign`](#newlinesforcebeforemultilineassign)
and uses the same values. The difference is, the rule forces a newline before a
matching assignment expression whether or not it can be formatted on a single line.

### `newlines.beforeTypeBounds`

> Since v3.0.0

This parameter controls formatting of bounds of type parameters: upper `<:`,
lower `>:`, view `<%`, and context `:` bounds. It accepts the
same values as [`newlines.source`](#newlinessource).

- `classic`: simply allows no line breaks (default; can't be specified)
- `keep`: preserves a no-break if the next bound fits on the line
- `fold`: uses a no-break if the next bound fits on the line
- `unfold`: puts all bounds on the same line, or breaks before each

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

### `newlines.beforeCurlyLambdaParams`

This parameter controls whether a newline is forced between the opening curly
brace and the parameters of a lambda or partial function. Added in 2.7.0,
replacing boolean `alwaysBeforeCurlyBraceLambdaParams` (removed in 3.4.0).

In 3.4.10, `beforeParenLambdaParams` was added to allow overriding this parameter
specifically for lambdas in parentheses.

```scala mdoc:defaults
newlines.beforeCurlyLambdaParams
```

```scala mdoc:scalafmt
newlines.beforeCurlyLambdaParams = never
---
// should keep one-line
x.map { x => s"${x._1} -> ${x._2}" }
x.map { case (c, i) => s"$c -> $i" }
// should break on arrow since case doesn't fit on a line
x.zipWithIndex.map { case (c, i) => s"$c -> $i" }
x.zipWithIndex.map { case (c, i) => s"$c -> $i (long comment)" }
```

```scala mdoc:scalafmt
newlines.beforeCurlyLambdaParams = always
---
// should break on brace, though fits on the same line
x.map { x => s"${x._1} -> ${x._2}" }
x.map { case (c, i) => s"$c -> $i" }
x.zipWithIndex.map { case (c, i) => s"$c -> $i (long comment)" }
// should break on brace and arrow as lambda doesn't fit on a line
x.zipWithIndex.map { x => s"${x._1} -> ${x._2} (long comment)" }
```

```scala mdoc:scalafmt
newlines.beforeCurlyLambdaParams = multiline
---
// should keep one-line
x.map { x => s"${x._1} -> ${x._2}" }
x.map { case (c, i) => s"$c -> $i" }
// should break on brace as lambda doesn't fit on the same line
x.zipWithIndex.map { x => s"${x._1} -> ${x._2}" }
x.zipWithIndex.map { case (c, i) => s"$c -> $i" }
// should break on brace and arrow as lambda doesn't fit on a line
x.zipWithIndex.map { x => s"${x._1} -> ${x._2} (long comment)" }
x.zipWithIndex.map { case (c, i) => s"$c -> $i (long comment)" }
```

```scala mdoc:scalafmt
newlines.beforeCurlyLambdaParams = multilineWithCaseOnly
---
// should keep one-line
x.map { x => s"${x._1} -> ${x._2}" }
x.map { case (c, i) => s"$c -> $i" }
// should break after arrow as lambda doesn't fit on the same line
x.zipWithIndex.map { x => s"${x._1} -> ${x._2}" }
x.zipWithIndex.map { x => s"${x._1} -> ${x._2} (long comment)" }
// should break on brace as lambda doesn't fit on the same line
x.zipWithIndex.map { case (c, i) => s"$c -> $i" }
// should break on brace and arrow as lambda doesn't fit on a line
x.zipWithIndex.map { case (c, i) => s"$c -> $i (long comment)" }
```

### `newlines.afterCurlyLambdaParams`

This parameter controls handling of newlines after the arrow following the
parameters of a curly brace lambda or partial function, and whether a space can
be used for one-line formatting of the entire function body (if allowed but the
body doesn't fit, a break is always forced).

This parameter was renamed in 2.7.0 from `afterCurlyLambda`, for clarity and
consistency with `beforeCurlyLambdaParams` defined above.

```scala mdoc:defaults
newlines.afterCurlyLambdaParams
```

```scala mdoc:scalafmt
newlines.afterCurlyLambdaParams = squash
---
// remove all blank lines if any
// one-line formatting is allowed
something.map { x =>



  f(x)
}

something.map { x => f(x) }
```

```scala mdoc:scalafmt
newlines.afterCurlyLambdaParams = never
---
// remove all blank lines if any
// one-line formatting depends on newlines.source:
// yes for fold; no for unfold; otherwise, only if there was no break
something.map { x =>



  f(x)
}

something.map { x => f(x) }
```

```scala mdoc:scalafmt
newlines.afterCurlyLambdaParams = keep
---
// if blank lines are present, keep only one
// one-line formatting depends on newlines.source:
// if no blank for fold; no for unfold; otherwise, only if there was no break
something.map { x =>



  f(x)
}

something.map { x => f(x) }
```

```scala mdoc:scalafmt
newlines.afterCurlyLambdaParams = always
---
// ensure a single blank line
// one-line formatting is not allowed
something.map { x =>



  f(x)
}

something.map { x => f(x) }
```

### `newlines.implicitParamListModifierXXX`

These parameters control newlines around `implicit` parameter list modifier.

> Since v2.5.0.

#### Newlines around `using` parameter and argument list modifier

`using` soft keyword was introduced in Scala 3 and is supported in `scalafmt`.
Besides parameter lists, `using` can also be used with argument lists hence
provided rules will then also be applied to them.

> Since v3.0.0.

The parameters defined below can also be accessed via their alternative names where
`implicit` is replaced with `using` (for instance, `newlines.usingParamListModifierPrefer`).
Whichever naming you use, formatting will be applied to both `implicit` and `using`.

#### Prefer After (default)

> Prefers newline after `implicit`. Newline will be added unless the entire
> implicit parameter list fits on a line, or config style is false. Newline can
> also be added _before_ if the keyword itself would overflow the line.

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifierPrefer = after
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Prefer Before

> Prefers newline before `implicit`. Newline will not be added if the entire
> implicit parameter list fits on a line.

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifierPrefer = before
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Force Before

> If set, forces newline before `implicit` (even if the parameter list would
> fit on one line). Otherwise, newline can still be added if the keyword would
> overflow the line.

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifierForce = [before]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Force After

> If set, forces newline after `implicit` (even if the parameter list would
> fit on one line). Otherwise, newline can still be added unless `before` is
> true, or the entire implicit parameter list fits on a line, or config style
> is false.

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifierForce = [after]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Force both before and after

```scala mdoc:scalafmt
maxColumn = 60
newlines.implicitParamListModifierForce = [before,after]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Implicit with `newlines.configStyle.defnSite.prefer`

While config-style normally requires a newline after the opening parenthesis,
postponing that break until after the `implicit` keyword is allowed if other
parameters require keeping this keyword attached to the opening brace.

Therefore, any of the parameters described in this section will take precedence
even when `newlines.configStyle.defnSite.prefer = true` is used.

### `newlines.infix`

Introduced in v3.8.4, this parameter (and its companions) controls formatting
around infix expressions. It contains the following parameter groups:

- `termSite`
  - primarily applies to ordinary infix expressions (`Term.ApplyInfix`)
- `typeSite`:
  - applies to infix expressions within types (`Type.ApplyInfix`) only
  - defaults to `termSite` if _no fields_ are specified
- `patSite`:
  - applies to infix expressions within patterns (`Pat.ExtractInfix`) only
  - defaults to `termSite` if _no fields_ are specified

Each of these groups has several parameters of its own (replacing deprecated
`newlines.afterInfixXxx`, originally added in v2.5.0, which moved to `termSite`):

- `style`:
- `maxCountPerFile`
- `maxCountPerFileForKeep`
- `maxCountPerExprForSome`
- `breakOnNested`

#### `newlines.infix: style=none,keep`

`none` (renamed from `keep` in v3.10.4) makes no attempt to alter breaks around
infix operators (preserving both existence and absence of a line break in the
input). This is the original behaviour, and default for `newlines.source=classic`.

`keep` (consistently with other parameters having this value as an option, and
default for `newlines.source=keep`), since v3.10.4 preserves only the existence
of a line break but reserves the right to introduce additional breaks after some
infix operators (that is, exactly as `some` would have done).

One caveat is: for `classic` type infixes with Scala3 (or if the dialect
[enables](#runnerdialectoverride) the `useInfixTypePrecedence` flag),
`some` is the default.

#### `newlines.infix: style=many,some`

These approaches _completely ignore_ existing newlines around infix, always use
a space before an infix operator and occasionally break after it. `some` is
default for `newlines.source=fold` (also see caveat under `keep` aboce), and
`many` for `newlines.source=unfold`.

> Might require increasing
> [optimizer limits](#route-search-optimizations-giving-up),
> to avoid _SearchStateExploded_ exceptions.

`some` will introduce fewer line breaks than `many`. Both will attempt to break
after
[higher-precedence operators](https://scala-lang.org/files/archive/spec/2.11/06-expressions.html#infix-operations),
and both will _always_ break before an expression enclosed in matching
parentheses.

#### `newlines.infix: maxCountPerFile`

```scala mdoc:defaults
newlines.infix.termSite.maxCountPerFile
```

If the total number of matching infix operations in the _entire file_ exceeds
`newlines.infix.xxxSite.maxCountPerFile`, the formatter automatically switches to
`newlines.infix.xxxSite.style=none` for this file.

Otherwise, if the number of infix expressions exceeds `maxCountPerFileForKeep`,
the formatter automatically switches to `newlines.infix.xxxSite.style=keep`.
This optional parameter was added in v3.10.4.

#### `newlines.infix: maxCountPerExprForSome`

```scala mdoc:defaults
newlines.infix.termSite.maxCountPerExprForSome
```

If `newlines.infix.xxxSite.style` is set to `some` and the number of infix operations in
a _given expression sequence_ (top-level or enclosed in parens/braces) exceeds
`newlines.infix.xxxSite.maxCountPerExprForSome`, the formatter switches to `many`
for that sequence only.

#### `newlines.infix: breakOnNested`

```scala mdoc:defaults
newlines.infix.termSite.breakOnNested
```

If enabled, will force line breaks around a nested parenthesized sub-expression
in a multi-line infix expression.

This logic, however, will not apply if the precedence of the inner expression
is lower than the outer; that is, we _had_ to put it in parentheses, not because
we _chose_ to, for readability. This is to prevent breaking like this:

```scala
// original
(a + b) * c
(a + b) < c

// formatted
(a + b) *
c // ok to force
(a + b) <
c // not ok by default
```

### `newlines.avoidForSimpleOverflow`

A list parameter (of comma-separated flags), with possible flags described
below. These flags relax formatting rules to allow occasional line overflow
(i.e., when line exceeds `maxColumn`) in simple cases instead of introducing a
newline.

```scala mdoc:defaults
newlines.avoidForSimpleOverflow
```

#### `newlines.avoidForSimpleOverflow=[tooLong]`

> Since v2.6.0.

This flag tries to avoid introducing a newline if the line would overflow even
with a newline.

```scala mdoc:scalafmt
maxColumn = 50
danglingParentheses.callSite = false
newlines.avoidForSimpleOverflow = [tooLong]
---
object Example {
  foo_bar_baz("the quick brown fox jumps over the lazy dog") {
    println("")
  }
  foo_bar_baz("the quick brown fox jumps over a dog") {
    println("")
  }
}
```

#### `newlines.avoidForSimpleOverflow=[punct]`

> Since v2.6.0.

This flag tries to avoid a newline if the line would overflow only because of
trailing punctuation (non-alphanum symbols of length 1).

With the flag set:

```scala mdoc:scalafmt
maxColumn = 80
newlines.avoidForSimpleOverflow = [punct]
---
class Engine[TD, EI, PD, Q, P, A](
    val dataSourceClassMap: Map[
      String,
      Class[_ <: BaseDataSource[TD, EI, Q, A]]]) {}
```

#### `newlines.avoidForSimpleOverflow=[slc]`

> Since v3.0.0.

This flag tries to avoid a newline if the line would overflow only because of
trailing single-line comment (one which starts with `//`).
Also, since v3.3.1, this includes a trailing `/* ... */` without embedded breaks.

```scala mdoc:scalafmt
maxColumn = 40
newlines.avoidForSimpleOverflow = [slc]
---
intercept[TestException] {
  val ct = Thread.currentThread() // comment
}
```

```scala mdoc:scalafmt
maxColumn = 40
newlines.avoidForSimpleOverflow = []
---
intercept[TestException] {
  val ct = Thread.currentThread() // comment
}
```

### `newlines.avoidInResultType`

If true, newlines in definition result type will only be used if formatting
without them is impossible. This parameter was added in 2.7.0, replacing
`neverInResultType`.

```scala mdoc:defaults
newlines.avoidInResultType
```

```scala mdoc:scalafmt
maxColumn = 40
newlines.avoidInResultType = true
newlines.neverBeforeJsNative = true
---
// no newlines in result type
def permissionState(a: A = js.native): js.Promise[PushPermissionState] = js.native
// no newlines in result type
val permissionState: js.Promise[PushPermissionState] = js.native
// can't format without newlines
implicit protected val td: TildeArrow {
  type Out = RouteTestResult } = TildeArrow.injectIntoRoute
```

### `newlines.sometimesBeforeColonInMethodReturnType`

If true, in some rare cases a newline could be added before the colon which
delimits the return type in a method signature.

Normally, this will only happen if formatting otherwise will lead to line
overflow or the return type spanning too many lines.

Also, see [newlines.beforeOpenParenXxxSite](#newlinesbeforeopenparenxxxsite) below.

```scala mdoc:defaults
newlines.sometimesBeforeColonInMethodReturnType
```

```scala mdoc:scalafmt
maxColumn = 40
newlines.sometimesBeforeColonInMethodReturnType = true
---
def someVeryLongMethodName: Map[String, String] = ???
```

### `newlines.beforeOpenParenXxxSite`

This parameter, if enabled, causes a parameter group which doesn't fit on the
same line to be formatted separately, with a newline just before the opening
parenthesis.

> Since v3.0.0.

Use `null` (default) to disable it. Otherwise, takes the same values as
[newlines.source](#newlinessource) (or explicit `source` to mirror the current
value of `newlines.source`).

Specific formatting depends on the value: for `keep`, will preserve a newline;
for `unfold`, will format every parameter group separately unless all fit on a
single line; `fold` will use a more compact formatting.

- `beforeOpenParenDefnSite` applies to definitions (methods, ctors, macros, etc.)
- `beforeOpenParenCallSite` applies to invocations (method calls) and is only
  supported for scala3

Additional nuances:

- if `newlines.sometimesBeforeColonInMethodReturnType` is true, a newline will
  be added before the colon unless the entire signature fits on a line (except
  when set to `keep`).
- if the corresponding `align.openParenXxxSite` is true, multi-line parameters
  will start on the same line as the opening parenthesis and align; otherwise,
  formatting will use a newline and an appropriate continuation indent.
- if the corresponding [`align.beforeOpenParenXxxSite`](#alignbeforeopenparenxxxsite)
  is true, when the first parameter group starts without a line break, subsequent
  parameter groups will be aligned to it.

```scala mdoc:defaults
newlines.beforeOpenParenDefnSite
newlines.beforeOpenParenCallSite
```

```scala mdoc:scalafmt
maxColumn = 20
runner.dialect = scala3 // for CallSite
align.openParenDefnSite = true
newlines {
  beforeOpenParenDefnSite = fold
  beforeOpenParenCallSite = unfold
  sometimesBeforeColonInMethodReturnType = true
}
---
def fooFunc(foo1: String)(foo2: String, foo3: String): String = ???
val res = fooFunc("foo1")("foo2", "foo3")
```

```scala mdoc:scalafmt
maxColumn = 20
runner.dialect = scala3 // for CallSite
align.openParenCallSite = true
newlines {
  beforeOpenParenDefnSite = unfold
  beforeOpenParenCallSite = fold
  sometimesBeforeColonInMethodReturnType = false
}
---
def fooFunc(foo1: String)(foo2: String, foo3: String): String = ???
val res = fooFunc("foo1")("foo2", "foo3")
```

### `newlines.inInterpolation`

This parameter controls how to format spliced scala code within string constants
(e.g., `s"..."`, etc). Also see [`align.inInterpolation`](#alignininterpolation).

```scala mdoc:defaults
newlines.inInterpolation
```

> Since v3.4.0.

- `allow`: allows breaks within spliced code (original)
  - this option will not prevent line overflow even if `${` is within bounds,
    because this option doesn't allow breaking right after `${`
- `avoid`: attemps to avoid breaks within the spliced code, regardless of line overflow
- `oneline`: formats the splice on a single line, or breaks after `${` if overflows

### `newlines.ignoreInSyntax`

The formatter frequently chooses between adding a newline and continuing the
same line but either prohibiting or heavily discouraging subsequent newlines
_between_ tokens, to fit the rest of the expression on the same line.

However, in many cases and, for historical reasons, intentionally, newlines
_within_ tokens have been frequently ignored, leading to "single-line" blocks
which actually span multiple lines.

This boolean parameter now allows controlling whether to ignore newlines found
in syntax of strings or other possibly multi-line tokens when newlines are
otherwise prohibited or undesirable (such as for single-line formatting).

```scala mdoc:defaults
newlines.ignoreInSyntax
```

> Since v3.7.13. Prior to that, this behaviour was always enabled.

```scala mdoc:scalafmt
newlines.ignoreInSyntax = true
---
// ignores newline in string, pretends everything fits on one line
println(s"""${1}
    """.stripMargin
)
```

```scala mdoc:scalafmt
newlines.ignoreInSyntax = false
---
// detects newline in string, forces proper multi-line formatting
println(s"""${1}
    """.stripMargin
)
```

### `newlines.annotation`

This boolean parameter controls newlines after annotations. Prior to v3.8.4,
was called `optIn.annotationNewlines`.

```scala mdoc:defaults
newlines.annotation
```

Its behaviour depends on [newlines.source](#newlinessource):

- `newlines.annotation = true`:
  - `newlines.source=fold`: allows space before another annotation
  - `newlines.source=unfold`: forces break
  - otherwise: preserves space _before_ or after an annotation
- `newlines.annotation = false`:
  - `newlines.source=fold`: allows space before a keyword or another annotation
  - `newlines.source=unfold`: allows space before another annotation
  - otherwise: allows space before a keyword

### `newlines.selfAnnotation`

If enabled, this parameter forces a break after a self annotation in a template if
`newlines.source=fold/unfold`, and preserves it otherwise.
Prior to v3.8.4, was called `optIn.selfAnnotationNewline`.

```scala mdoc:defaults
newlines.selfAnnotation
```

## Newlines: `danglingParentheses`

When enabled, these parameters will force a break before the closing parenthesis
when there's a break after the opening parenthesis (unless otherwise specified).
While this parameter is not technically under the `newlines` section, it
logically belongs there.

### `danglingParentheses.defnSite`

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

### `danglingParentheses.callSite`

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

### `danglingParentheses.bracketXxxSite`

These control the bracketed type clauses in definitions and method calls and
by default are equal to their non-bracket counterparts.

```scala mdoc:scalafmt
danglingParentheses.defnSite = true
danglingParentheses.callSite = true
danglingParentheses.bracketDefnSite = false
danglingParentheses.bracketCallSite = false
maxColumn=25
---
object a {
  // defnSite
  def method[A <: Any, B]: Boolean

  // callSite
  method[String, SomeOtherType]
}
```

```scala mdoc:scalafmt
danglingParentheses.defnSite = false
danglingParentheses.callSite = false
danglingParentheses.bracketDefnSite = true
danglingParentheses.bracketCallSite = true
maxColumn=25
---
object a {
  // defnSite
  def method[A <: Any, B]: Boolean

  // callSite
  method[String, SomeOtherType]
}
```

### `danglingParentheses.ctrlSite`

> Since v2.5.0.

Forces dangling on open/close parens around control structures (`if`, `while`,
`for`) when line breaks must occur.

For optional braces in scala3, this parameter also controls whether to break
before `then` or `do` in a multi-line condition.

```scala mdoc:defaults
danglingParentheses.ctrlSite
```

```scala mdoc:scalafmt
danglingParentheses.ctrlSite = true
maxColumn=20
---
if (something) {
  // nothing
}
if (something_else) {
  // nothing
}
```

### `danglingParentheses.importSite`

> Since v3.10.1.

```scala mdoc:defaults
danglingParentheses.importSite
```

Normally, multiple import selectors
(see [`binPack.importSelectors`](#binpackimportselectors))
are either formatted one a single line if they fit,
or newlines are added after the opening brace and before the closing one.

However, if you prefer to avoid having these newlines, some combinations
of parameters will help you achieve that. This parameter must be set to false
and one of the following conditions enabled as follows:

- [`binPack.importSelectors`](#binpackimportselectors) is`fold`
  (explicitly, or implied from [`newlines.source`](#newlinessource), or
- there's no break after the opening brace and
  - [`binPack.importSelectors`](#binpackimportselectors) is `keep`, or
  - [`binPack.importSelectors`](#binpackimportselectors) and
    [`newlines.source`](#newlinessource) are both unspecified

```scala mdoc:scalafmt
danglingParentheses.importSite = false
maxColumn = 20
binPack.importSelectors = fold
---
// should be folded, importSelectors = fold
import ref.{foo, bar, baz, qux}
// should be folded, importSelectors = fold
import ref.{
  foo, bar, baz, qux}
```

```scala mdoc:scalafmt
danglingParentheses.importSite = false
maxColumn = 20
binPack.importSelectors = unfold
---
// should NOT be folded, importSelectors = unfold
import ref.{foo, bar, baz, qux}
// should NOT be folded, importSelectors = unfold
import ref.{
  foo, bar, baz, qux}
```

```scala mdoc:scalafmt
danglingParentheses.importSite = false
maxColumn = 20
newlines.source = fold
---
// should be folded, source = fold
import ref.{foo, bar, baz, qux}
// should be folded, source = fold
import ref.{
  foo, bar, baz, qux}
```

```scala mdoc:scalafmt
danglingParentheses.importSite = false
maxColumn = 20
newlines.source = keep
---
// should be folded: no break after opening brace
import ref.{foo, bar, baz, qux}
// should NOT be folded: break after opening brace
import ref.{
  foo, bar, baz, qux}
```

```scala mdoc:scalafmt
danglingParentheses.importSite = false
maxColumn = 20
newlines.source = unfold
---
// should NOT be folded, source = unfold
import ref.{foo, bar, baz, qux}
// should NOT be folded, source = unfold
import ref.{
  foo, bar, baz, qux}
```

### `danglingParentheses.tupleSite`

This parameter controls dangling of closing parentheses in tuples. If not
specified, will use the value of `danglingParentheses.callSite`.

> Since v3.0.0.

```scala mdoc:scalafmt
danglingParentheses.tupleSite = true
---
val i =
          ( //hello scalafmt, please don't blow up
                   (1, 2),
                   1,
                   3,
                   4,
                   4)
```

```scala mdoc:scalafmt
danglingParentheses.tupleSite = false
---
val i =
          ( //hello scalafmt, please don't blow up
                   (1, 2),
                   1,
                   3,
                   4,
                   4)
```

### `danglingParentheses.exclude`

> Since v2.5.0.

When the appropriate `danglingParentheses` flag (e.g., `defnSite`) has been set,
this parameter can be used to limit contexts where dangling is applied
(currently, `class`, `trait`, `enum`, `extension` and `def` are supported).

For backwards compatibility, the default depends on whether
[Vertical Multiline](#vertical-multiline) mode is used. If it is, the default is
`[class, trait]`; otherwise, it's empty.

```scala mdoc:scalafmt
indent.defnSite = 2
danglingParentheses.defnSite = true
danglingParentheses.exclude = [def]
---
def other(a: String, b: String)(c: String, d: String) = a + b + c
other(a, b)(c, d)
```

## Newlines: Config-style formatting

This formatting applies to argument clauses in method calls or parameter
clauses in definitions. It outputs a newline after the opening parenthesis
(or after the `implicit` keyword) and a newline before the closing parenthesis,
with arguments or parameters listed one per line.

### `newlines.configStyle.xxxSite`

The `xxxSite` portion refers to:

- `fallBack` [since v3.8.4]: used if a more specific group is not defined
  - its `prefer` field replaced `optIn.configStyleArguments`, enabled by default
- `callSite` [since v3.8.4]: applies to argument clauses in method calls
  - replaced section called `newlines.configStyleCallSite`
- `bracketCallSite` [since v3.8.4]: applies to type argument clauses, falls
  back to `callSite` if not specified
- `defnSite` [since v3.8.4]: applies to parameter clauses in method or class
  definitions
  - replaced section called `newlines.configStyleDefnSite`
- `bracketDefnSite` [since v3.8.4]: applies to type parameter clauses, falls
  back to `defnSite` if not specified

### `newlines.configStyle.xxxSite.prefer`

If true, applies config-style formatting:

- `newlines.source = fold/unfold`:
  if single-line formatting is impossible
- `newlines.source = classic/keep`:
  if preference for config-style is detected in the source:
  - there's a newline present before the closing delimiter
  - if [`danglingParentheses.xxxSite = false`](#danglingparenthesescallsite)
    (and [`align.openParenXxxSite = false`](#alignopenparencallsite)):
    no other condition needs to be present (the [scala.js rule](#presetscalajs))
  - otherwise, there needs to be a newline after the opening delimiter,
    or after the `implicit` keyword if present.

Please note that other parameters might also force config-style (see below).

```scala mdoc:scalafmt
newlines.configStyle.defnSite.prefer = true
maxColumn=45
---
object a {
  // keeps single line
  def method1(a: Int, b: String): Boolean

  // forces config style
  def method2(a: Int, b: String, c: String): Boolean

  // preserves config style
  def method3(
    a: Int, b: String, c: String
  ): Boolean
}
```

### Forcing config style

When `newlines.configStyle.xxxSite.forceIfOptimized` is enabled and
[route search optimization](#route-search-optimizations-arg-or-param-clause)
is applicable to a clause, the formatter will force config-style formatting.

By default, takes the same value as
[`newlines.configStyle.xxxSite.prefer`](#newlinesconfigstylexxxsiteprefer).

```scala mdoc:scalafmt
newlines.configStyle.callSite.forceIfOptimized = true
newlines.configStyle.defnSite.forceIfOptimized = false
runner.optimizer.callSite { minSpan = 5, minCount = 2 }
maxColumn = 60
---
object a {
  // this is a definition, not a method call
  def method(a: String, b: String = null): Boolean

  // keeps single line; min offset not satisfied
  method(a, b)

  // keeps single line; min arg not satisfied
  method(SomeVeryVeryVeryVeryLongArgument)

  // forces config style
  method(foo, bar)
}
```

### `newlines.configStyle.beforeComma`

If enabled, will break before comma (instead of after) when using config style.
However, if the `runner.dialect` supports trailing commas, using
[`rewrite.trailingCommas`](#trailing-commas) is recommended (which is why,
prior to v3.8.4, this parameter was called `poorMansTrailingCommasInConfigStyle`).
`

## Rewrite Rules

To enable a rewrite rule, add it to the config like this
`rewrite.rules = [Imports]`.

### `AvoidInfix`

This rule replaces infix expressions `a op b` with proper method calls `a.op(b)`.

> NB: The rule currently does not support right-associative operators (i.e.,
> those which end in `:`) which would have had to be rewritten as `b.op(a)`.

The rule takes the following parameters under `rewrite.avoidInfix`:

- `includeFilters` and `excludeFilters`, two lists of regular expressions, which
  determine which operators are eligible for this rewrite
  - for this rule to be enabled (that is, for the rewrite to be applied), an infix
    expression must match `includeFilters` and not match `excludeFilters`
  - (since 3.8.0) if a regular expression contains `\\.`, matching will be against
    not only the infix operator but also its left-hand-side expression (with the
    non-empty operator part following the last `\\.` in the pattern)
  - (before 3.8.0) these two parameters were nested under `rewrite.neverInfix`
- `excludePlaceholderArg` (default: `true`) will not rewrite infix
  expressions if the argument is a solo placeholder (`_` or `(_: Type)`)
  - this parameter does not control any other cases with the infix argument containing a
    placeholder character; some of them will never be rewritten as adding parentheses will
    change their syntactic meaning, and others will be rewritten as usual
  - (before 3.8.0 and since 3.4.4) this parameter was named
    `rewrite.allowInfixPlaceholderArg`
- (since 3.8.0) `excludeScalaTest` controls whether the standard set of
  `scalatest` assert methods is added to `excludeFilters`
  - if unspecified, and `project.layout` determines that the file being
    formatted is not a test file, then these test assert methods will not
    be excluded
- (since 3.8.4) `excludePostfix`, unless set to `true` explicitly, will also
  apply the rule to `Term.Select` trees specified without a dot
- (since 3.8.4) `excludeMatch`, if set to `false` explicitly and if the dialect
  enables `allowMatchAsOperator` (such as Scala 3), will also apply the rule to
  `Term.Match` trees specified without a dot

```scala mdoc:scalafmt
rewrite.rules = [AvoidInfix]
rewrite.avoidInfix.excludeFilters."+" = [ "map" ]
---
a success b
a error (b, c)
a map { x =>
  x + 2
}
"o" % "a" % "v" c(D)
future map {
  case e: Err => 0
} recover (_.toString)
future recover {
  case e: Err => 0
} map (_.toString)
```

```scala mdoc:scalafmt
rewrite.rules = [AvoidInfix]
rewrite.avoidInfix.excludePlaceholderArg = false
---
_ foo _
_ bar (_: Int)
_ baz (_.qux)
_ baz _.qux // cannot be rewritten, not the same as previous line
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
`newlines.afterCurlyLambdaParams=squash`. It will try to squash lambda body in
one line and then replace braces with parens:

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
newlines.afterCurlyLambdaParams=squash
---
List(1, 2, 3).map { x =>
  x + 1
}

List(1, 2, 3).map { x =>
  println("you can't squash me!")
  x + 1
}
```

#### `RedundantBraces`: `generalExpressions`

```scala mdoc:defaults
rewrite.redundantBraces.generalExpressions
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.generalExpressions = true
---

while (x < 10) {
  x += 1
}

str match {
  case "a" => {
    println("ok")
  }
  case _ => {
    println("not ok")
  }
}
```

#### `RedundantBraces`: `ifElseExpressions`

```scala mdoc:defaults
rewrite.redundantBraces.ifElseExpressions
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.ifElseExpressions = true
---

if (a > b) {
  doSomething()
} else {
  doAnything()
}
```

#### `RedundantBraces`: `defnBodies`

This parameter takes the following values:

- `none`: rewriting of a definition body is disabled
- `all`: applies to body of any definition (`def`, `val`, `macro` etc.)
- `noParams`: applies to body of any definition which doesn't have parameters
  (e.g.: `val`; `var`; parameterless `def`, without brackets or parentheses)

In v3.3.2, this parameter superseded a boolean `methodBodies`.

```scala mdoc:defaults
rewrite.redundantBraces.defnBodies
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.defnBodies = true
---
def f() = {
  1 + 1
}
```

#### `RedundantBraces`: `includeUnitMethods`

```scala mdoc:defaults
rewrite.redundantBraces.includeUnitMethods
```

Affects only functions with **explicitly** specified `Unit` type

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.methodBodies = true
rewrite.redundantBraces.includeUnitMethods = false
---
def f() = {
  1 + 1
}

def x(): Unit = {
  println("example")
}
```

#### `RedundantBraces`: `stringInterpolation`

```scala mdoc:defaults
rewrite.redundantBraces.stringInterpolation
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.stringInterpolation = true
---
s"user id is ${id}"
```

#### `RedundantBraces`: `oneStatApply`

Added in v3.8.4, controls treatment of single-argument apply delimiters,
has lower priority than [fewer braces](#rewritescala3removeoptionalbraces),
and takes the following values:

- `parensMaxSpan`: if non-negative, converts braces to parentheses in an
  argument clause if its span is _not larger than_ the specified value
  (and can be legally used with parentheses)
- `bracesMinSpan`: if non-negative, converts parentheses to braces in a
  single-statement argument clause if its span is _strictly larger than_
  the specified value
  - clearly, `parensMaxSpan` may not exceed `bracesMinSpan` but might be
    the same (for convenience of specifying the pivot point)
- `parensMaxSpan=0, bracesMinSpan<0` is a special combination which
  replaced an earlier parameter `parensForOneLineApply = true` and uses parens
  when the argument clause ends up formatted on a single line (rather than
  looking at the argument clause span)
  - see also
    [newlines.afterCurlyLambdaParams = squash](#newlinesaftercurlylambdaparams)

> Here, the span is computed a bit differently than for
> [fewer braces](#rewritescala3removeoptionalbraces) or
> [search optimizer](#route-search-optimizations-arg-or-param-clause),
> in that it removes not only whitespace but also all punctuation (opening and
> closing delimiters, commas, semicolons and dots), comments and any optional
> syntax tokens in scala3 (such as `then`, `:` in coloneol or varargs, end
> markers etc.), anything that a rewrite rule could modify.
>
> The reason is that this happens during the rewrite phase where this or other
> rules could modify or remove braces, trim trailing commas, add dots and
> parentheses to an infix, rewrite scala3 code using new syntax, or reformat
> comments.
>
> Thus, to avoid non-idempotent formatting, we ignore them.

In all cases, redundant delimiters will be rewritten, as before.

```scala mdoc:defaults
rewrite.redundantBraces.oneStatApply.parensMaxSpan
rewrite.redundantBraces.oneStatApply.bracesMinSpan
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.oneStatApply.parensMaxSpan = 0
---
xs.map { x => x + 1 }
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.oneStatApply.parensMaxSpan = 15
---
xs.map { x => // should rewrite this
  x + 1
}
xs.map { x => // should not rewrite this, not a single-stat argument
  x + 1
  x + 2
}
xs.map { x => // should not rewrite outer, span too long
  x.foo {
    bar.baz(qux)
  }
}
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.oneStatApply.bracesMinSpan = 2
---
xs.map(x => // should rewrite this
  x + 1
)
xs.map( // should not rewrite this, contains an infix with a break before operator
  x
  + 1
)
xs.map( // should rewrite this, contains an infix with a break after operator
  x +
  1
)
// scalafmt: { newlines.infix.termSite.style = some }
xs.map( // should rewrite this, allows formatting infix
  x
  + 1
)
xs.map( // should not rewrite this, span too short
  xy)
```

#### `RedundantBraces`: `maxBreaks`

This parameter limits the number of line breaks inside the input body. Prior to
v3.3.2, was incorrectly called `maxLines`.

```scala mdoc:defaults
rewrite.redundantBraces.maxBreaks
```

```scala mdoc:scalafmt
rewrite.rules = [RedundantBraces]
rewrite.redundantBraces.maxBreaks = 3
---
def f() = {
  collection
    .map(x => x + 1)
    .filter(_ < 10)
    .map(_ * 2)
}

def f() = {
  collection
    .map(x => x + 1)
    .filter(_ < 10)
    .map(_ * 2)
    .headOption
}
```

### Inserting braces

> Warning: this rewrite might cause non-idempotent formatting,
> formatter might need to be run twice.
>
> This rule cannot be used with
> [`rewrite.scala3.endMarker.insertMinSpan`](#rewritescala3endmarkerinsertminspan) or
> [`rewrite.scala3.removeOptionalBraces.oldSyntaxToo == true`](#rewritescala3removeoptionalbraces).

This rewrite in essence provides the opposite of what `RedundantBraces` achieves,
and somewhat similar to Scala3's end marker rewrite rules.

The rule is applied _after_ all whitespace decisions had been made and simply attempts
to output curly braces around a single-statement block when it spans at least a given
number of lines.

The rule is enabled by configuring `rewrite.insertBraces`:

- `allBlocks` (default: false): compute maximum span of all blocks under the parent
  expression rather than just the statement to be enclosed in curly braces
  - this could be used to have consistent application of curly braces in expressions
    with multiple sub-blocks, such as `if-else`, `try-finally`, `for-yield`, etc.
- `settings`:
  - `minBreaks` (default: 0, or disabled): the minimum number of line breaks to trigger the rule
  - `nonBlocksMinBreaks` (default: fallback to `minBreaks`; added in v3.10.4)
    - this threshold applies to other expressions associated with the outer tree
      containing the block in question (such as conditions for `if` or `while`,
      or parameter clauses for methods or lambdas);
    - it is disabled if set to 0, or if `allBlocks` is false; will default to `minBreaks`
      if negative; otherwise, sets the minimum number of line breaks within the expression
      to trigger braces around all blocks within the parent expression.
  - `countBreakBefore` (default: false; added in v3.10.4):
    - this parameter controls when we include the break before the expression
      when comparing it with `minBreaks`
    - it helps when we want to differentiate between, let's say, a one-liner
      `else` part following the keyword with or without a break
- `overrideFor` (default: empty; added in v3.10.4)
  - this list parameter allows customizing or overriding certain settings
  - each entry on this list contains:
    - the same fields as under `settings`; if not specified, default to `settings`
    - `owner`: logic to match the owner of expression whose span we are computing
      - it contains the following fields:
        - `regex` and `parents`, with same logic as the owners in
          [`align.tokens`](#aligntokens)

Here are some limitations:

- the rule might occasionally lead to non-idempotent formatting (that is, applying
  the formatter a second time would produce a different result); some examples are:
  - adding braces might overflow a line
  - adding braces might lead to different indentation of infix expressions
- the rule will not be applied:
  - unless the single statement is preceded by a newline; doing so would _definitely_
    lead to non-idempotent formatting
  - if the statement is an infix expression which is not enclosed in parentheses and
    has a line break _before_ an operator
  - if the code uses Scala3 syntax with significant indentation

### `RedundantParens`

```scala mdoc:scalafmt
rewrite.rules = [RedundantParens]
---
for {
  a <- b
  if (a.nonEmpty)
} yield a

val z = (insertData *> readDatabase(id))
```

#### `RedundantParens`: `infixSide`

> Since v3.5.4.

Parameter `rewrite.redundantParens.infixSide` controls how the rule applies
to expressions which are part of an outer infix expression (either left- or
right-hand side). Can take the following values:

- `null` (default): rewrites only simple expressions (literals or identifiers)

```scala mdoc:scalafmt
rewrite.rules = [RedundantParens]
rewrite.redundantParens.infixSide = null
---
// null+
foo & (0) // literal
foo & (bar) // identifier
// some+
foo & (bar.baz) // non-infix
foo & (bar + baz) // very high precedence infix
foo or (bar < baz) // non-symbolic outer op, medium precedence infix
```

- `some`: additionally, rewrites
  - all non-infix sides
  - very-high-precedence nested infix sides
  - medium-precedence nested infix sides if the outer infix operator is _non-symbolic_

```scala mdoc:scalafmt
rewrite.rules = [RedundantParens]
rewrite.redundantParens.infixSide = some
---
// some+
foo & (bar.baz) // non-infix
foo & (bar + baz) // very high precedence infix
foo or (bar < baz) // non-symbolic outer op, medium precedence infix
// many+
foo || (bar == baz) // high precedence infix
foo or (bar || baz) // non-symbolic outer op; low precedence infix
foo |: (bar |: baz) // identical op: non-symbolic; right infix, right assoc
(foo :| bar) :| baz // identical op: symbolic; left infix, left assoc
(foo or bar) or baz // identical op: non-symbolic; left infix, left assoc
```

- `many`: additionally, rewrites
  - high-precedence nested infix sides
  - nested infix sides when the operator is _identical_ to the outer infix (and associativity allows)
  - any symbolic nested infix sides if the outer infix operator is _non-symbolic_

```scala mdoc:scalafmt
rewrite.rules = [RedundantParens]
rewrite.redundantParens.infixSide = many
---
// many+
foo || (bar == baz) // high precedence infix
foo or (bar || baz) // non-symbolic outer op; low precedence infix
foo |: (bar |: baz) // identical op: non-symbolic; right infix, right assoc
(foo :| bar) :| baz // identical op: symbolic; left infix, left assoc
(foo or bar) or baz // identical op: non-symbolic; left infix, left assoc
// all
foo || (bar && baz) // low precedence infix
```

- `all`: rewrites all expressions within an infix

```scala mdoc:scalafmt
rewrite.rules = [RedundantParens]
rewrite.redundantParens.infixSide = all
---
// all
foo || (bar && baz) // low precedence infix
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

class Test(
    implicit
    final private val i1: Int,
    private final val i2: String
)

sealed protected[X] trait ADT
final private case object A1 extends ADT
private final case class A2(a: Int)
    extends ADT
```

If you prefer to use the order based on the one
[partially](https://docs.scala-lang.org/style/declarations.html#modifiers)
specified in the Scala Style Guide, you can use the corresponding preset
(since v3.8.1):

```scala mdoc:scalafmt
rewrite.rules = [SortModifiers]
rewrite.sortModifiers.preset = styleGuide
---
final lazy private implicit val x = 42
lazy final implicit private val y = 42

class Test(
    implicit
    final private val i1: Int,
    private final val i2: String
)

sealed protected[X] trait ADT
final private case object A1 extends ADT
private final case class A2(a: Int)
    extends ADT
```

If you choose a custom sort order, you can specify some or all modifiers
in the desired order:

```scala mdoc:scalafmt
rewrite.rules = [SortModifiers]
rewrite.sortModifiers.order = [
  "implicit", "final", "sealed", "abstract",
  "override", "private", "protected", "lazy"
]
---
override implicit final val x = 2
```

Hint: since some modifiers are mutually exclusive, you might want to order
them next to each other.

If you fail to include some modifiers, they will be moved to the front, before
modifiers explicitly configured, while preserving their relative order to each
other.

```scala mdoc:scalafmt
rewrite.rules = [SortModifiers]
rewrite.sortModifiers.order = [
  "implicit",  "override"
]
---
override implicit final val x = 2
```

The following modifiers are recognized currently:

- `override`
- `private`, `protected`
- `implicit`
- `final`, `sealed`, `abstract`
- `erased`, `lazy`, `open`, `transparent`, `inline`, `infix`, `opaque`

### `PreferCurlyFors`

Replaces parentheses into curly braces in for comprehensions that contain
multiple enumerator generators.

```scala mdoc:scalafmt
rewrite.rules = [PreferCurlyFors]
---
for(a <- as; b <- bs if b > 2)
 yield (a, b)
```

This rule accepts the following settings:

- `rewrite.preferCurlyFors.removeTrailingSemicolonsOnly`:
  - if `false`, replaces all semicolons with a newline
  - if `true`, keeps semicolons unless followed by a newline or single-line comment
    - see also [RemoveSemicolons](#removesemicolons)
- `rewrite.preferCurlyFors.includeGuards`: controls whether if-guards are
  also counted, in addition to generators, when detecting "multiple enumerators"

```scala mdoc:defaults
rewrite.preferCurlyFors
```

### `Imports`

This rule applies to `import` or, in Scala 3, also `export` statements found
at the top level (source, package or class/object/trait level).

The logic also moves comments attached to each import statement
or selector, as follows:

- all consecutive standalone comments before (no blank lines and not
  following some other token)
- the comment right after (possibly after a comma), on the same line
- only single-line (`//`) comments are supported; multiline (`/*`) comments
  will not be moved and in many cases will likely be removed instead

> Since v3.0.0.

Let's define some terminology: an import statement consists of several parts:

- keyword: `import` or `export`
- one or more comma-separated _importers_
  - for instance, `import foo.bar, foo.baz.{qux => quux}` contains two
    importers, `foo.bar` and `foo.baz.{qux => quux}`
- each importer is split, on the final dot, into
  - _reference_: `foo` and `foo.baz` in the example above
  - _selectors_: `bar` and `{qux => quux}` above

#### Imports: `selectors`

This parameter controls handling of import selectors within a `{...}`. Added in
v3.10.0, it replaces the boolean parameter `expand` (which, in turn, replaced
the deprecated rule `ExpandImportSelectors`).

It takes the following values:

- `keep` (default): keeps the original grouping of selectors (was: `expand = false`)
- `fold`: will group selectors from multiple import statements sharing the same
  keyword (`import` or `export`) and the same reference (the part before the
  last dot) into a single statement
  - it will keep import statements which have a trailing comment as-is
  - [`binPack.importSelectors`](#binpackimportselectors) may not be `singleLine`
    as it might result in a very long line
- `unfold`: will attempt to create a separate line for each selector (was: `expand = true`)

By default, the parameter is set to:

- `unfold` if [`newlines.source = unfold`](#newlinessource)
- `fold` if [`newlines.source = fold`](#newlinessource)
- `keep` otherwise
  - this used to be the default behavior prior to v3.10.1

```scala mdoc:scalafmt
rewrite.rules = [Imports]
rewrite.imports.selectors = unfold
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

```scala mdoc:scalafmt
rewrite.rules = [Imports]
rewrite.imports.selectors = fold
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

#### Imports: sorting

Sorting is applied as follows:

- if disabled, no sorting
- if enabled, it applies to import _selectors_ within one _importer_
- if [`groups`](#imports-groups) are used, sorting will also apply to importers
  in the same group
  - without groups, multiple import statements will **not** be sorted
  - importers are sorted one dot-separated label at a time
    - importers `foo.bar.baz` and `foo.bar as fbar` will compare:
      - `foo` and `foo`: equal
      - `bar` and `bar as fbar`: `bar` comes earlier, just like it does
        with equivalent scala2 selector syntax `{bar => fbar}`

#### Imports: `sort = none`

This default option causes no sorting.

#### Imports: `sort = original`

Replaces the deprecated rule `SortImports`.
The imports are sorted by the groups: symbols, lower-case, upper-case.

```scala mdoc:scalafmt
rewrite.rules = [Imports]
rewrite.imports.sort = original
---
import foo.{Zilch, bar, Random, sand}
```

#### Imports: `sort = ascii`

Replaces the deprecated rule `AsciiSortImports`.
The imports are sorted by their Ascii codes.

```scala mdoc:scalafmt
rewrite.rules = [Imports]
rewrite.imports.sort = ascii
---
import foo.{~>, `symbol`, bar, Random}
```

#### Imports: `sort = scalastyle`

- Selectors are sorted in a case-insensitive manner (ascii on lowercase),
  except by first character as follows: non-wildcard symbols, lowercase,
  uppercase, wildcard.
- If grouping, import statements are also sorted using case-insensitive
  order, except by first character in every dot-separated label as follows:
  symbols (including wildcard), uppercase, lowercase.

```scala mdoc:scalafmt
maxColumn = 50
rewrite.rules = [Imports]
rewrite.imports.sort = scalastyle
rewrite.imports.groups = [["foo\\..*"]]
---
import foo.bar.{Random, bar, ~>, `symbol`}
import foo.Baz.{bar => xyz, _}
import foo.`qux`.{Random, bar, ~>, `symbol`}
import foo._
```

#### Imports: `groups`

> Keep in mind that this functionality should be used very carefully if
> hierarchical (relative) imports are allowed in your codebase. Groups
> should only refer to typical top-level domains such as `java`, `org`,
> `com` or `scala` (i.e. ensuring no relative imports would fall into any
> of the groups), and making sure
> [`rewrite.imports.sortCatchallGroup`](#imports-sortcatchallgroup) is
> not `full`.
>
> The safest way to handle this case is by using `scalafix` with a semantic
> rule like `OrganizeImports`. However, on a large codebase, the overhead
> of using semantic `scalafix` rules might be substantial.

This rule will separate all import statements (or, to be more precise, all
[importers](#imports) from all import statements) into groups. If sorting
is enabled (i.e., not `none`), imports will also be sorted within each group.

The rule accepts the following parameters:

- `rewrite.imports.groups`: defines several sets of regular expressions
  - each set defines a single group, and the groups are output in the order they
    are configured
  - imports not matching any of the regexes will form their own group at the end
  - regular expressions are applied to the entire parent domain of the import
    statement, up to and including the final dot
  - the longest patterns are applied first
- `rewrite.imports.contiguousGroups` (since v3.0.2):
  - if `only` (default), only consecutive import statements will be grouped
  - if `no`, grouping will happen on all imports within the same container
    (source, package, template etc.)

```scala mdoc:scalafmt
runner.dialect = scala3
rewrite.rules = [Imports]
rewrite.imports.sort = ascii
rewrite.imports.groups = [
  ["foo\\..*"],
  ["bar\\..*", "baz\\..*"]
]
---
import bar.bar as bbar
import bar.bar.{Random, bar, ~>, `symbol`}
import baz.Baz.{bar => xyz, _}
import qux.`qux`.{Random, bar, ~>, `symbol`}
import foo._
import baz.bar.{Random, bar, ~>, `symbol`}
import qux.Baz.{bar => xyz, _}
import foo.`qux`.{Random, bar, ~>, `symbol`}
import bar._
import qux.bar.{Random, bar, ~>, `symbol`}
import foo.Baz.{bar => xyz, _}
import bar.`qux`.{Random, bar, ~>, `symbol`}
import baz._
```

#### Imports: `sortCatchallGroup`

> Since v3.10.2.

Preserving the order of relative imports is important, to avoid putting
"relativeB-of-relativeA" before "relativeA", so they shouldn't be sorted
the same way fully qualified ones are.

If we assume that explicitly defined groups would not catch relative
imports, then we only need to forgo or modify sorting of the implicit,
catch-all group where everything else (including relatives) would end up.

```scala mdoc:defaults
rewrite.imports.sortCatchallGroup
```

This parameter takes the following values:

- `full`: the catch-all group is sorted the same way as other groups
- `none`: the catch-all group is not sorted, preserving the original order
- `tail`: the catch-all group applies sorting to statements which have the same
  first element (say, `import a.b` and `import a.c`), otherwise their original
  order is preserved

#### Imports: `removeRedundantSelectors`

> Since v3.10.0.

If `rewrite.imports.removeRedundantSelectors` is enabled, will remove those
selectors which are identical to another or are superseded by an appropriate
wildcard. This deduplication will only happen at the statement level unless
[groups](#imports-groups) or [selectors](#imports-selectors) are enabled.

Regardless of this setting, the rule will de-duplicate identical import statements.

Example with statement-level dedup:

```scala mdoc:scalafmt
runner.dialect = scala3
rewrite.rules = [Imports]
rewrite.imports.sort = ascii // this is not enough
rewrite.imports.removeRedundantSelectors = true
---
import foo.{bar, baz, bar, baz} // will dedup only this
import foo.bar
import foo.{baz => qux, given A, _}
import foo.given
```

Example with wider dedup due to selectors:

```scala mdoc:scalafmt
runner.dialect = scala3
rewrite.rules = [Imports]
rewrite.imports.selectors = unfold
rewrite.imports.removeRedundantSelectors = true
---
// will dedup all
import foo.{bar, baz}
import foo.bar
import foo.{baz => qux, given A, _}
import foo.given
```

Example with wider dedup due to grouping:

```scala mdoc:scalafmt
runner.dialect = scala3
rewrite.rules = [Imports]
rewrite.imports.groups = [[ "foo1.*" ]]
rewrite.imports.removeRedundantSelectors = true
---
// will dedup all
import foo1.{bar, baz}
import foo1.bar
import foo1.{baz => qux, given A, _}
import foo1.given
import foo2.{bar, baz}
import foo2.bar
import foo2.{baz => qux, given A, _}
import foo2.given
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
> parameters within the `rewrite.trailingCommas` section (since v3.0.5; prior to that
> there was a single top-level `trailingCommas` parameter).

```scala mdoc:defaults
rewrite.trailingCommas.style
```

#### Trailing commas: `never`

Makes sure there are no trailing commas:

```scala mdoc:scalafmt
rewrite.trailingCommas.style = never
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

#### Trailing commas: `keep`

Keeps any trailing commas:

```scala mdoc:scalafmt
rewrite.trailingCommas.style = keep
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
rewrite.trailingCommas.style = always
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
rewrite.trailingCommas.style = multiple
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

#### `rewrite.trailingCommas.allowFolding`

This parameter controls whether the trailing comma must be maintained (except
for `never`), or the code can be folded to avoid a dangling closing delimiter
which is required by Scala after a trailing comma.

> Since v3.0.5

```scala mdoc:defaults
rewrite.trailingCommas.allowFolding
```

If set to false, the trailing comma will always be forced.

### `rewrite.tokens`

> Prior to v3.8.4, was called `rewriteTokens`.

Map of tokens to rewrite. For example, Map("⇒" -> "=>") will rewrite unicode
arrows to regular ascii arrows.

```scala mdoc:defaults
rewrite.tokens
```

```scala mdoc:scalafmt
rewrite.tokens = {
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

### RemoveSemicolons

> Since v3.10.1.

This rule removes semicolons unless required by the Scala syntax (such
as those used in for-clause enumerators enclosed in parentheses).
See also [PreferCurlyFors](#prefercurlyfors).

### RewriteLiterals

> Since v3.10.3.

This rule rewrites numeric literals potentially modifying representation style.
It's an extension of single-character replacement rules grouped under [`literals`](#literals).

#### `literals.floatingPoint.filter`

```scala mdoc:defaults
literals.floatingPoint.filter
```

These settings determine whether the rule is applicable to a given
floating-point token.

##### `literals.floatingPoint.filter.minTotalDigits`

Filters on length of literal, excluding sign and type suffix.

```scala mdoc:scalafmt
rewrite.rules = [RewriteLiterals]
literals.floatingPoint {
  filter.minTotalDigits = 6
}
---
-0.000120d
-0.00120d
-0.0120d
-0.120d
-1.20d
-12.0d
-120d
-1200d
-12000d
-120000d
-1200000d
```

##### `literals.floatingPoint.filter.minSignificantDigits`

Filters on number of decimal digits in normalized
scientific notation, excluding sign, type suffix, dot and exponent.

```scala mdoc:scalafmt
rewrite.rules = [RewriteLiterals]
literals.floatingPoint {
  filter {
    minTotalDigits = 0
    minSignificantDigits = 3
  }
}
---
-0.000120e0d
-0.000123e0d
-0.00120e0d
-0.00123e0d
-0.0120e0d
-0.0123e0d
-0.120e0d
-0.123e0d
-1.20e0d
-1.23e0d
-12.0e0d
-12.3e0d
-120e0d
-123e0d
-1200e0d
-1230e0d
-12000e0d
-12300e0d
-120000e0d
-123000e0d
```

##### `literals.floatingPoint.filter.needSuffix`

Applies only to literals which contain a type suffix (`D` or `F`),
to avoid modifying
[custom number literals](https://docs.scala-lang.org/scala3/reference/experimental/numeric-literals.html).

```scala mdoc:scalafmt
rewrite.rules = [RewriteLiterals]
literals.floatingPoint {
  filter {
    minTotalDigits = 0
    needSuffix = true
  }
}
---
0.000120
0.000120d
0.00120
0.00120d
0.0120
0.0120d
0.120
0.120d
1.20
1.20d
12.0
12.0d
120.0
120d
1200.0
1200d
12000.0
12000d
```

#### `literals.floatingPoint.format`

```scala mdoc:defaults
literals.floatingPoint.format
```

These settings control how the resulting literal is formatted.
The output is in either a
[normalized scientific notation](https://en.wikipedia.org/wiki/Scientific_notation#Normalized_notation)
or a simple decimal form with a non-empty whole part and a possibly empty
fractional part.

##### `literals.floatingPoint.format.maxPaddingZeros`

This setting controls choice between decimal and scientific notation:
normalized scientific notation is selected if in the decimal representation
the number of padding zeros just before trailing `.0` or immediately after
leading `0.` exceeds this value.

```scala mdoc:scalafmt
rewrite.rules = [RewriteLiterals]
literals.floatingPoint {
  filter.minTotalDigits = 1
  format.maxPaddingZeros = 2
}
---
12300.0
123000.0
0.00123
0.000123
```

##### `literals.floatingPoint.format.separators`

If positive, and allowed by the dialect (via
`allowNumericLiteralUnderscoreSeparators` flag), will add a numeric
separator (underscore) for every group of digits (counting from the dot).

```scala mdoc:scalafmt
rewrite.rules = [RewriteLiterals]
literals.floatingPoint {
  filter.minTotalDigits = 1
  format.maxPaddingZeros = 10
}
---
1234e-4
1234e-3
1234e-2
1234e-1
1234e+0
1234e+1
1234e+2
1234e+3
1234e+4
// scalafmt: { literals.floatingPoint.format.separators = 2 }
1234e-4
1234e-3
1234e-2
1234e-1
1234e+0
1234e+1
1234e+2
1234e+3
1234e+4
```

##### `literals.floatingPoint.format.forceDot`

If enabled, always includes `.0` if the number has no fractional part; otherwise,
this would be added only if the number looks like an integer
(that is, no exponent or type suffix).

```scala mdoc:scalafmt
rewrite.rules = [RewriteLiterals]
literals.floatingPoint {
  filter.minTotalDigits = 1
  format.maxPaddingZeros = 5
}
---
1230.0
1230d
1230.0d
123e3
123.0e3
123e10
123.0e10
// scalafmt: { literals.floatingPoint.format.forceDot = true }
1230.0
1230d
1230.0d
123e3
123.0e3
123e10
123.0e10
```

##### `literals.floatingPoint.format.forceExpPlus`

If enabled, always includes a sign after the `E` in exponent; otherwise,
only negative exponent (naturally) comes with a sign.

```scala mdoc:scalafmt
rewrite.rules = [RewriteLiterals]
literals.floatingPoint {
  filter.minTotalDigits = 1
}
---
123e0
123e+0
123e1
123e+1
123e-1
// scalafmt: { literals.floatingPoint.format.forceExpPlus = true }
123e0
123e+0
123e1
123e+1
123e-1
```

### `ProcedureSyntax`

If the dialect still supports procedure syntax, this rule would replace it with
a `Unit` method.

```scala mdoc:scalafmt
rewrite.rules = [ProcedureSyntax]
runner.dialect = scala213
---
def foo() {}
```

## Scala3 rewrites

This section describes rules which are applied if the appropriate dialect (e.g.,
`runner.dialect = scala3`) is selected.

> This logic is not triggered via the `rewrite.rules` parameter, but by setting
> parameters under `rewrite.scala3` subsection.

### `rewrite.scala3.convertToNewSyntax`

If this flag is enabled, the following new syntax will be applied (also,
**since 3.8.0**, if an appropriate flag under `rewrite.scala3.newSyntax` is not
set to `false`, see below):

- [control syntax](https://dotty.epfl.ch/docs/reference/other-new-features/control-syntax.html)
  - if dialect sets `allowSignificantIndentation`
    (any scala3 dialect) and `...newSyntax.control` is set
    - `if (...)` to `if ... then`
    - `while (...)` to `while ... do`
    - `for (...)` to `for ... do` (or `for (...) yield` to `for ... yield`)
- [vararg splices](https://dotty.epfl.ch/docs/reference/changed-features/vararg-splices.html)
  - vararg `: _*` or `@ _*` to `*` if dialect sets `allowPostfixStarVarargSplices`
    (any scala3, or scala2xxSource3) and `...newSyntax.deprecated` is set
- [imports](https://dotty.epfl.ch/docs/reference/changed-features/imports.html)
  - import wildcard `_` to `*` if dialect sets `allowStarWildcardImport`
    (any scala3, or scala2xxSource3) and `...newSyntax.deprecated` is set
  - import rename `=>` to `as` if dialect sets `allowAsForImportRename`
    (any scala3, or scala2xxSource3) and `...newSyntax.deprecated` is set
- [wildcards](https://docs.scala-lang.org/scala3/reference/changed-features/wildcards.html)
  - type wildcard `_` to `?` if dialect sets `allowQuestionMarkAsTypeWildcard`
    (scala212 and later) and `...newSyntax.deprecated` is set
  - anonymous type param `*` to `_` if dialect sets `allowUnderscoreAsTypePlaceholder`
    (scala3Future only) and `...newSyntax.deprecated` is set

NB: You could control these rules individually by
[overriding dialect properties](#runnerdialectoverride).

### `rewrite.scala3.removeOptionalBraces`

If this section is enabled,
[optional braces](https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html)
will be removed and significant indentation applied.

```scala mdoc:defaults
rewrite.scala3.removeOptionalBraces
```

The section contains the following settings (available since v3.8.1):

- `enabled`:
  - if `false`, disables any rewriting, regardless of other
    flags in this section
  - if `true`, enables rewriting
    - applies to expressions using the new control syntax
      (or those which would rewritten to new syntax if
      `rewrite.scala3.convertToNewSyntax` is set)
    - other flags below might extend rewrites to other cases
- `oldSyntaxToo`
  - if `true`, applies also to expressions using deprecated syntax
- (since v3.8.1) `fewerBracesMinSpan` and `fewerBracesMaxSpan`
  - will apply the rewrite to last curried single-argument group if
    it is enclosed in curly braces (or would be rewritten to curly
    braces by the `RedundantBraces` rule)
  - will only apply the rewrite if the cumulative span of all visible
    (non-whitespace) tokens within the argument is between the two values
  - this rule is disabled if `fewerBracesMaxSpan == 0`
- (since v3.8.4) `fewerBracesParensToo`
  - will apply the rule just above to an argument in parentheses as well,
    if the one of following is also satisfied:
    - [`newlines.infix.xxxSite.style`](#newlinesinfix-stylekeep) is NOT `keep`; or
    - current dialect supports `allowInfixOperatorAfterNL`

Prior to v3.8.1, `rewrite.scala3.removeOptionalBraces` was a flag which
took three possible values (with their equivalent current settings shown):

- `no`: `enabled = false`
- `yes`: `enabled = true`
- `oldSyntaxToo`: `enabled = true` and `oldSyntaxToo = true`

### `rewrite.scala3.endMarker.insertMinSpan`

If this flag is set to a positive value, when an expression containing an
[optional braces](https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html)
region spans at least as many lines and isn't followed by an end marker, one will be inserted.

> We will not insert end markers if the statement is not part of a template body,
> or a multi-stat block. Doing so might turn a single-stat expression (which
> doesn't require significant indentation handling) into a multi-stat block.

Prior to v3.10.3, this setting was named `scala3.insertEndMarkerMinLines`.

### `rewrite.scala3.endMarker.removeMaxSpan`

If this flag is set to a positive value, when an expression containing an
[optional braces](https://dotty.epfl.ch/docs/reference/other-new-features/indentation.html)
region spans at most as many lines and is followed by a standalone end marker
(i.e., no other tokens on that line, including comments), the line containing
the end marker will be deleted.

> We will not remove end markers if
>
> - the statement is not part of a template body, or a block with at least 3
>   statements. Doing so might turn a multi-stat expression (which requires
>   significant indentation handling) into a single-stat.
> - there are comments before the end marker, as without the end marker they
>   would be treated as outside of the optional-braces region.

Prior to v3.10.3, this setting was named `scala3.removeEndMarkerMaxLines`.

### `rewrite.scala3.endMarker.spanHas`

> Since v3.0.6.

This flag dictates which part of the expression terminated by the end marker
is used to calculate the span for the purposes of applying
[`insertMinSpan`](#rewritescala3endmarkerinsertminspan) and
[`removeMaxSpan`](#rewritescala3endmarkerremovemaxspan).

- `all` (default): the entire expression
- `lastBlockOnly`: only the last block with significant indentation relative to
  the start of the said expression (as a replacement for the closing curly brace
  which would have been used otherwise); for instance:
  - in case of a class, this would be the body of the class
  - but for an if-else, this would be just the `else` part

Prior to v3.10.3, this setting was named `scala3.countEndMarkerLines`.

### `rewrite.scala3.endMarker.spanIs`

```scala mdoc:defaults
rewrite.scala3.endMarker.spanIs
```

> Since v3.10.3.

This parameter defines what "span" refers to when applying
[`insertMinSpan`](#rewritescala3endmarkerinsertminspan) and
[`removeMaxSpan`](#rewritescala3endmarkerremovemaxspan).
It takes the following values:

- `lines`: span counts the number of lines
- `blankGaps`: span counts the number of blank gaps instead

## Vertical Multiline

Since: v1.6.0.

If enabled this formats methods such that parameters are on their own line
indented by [`indent.defnSite`](#indentdefnsite).
Separation between parameter groups are indented by two spaces less than
`indent.defnSite`. The return type is on its own line at the end.

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
indent.defnSite = 2
verticalMultiline.atDefnSite = true
verticalMultiline.arityThreshold = 2
verticalMultiline.newlineAfterOpenParen = true
---
def other(a: String, b: String)(c: String, d: String) = a + b + c
```

### `verticalMultiline.excludeDanglingParens`

This parameter has been removed in 3.4.0, please use
[danglingParentheses.exclude](#danglingparenthesesexclude).

```scala mdoc:scalafmt
indent.defnSite = 2
danglingParentheses.exclude = [def]
verticalMultiline.atDefnSite = true
verticalMultiline.arityThreshold = 2
verticalMultiline.newlineAfterOpenParen = true
---
def other(a: String, b: String)(c: String, d: String) = a + b + c
other(a, b)(c, d)
```

### Vertical multiline with `implicit` parameter lists

> Also see the general section on
> [implicit parameter lists](#newlinesimplicitparamlistmodifierxxx).

#### Before only

```scala mdoc:scalafmt
maxColumn = 60
verticalMultiline.atDefnSite = true
newlines.implicitParamListModifierForce = [before]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### After only

```scala mdoc:scalafmt
maxColumn = 60
verticalMultiline.atDefnSite = true
newlines.implicitParamListModifierForce = [after]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

#### Before and after

```scala mdoc:scalafmt
maxColumn = 60
verticalMultiline.atDefnSite = true
newlines.implicitParamListModifierForce = [before,after]
---
def format(code: String, age: Int)(implicit ev: Parser, c: Context): String
```

## Comment processing

### `comments.wrap`

> Since v2.6.0.

Allows wrapping comments exceeding `maxColumn`.

```scala mdoc:defaults
comments.wrap
```

#### `comments.wrap = standalone`

A standalone comment is one which is surrounded by line breaks.

```scala mdoc:scalafmt
maxColumn = 20
comments.wrap = standalone
---
/* long multiline comment */
// long singleline comment
val a = 1 // short
val b = 2 // long singleline comment
```

#### `comments.wrap = trailing`

A trailing comment is one which is followed by a line break.

```scala mdoc:scalafmt
maxColumn = 20
comments.wrap = trailing
---
/* long multiline comment */
// long singleline comment
val a = 1 // short
val b = 2 // long singleline comment
```

### `comments.wrapStandaloneSlcAsSlc`

> Since v2.6.0.

This parameter allows formatting a standalone single-line comment (i.e., `//`)
to be wrapped using the same type, not a multi-line comment (`/* ... */`).

```scala mdoc:defaults
comments.wrapStandaloneSlcAsSlc
```

```scala mdoc:scalafmt
maxColumn = 20
comments.wrap = trailing
comments.wrapStandaloneSlcAsSlc = true
---
// long singleline comment
val b = 2 // long singleline comment
```

### `comments.wrapSingleLineMlcAsSlc`

> Since v3.3.1.

If comment wrapping is enabled ([`comments.wrap != no`](#commentswrap)), this parameter
allows formatting a trailing or standalone multi-line comment (i.e., `/* ... */`) as a
single-line comment (`//`) if it occupies a single line.

```scala mdoc:defaults
comments.wrapSingleLineMlcAsSlc
```

```scala mdoc:scalafmt
maxColumn = 50
comments.wrap = trailing
comments.wrapSingleLineMlcAsSlc = true
---
/* standalone multi-line comment */
val b = 2 /* mlc */ /* trailing mlc */
```

### `comments.indentTrailingInCaseBody`

> Since v3.10.4.

This parameter controls indentation of standalone comments occurring after the
last statement of a case body:

- `none` (default): these comments would not be indented, thus getting attributed
  to the next case clause instead; this was the only behaviour prior to v3.10.4
- `less`:
  - for final case clause, or if there are no blank lines, all comments are indented
  - otherwise, only comments before the very _first_ blank line would be indented
- `more`:
  - for final case clauses, or if there are no blank lines, all comments are indented
  - otherwise, only comments after the very _last_ blank line would _not_ be indented

### `docstrings.style`

> Since v2.6.0.

```scala mdoc:defaults
docstrings.style
```

#### `docstrings.style = keep`

Prohibits formatting of docstrings. All other `docstrings` parameters are
ignored.

> Since v3.0.0.

```scala mdoc:scalafmt
docstrings.style = keep
---
/**   do not touch
 * this style
  * keep the text as-is
*/
```

#### `docstrings.style = Asterisk`

This variant used to be called `JavaDoc`.

```scala mdoc:scalafmt
docstrings.style = Asterisk
---
/** Skip first line, format intermediate lines with an asterisk
  * below the first asterisk of the first line (aka JavaDoc).
  * Since v3.8.4, `blankFirstLine = fold` takes precedence.
  */
```

#### `docstrings.style = SpaceAsterisk`

This variant used to be called `ScalaDoc`.

```scala mdoc:scalafmt
docstrings.style = SpaceAsterisk
---
/** Format intermediate lines with a space and an asterisk,
 * both below the two asterisks of the first line
 */
```

#### `docstrings.style = AsteriskSpace`

```scala mdoc:scalafmt
docstrings.style = AsteriskSpace
---
/** Format intermediate lines with an asterisk and a space,
  * both below the two asterisks of the first line
  */
```

### `docstrings.removeEmpty`

If set, will cause empty docstrings to be removed.

> Since v3.0.4.

```scala mdoc:defaults
docstrings.removeEmpty
```

```scala mdoc:scalafmt
docstrings.removeEmpty = true
---
/** */
/**
  *
  */
/** */ /** */
val a = 1
```

### `docstrings.oneline`

> Since v2.6.0. Ignored for `docstrings.style = keep`.

```scala mdoc:defaults
docstrings.oneline
```

#### `docstrings.oneline = fold`

```scala mdoc:scalafmt
docstrings.style = Asterisk
docstrings.oneline = fold
---
/** Scaladoc oneline */
/**
  * Scaladoc multiline
  */
val a = 1
```

#### `docstrings.oneline = unfold`

```scala mdoc:scalafmt
docstrings.style = Asterisk
docstrings.oneline = unfold
---
/** Scaladoc oneline */
/**
  * Scaladoc multiline
  */
val a = 1
```

#### `docstrings.oneline = keep`

```scala mdoc:scalafmt
docstrings.style = Asterisk
docstrings.oneline = keep
---
/** Scaladoc oneline */
/**
  * Scaladoc multiline
  */
val a = 1
```

### `docstrings.wrap`

Will parse scaladoc comments and reformat them.
Takes the following values:

- `keep`: preserves scaladoc comments as-is and will not reformat them
  (replaced `no` in v3.8.2)
- `fold`: will use a more compact, horizontal formatting
  (added in v3.8.2)
- `unfold`: will use a more expanded, vertical formatting
  (replaced `yes` in v3.8.2)

This functionality is generally limited to
[standard scaladoc elements](https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html)
and might lead to undesirable results in corner cases; for instance, the
scaladoc parser doesn't have proper support of embedded HTML.

However,
[tables are supported](https://www.scala-lang.org/blog/2018/10/04/scaladoc-tables.html).

> Since v2.6.0. Ignored for `docstrings.style = keep`.

```scala mdoc:defaults
docstrings.wrap
```

```scala mdoc:scalafmt
docstrings.wrap = yes
maxColumn = 30
---
/**
 * @param d the Double to square, meaning multiply by itself
 * @return the result of squaring d
 *
 * Thus
 * - if [[d]] represents a negative value:
 *  a. the result will be positive
 *  a. the value will be {{{d * d}}}
 *  a. it will be the same as for `-d`
 * - however, if [[d]] is positive
 *  - the value will still be {{{d * d}}}
 *    - i.e., the same as {{{(-d) * (-d)}}}
 *
 * In other words:
 * {{{
 *    res = d * d
 *        = (-d) * (-d) }}}
 */
def pow2(d: Double): Double
```

### `docstrings.wrapMaxColumn`

If wrapping (or applying `oneline`), allows specifying a different value than
the default `maxColumn`.

### `docstrings.blankFirstLine`

Controls whether to force the first line to be blank in a multiline docstring.
Some values of [`docstrings.style`](#docstringsstyle) might take precedence.
Takes the following values:

- `keep`: preserves the first line as-is
- `fold`: will ensure there's no blank first line
  (default; replaced `no` in v3.8.2)
- `unfold`: will enforce a blank first line
  (replaced `yes` in v3.8.2)

> Since v2.7.5.
>
> - Ignored for `docstrings.style = keep` or `docstrings.wrap = no`.
> - [since v3.8.4] For `docstrings.style = Asterisk`, only `fold` changes default behaviour.

```scala mdoc:scalafmt
# do not force a blank first line
docstrings.blankFirstLine = fold
docstrings.style = SpaceAsterisk
maxColumn = 30
---
/** Scaladoc oneline */
/** Scaladoc multiline1
  */
/**
  * Scaladoc multiline2
  */
val a = 1
```

```scala mdoc:scalafmt
# force a blank first line
docstrings.blankFirstLine = unfold
docstrings.style = SpaceAsterisk
maxColumn = 30
---
/** Scaladoc oneline */
/** Scaladoc multiline1
  */
/**
  * Scaladoc multiline2
  */
val a = 1
```

```scala mdoc:scalafmt
# preserve a blank first line
docstrings.blankFirstLine = keep
docstrings.style = SpaceAsterisk
maxColumn = 30
---
/** Scaladoc oneline */
/** Scaladoc multiline1
  */
/**
  * Scaladoc multiline2
  */
val a = 1
```

### `docstrings.forceBlankLineBefore`

If true (default), always insert a blank line before docstrings.
If false, preserves blank line only if one exists before.

> Since v3.4.0. Replaced deprecated `optIn.forceBlankLineBeforeDocstring`.

```scala mdoc:scalafmt
docstrings.forceBlankLineBefore = true
---
object Stuff {
  /** Some function */
  def hello = ()
}
```

```scala mdoc:scalafmt
docstrings.forceBlankLineBefore = false
---
object Stuff {
  /** Some function */
  def hello = ()
}
```

## Disabling or customizing formatting

### `Search state exploded`

If this exception occurs, you can try increasing
[optimizer limits](#route-search-optimizations-giving-up),
globally or using any of the options further in this section.

### For code block

There is a possibility to override scalafmt config for subsequent code with a
`// scalafmt: { <settings> }` comment.

Each such invocation overlays new settings over the initial configuration for a
given file; thus, to reset back to that configuration, simply use `// scalafmt: {}`.

However, since v3.10.4, if this comment is specified at the top of the file,
before the first non-comment, then this configuration becomes the "initial".

```scala mdoc:scalafmt
---
// scalafmt: { align.preset = most, danglingParentheses.preset = false }
libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % scalametaV,
  "org.scalacheck" %% "scalacheck" % scalacheckV)

/* scalafmt: {
     align.preset = some
     danglingParentheses.preset = true
   }
 */
libraryDependencies ++= Seq(
  "org.scalameta" %% "scalameta" % scalametaV,
  "org.scalacheck" %% "scalacheck" % scalacheckV)

// scalafmt: {} // back to defaults
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

Since v3.8.4, these format on-off tags can be configured:

- `formatOff`: tags to turn formatting off
- `formatOn`: tags to turn formatting back on

```scala mdoc:defaults
formatOff
formatOn
```

### Project

Configure which source files should be formatted in this project.

#### `project.git`

If this boolean flag is set, only format files tracked by git.

#### `project.include/exclude`

> Since v3.0.0.

```scala mdoc:defaults
project.includePaths
project.excludePaths
```

Allows specifying
[PathMatcher](https://docs.oracle.com/javase/8/docs/api/java/nio/file/FileSystem.html#getPathMatcher-java.lang.String-)
selection patterns to identify further which files are to be formatted (explicit `glob:` or
`regex:` prefixes are required; keep in mind that `PathMatcher` patterns must match the entire
path).

For instance,

```conf
project {
  includePaths = [
    "glob:**.scala",
    "regex:.*\\.sc"
  ]
  excludePaths = [
    "glob:**/src/test/scala/**.scala"
  ]
}
```

#### `project.layout`

Allows specifying a project structure naming convention which can be used to
select an appropriate dialect for cross-building if one is explicitly selected
via [`fileOverride`](#fileoverride). By default, it's disabled (`null`).

Currently, the following options are supported:

- (since v3.2.0) `StandardConvention`: this is the usual naming convention
  putting scala source code under `src/main/scala` or `src/test/scala`, with
  alternate cross-build dialects in `src/main/scala-2.13`

If this parameter is set, some supported dialects will be determined automatically;
if the detected dialect is compatible with the overall one (`runner.dialect`),
no change will be applied.

Currently, supports scala binary versions 2.10-2.13 as well as 3; also, if the version
is major scala 2 (i.e., `scala-2`), will select the scala 2.13 dialect.

### `fileOverride`

> Since v2.5.0.
>
> - This parameter does not modify which files are formatted.
> - The match pattern will be applied to the **entire** absolute,
>   canonical file name; it is not a suffix or a substring match.

Allows specifying an additional subset of parameters for each file matching a
[PathMatcher](https://docs.oracle.com/javase/8/docs/api/java/nio/file/FileSystem.html#getPathMatcher-java.lang.String-)
pattern (e.g., a `glob` or a `regex`):

```
fileOverride {
  "<PathMatcher pattern>" { # must match the entire filename
    <overridden parameters>
  }
}
```

For instance,

```
align.preset = none
fileOverride {
  "glob:**.sbt" {
    align.preset = most
  }
  "glob:**/src/test/scala/**.scala" {
    maxColumn = 120
    binPack.callSite = true
  }
}
```

uses `align.preset=none` for all files except `.sbt` for which
`align.preset=most` will apply. It will also use different parameters for test
suites.

In case multiple patterns match a given file, the longest pattern will be used.

The parameter also allows the following shortcuts:

- (since v3.2.0) setting only the dialect:
  - `fileOverride { "glob:**/*.sbt" = sbt1 }`
- (since v3.2.0) setting based on the file extension:
  - `fileOverride { ".sbt" { runner.dialect = sbt1 } }`
  - this is simply a shortcut for `glob:**.ext`
- (since v3.2.0) setting based on the language:
  - `fileOverride { "lang:scala-2" = scala213 }`
  - requires [project.layout](#projectlayout) (sets dialect for minor versions)
  - these patterns will be matched last

## Spaces

### `spaces.beforeContextBoundColon`

This parameter controls if a space should be used before a colon that precedes
type context bounds and takes values `never`, `always`, `ifMultipleBounds`, and
(added in v3.8.6) `ifMultipleContextBounds`. The difference between the latter
two is: the first considers all bounds (including subtype and supertype),
whereas the second only the context bounds.

```scala mdoc:defaults
spaces.beforeContextBoundColon
```

```scala mdoc:scalafmt
spaces.beforeContextBoundColon=Never
---
def method[A: Bound]: B
def method[A : Bound]: B
def method[A: Bound: Bound2]: B
```

```scala mdoc:scalafmt
spaces.beforeContextBoundColon=Always
---
def method[A: Bound]: B
def method[A : Bound]: B
def method[A: Bound: Bound2]: B
```

```scala mdoc:scalafmt
spaces.beforeContextBoundColon=IfMultipleBounds
---
def method[A: Bound]: B
def method[A : Bound]: B
def method[A: Bound: Bound2]: B
def method[A <: Bound: Bound2]: B
```

```scala mdoc:scalafmt
spaces.beforeContextBoundColon=IfMultipleContextBounds
---
def method[A: Bound]: B
def method[A : Bound]: B
def method[A: Bound: Bound2]: B
def method[A <: Bound: Bound2]: B
```

### `spaces.withinContextBoundBraces`

This parameter controls if a space should be used within braces which surround
type context bounds and takes the same values as
[`beforeContextBoundColon`](#spacesbeforecontextboundcolon) above. Added in v3.8.6.

```scala mdoc:defaults
spaces.withinContextBoundBraces
```

```scala mdoc:scalafmt
runner.dialect = scala3
spaces.withinContextBoundBraces=Never
---
def method[A: {Bound}]: B
def method[A: {Bound, Bound2}]: B
```

```scala mdoc:scalafmt
runner.dialect = scala3
spaces.withinContextBoundBraces=Always
---
def method[A: {Bound}]: B
def method[A: {Bound, Bound2}]: B
```

```scala mdoc:scalafmt
runner.dialect = scala3
spaces.withinContextBoundBraces=IfMultipleBounds
---
def method[A: {Bound}]: B
def method[A <: Bound : {Bound2}]: B
def method[A: {Bound, Bound2}]: B
```

```scala mdoc:scalafmt
runner.dialect = scala3
spaces.withinContextBoundBraces=IfMultipleContextBounds
---
def method[A: {Bound}]: B
def method[A <: Bound : {Bound2}]: B
def method[A: {Bound, Bound2}]: B
```

### `spaces.inImportCurlyBraces`

```scala mdoc:defaults
spaces.inImportCurlyBraces
```

```scala mdoc:scalafmt
spaces.inImportCurlyBraces=true
---
import a.b.{c, d}
```

### `spaces.inInterpolatedStringCurlyBraces`

> Since v3.0.0.

```scala mdoc:defaults
spaces.inInterpolatedStringCurlyBraces
```

```scala mdoc:scalafmt
spaces.inInterpolatedStringCurlyBraces = true
---
s"Hello ${the} world!"
s"Hello ${ th.e} world!"
s"Hello ${the() } world!"
```

```scala mdoc:scalafmt
spaces.inInterpolatedStringCurlyBraces = false
---
s"Hello ${ oneHundred }% world!"
s"Hello ${ th.e} world!"
s"Hello ${the() } world!"
```

### `spaces.inParentheses`

```scala mdoc:defaults
spaces.inParentheses
```

```scala mdoc:scalafmt
spaces.inParentheses=true
---
foo(a, b)
```

### `spaces.neverAroundInfixTypes`

```scala mdoc:defaults
spaces.neverAroundInfixTypes
```

```scala mdoc:scalafmt
spaces.neverAroundInfixTypes=["##"]
---
def f: Foo##Repr
def g: Foo\/Repr
// usage same operator not as type
def e = a##b
```

### `spaces.aroundSymbolicInfixOperators`

Added in v3.8.3, this parameter controls spaces around some infix operators
(unless [`spaces.neverAroundInfixTypes`](#spacesneveraroundinfixtypes) above
applies).

This parameter contains two subparameters, `include` and `exclude`, each either
a string containing a regex, or a list of regex alternatives. By default,
`include` is `.*` (i.e., matches everything) and `exclude` is `^$` (matches
nothing), thus all infix operators would use a space.

The logic is applied as follows:

- first, the infix operator must be symbolic (i.e., does not start with a
  letter or underscore) and not an assignment; otherwise, space is enforced;
- if the two characters on either side of the proposed space (i.e., the last
  character of the left-hand side and the first character of the operator,
  for spaces before the operator, or the last character of the operator and
  the first character of the right-hand side for spaces after the operator)
  can both be part of a symbolic operator, the space is enforced;
- otherwise, if the operator doesn't match any of the `include` patterns or
  matches one of the `exclude` patterns, the space is not output.

```scala mdoc:scalafmt
maxColumn = 80
spaces.aroundSymbolicInfixOperators.include = ".*" # default
spaces.aroundSymbolicInfixOperators.exclude = [ "^##$", "==" ]
---
def f: Foo ## Repr
def g(a: Column, b: Column): Boolean = a === b || a###b
def e(a: Int, b: Int) = a ## b || a == b || a!=b
```

### `spaces.afterKeywordBeforeParen`

```scala mdoc:defaults
spaces.afterKeywordBeforeParen
```

```scala mdoc:scalafmt
spaces.afterKeywordBeforeParen = false
---
if (a) println("HELLO!")
while (a) println("HELLO!")
```

### `spaces.inByNameTypes`

```scala mdoc:defaults
spaces.inByNameTypes
```

```scala mdoc:scalafmt
spaces.inByNameTypes = false
---
def foo(a: => A): A
```

### `spaces.afterSymbolicDefs`

```scala mdoc:defaults
spaces.afterSymbolicDefs
```

```scala mdoc:scalafmt
spaces.afterSymbolicDefs=true
---
def +++(a: A): F[A]
```

### `spaces.beforeXxxArgInParens`

> Since v3.7.13.

These parameters control whether a space should be added before an argument of
a function call or infix expression, if the argument is enclosed in parentheses.

They take the following values:

- `Never`: no space is added
- `Always`: space is added
- `AfterSymbolic`: space is added if the infix operator or function name is
  symbolic (doesn't start with a letter or underscore)

Please note that these parameters will not affect spacing after an
[unary operator](https://docs.scala-lang.org/scala3/reference/changed-features/operators.html#unary-operators)
(i.e., one of `+`, `-`, `!`, `~`), as it's neither a function call nor an infix.

Also, `spaces.beforeApplyArgInParens` generalizes the special-case parameter
`spaces.afterTripleEquals` which only applies to a `===` function call.

```scala mdoc:defaults
spaces.beforeApplyArgInParens
spaces.beforeInfixArgInParens
```

```scala mdoc:scalafmt
spaces.beforeApplyArgInParens = Always
spaces.beforeInfixArgInParens = Always
---
+(baz)
===(baz)
bar(baz)

foo +(baz)
foo ===(baz)
foo bar(baz)
```

```scala mdoc:scalafmt
spaces.beforeApplyArgInParens = Never
spaces.beforeInfixArgInParens = Never
---
+ (baz)
=== (baz)
bar (baz)

foo + (baz)
foo === (baz)
foo bar (baz)
```

```scala mdoc:scalafmt
spaces.beforeApplyArgInParens = AfterSymbolic
spaces.beforeInfixArgInParens = AfterSymbolic
---
+ (baz)
===(baz)
bar (baz)

foo +(baz)
foo ===(baz)
foo bar (baz)
```

### `spaces.afterColonInMatchPattern`

> Since v3.8.3.

```scala mdoc:defaults
spaces.afterColonInMatchPattern
```

This parameter controls whether to output a space between a colon and a type in
typed match patterns. It takes the following values:

- `Always`:

```scala mdoc:scalafmt
spaces.afterColonInMatchPattern = always
---
foo match {
  case e: Etype | f: Ftype =>
  case g: Gtype =>
}
```

- `Never`:

```scala mdoc:scalafmt
spaces.afterColonInMatchPattern = never
---
foo match {
  case e: Etype | f: Ftype =>
  case g: Gtype =>
}
```

- `NoAlternatives` (when there are no pipe-separated alternatives):

```scala mdoc:scalafmt
spaces.afterColonInMatchPattern = noAlternatives
---
foo match {
  case e: Etype | f: Ftype =>
  case g: Gtype =>
}
```

## Literals

> Since v2.5.0.

Scalafmt allows flexible configuration of
[Integer](https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html#integer-literals)
and
[Floating Point](https://www.scala-lang.org/files/archive/spec/2.13/01-lexical-syntax.html#integer-literals)
literals formatting.

Default formatting:

```scala mdoc:scalafmt
---
123l
0XFff
0x1Abl
10E-1
10e-1D
0B111
0b111l
```

Each `literals.*` setting has three available options: `Upper`, `Lower`,
`Unchanged`.

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

### `literals.binPrefix`

> Since v3.8.4.

```scala mdoc:defaults
literals.binPrefix
```

Responsible for the case of binary integer literals prefix `0b`

```scala mdoc:scalafmt
literals.binPrefix=Lower
literals.long=Upper
---
0B111
0B111l
```

### `literals.floatingPoint`

This sections contains settings controlling formatting of floating-point
(float, double, generic) literals.

Also see the [`RewriteLiterals` rule](#rewriteliterals)
which defines additional settings in this section.

#### `literals.floatingPoint.float`

```scala mdoc:defaults
literals.floatingPoint.float
```

Responsible for the case of `Float` literals suffix `F`

```scala mdoc:scalafmt
literals.floatingPoint.float = Lower
---
42.0F
```

#### `literals.floatingPoint.double`

```scala mdoc:defaults
literals.floatingPoint.double
```

Responsible for the case of `Double` literals suffix `D`

```scala mdoc:scalafmt
literals.floatingPoint.double = Lower
---
42.0d
```

#### `literals.floatingPoint.scientific`

```scala mdoc:defaults
literals.floatingPoint.scientific
```

Responsible for the case of `Double` literals exponent part

```scala mdoc:scalafmt
literals.floatingPoint {
  scientific = Upper
  float = Lower
}
---
10e-1
10e-1f
```

## XML

Controls formatting of Scala embedded within XML.

### `xmlLiterals.assumeFormatted`

> Since v2.6.0.

If set, formats embedded Scala relative to containing XML, making the assumption
that XML itself is properly formatted. Otherwise, formatting is relative to the
outer Scala code which contains the XML literals.

```scala mdoc:defaults
xmlLiterals.assumeFormatted
```

```scala mdoc:scalafmt
maxColumn = 40
xmlLiterals.assumeFormatted = true
---
object Example2 {
  def apply() = {
      <foo>
        <bar>{ (1 + 2 + 3).toString("some long format") }</bar>
      </foo>
  }
}
```

```scala mdoc:scalafmt
maxColumn = 40
xmlLiterals.assumeFormatted = false
---
object Example2 {
  def apply() = {
      <foo>
        <bar>{ (1 + 2 + 3).toString("some long format") }</bar>
      </foo>
  }
}
```

## Binpacking

### `binPack` presets

One can specify the following values to `binPack.preset`:

- `never` or `false`:
  - [`callSite = Never`](#binpackxxxsite)
  - [`defnSite = Never`](#binpackxxxsite)
  - [`parentConstructors = Never`](#binpackparentconstructors)
- `Always` or `true`:
  - [`callSite = Always`](#binpackxxxsite)
  - [`defnSite = Always`](#binpackxxxsite)
  - [`parentConstructors = Always`](#binpackparentconstructors)
- `Oneline`:
  - [`callSite = Oneline`](#binpackxxxsite)
  - [`defnSite = Oneline`](#binpackxxxsite)
  - [`parentConstructors = Oneline`](#binpackparentconstructors)
- `Onelinesjs`:
  - [`callSite = OnelineSjs`](#binpackxxxsite)
  - [`defnSite = OnelineSjs`](#binpackxxxsite)
  - [`parentConstructors = Oneline`](#binpackparentconstructors)

The rest of parameters retain their default behaviour.

### Literal argument lists

This group of parameters controls binpacking of an argument list if _all_ arguments are
considered to be literals.
These parameters take precedence over [forcing of config style](#forcing-config-style)
or other parameters [controlling binpacking](#binpackxxxsite).

The following parameters affect this behaviour:

- `binPack.literalArgumentLists`: if false, this behaviour is disabled, other parameters ignored
- `binPack.literalsMinArgCount`: doesn't apply binpacking to calls with fewer arguments
- `binPack.literals{Include,Exclude}`: lists of regular expressions which define a literal
- (since v2.5.0) `binPack.literalsIncludeSimpleExpr`: allows a few selects (i.e. `a.b`),
  followed by a few nested single-argument apply calls, with literals as arguments
  - since v3.3.2, also includes `new`
- (since v2.5.0) `binPack.literalsSingleLine`: the entire argument list will be formatted on
  one line, regardless of `maxColumn`

```scala mdoc:defaults
binPack.literalArgumentLists
binPack.literalsMinArgCount
binPack.literalsInclude
binPack.literalsExclude
binPack.literalsIncludeSimpleExpr
binPack.literalsSingleLine
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

```scala mdoc:scalafmt
binPack.literalArgumentLists = true
binPack.literalsSingleLine = true
---
val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1,
  0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)
```

### `binPack.parentConstructors`

Parent constructors are `B` (since 3.4.1), `C` and `D` in `class A extends B with C and D`. Changed
from a boolean to a wider set of options in v2.6.0.

```scala mdoc:defaults
binPack.parentConstructors
```

The behaviour of `binPack.parentConstructors = source` depends on the value of
[`newlines.source`](#newlinessource); `keep` maps to `keep` and attempts to preserve the
space if there's no line break in the source, `fold` maps to `Oneline`, rest to `Never`.

#### `binPack.parentConstructors=Always`

This option attempts to binpack parents, formatting as many on each line as will fit.

```scala mdoc:scalafmt
binPack.parentConstructors = Always
maxColumn = 30
---
object A {
  trait Foo
  extends Bar
  with Baz
  trait Foo extends Bar with Baz
}
```

#### `binPack.parentConstructors=Never`

This option will attempt to format the entire declaration on one line (starting
with `class` or `trait` and including all parents); otherwise, will enforce
breaks before each.

```scala mdoc:scalafmt
binPack.parentConstructors = Never
maxColumn = 30
---
object A {
  trait Foo extends Bar with Baz
}
```

#### `binPack.parentConstructors=Oneline`

This option will attempt to format all parents on one line (starting with
`extends` and including all parents); otherwise, will enforce breaks before each.

```scala mdoc:scalafmt
binPack.parentConstructors = Oneline
maxColumn = 30
---
object A {
  class Foo(a: Int)
  extends Bar
  with Baz

  class Foo(
    a: Int
  )
  extends Bar
  with Baz
}
```

#### `binPack.parentConstructors=OnelineIfPrimaryOneline`

This option will attempt to format all parents on one line (from `extends` and
including all parents), but only if the primary constructor fits on one line
as well (the same or previous); otherwise, will enforce breaks before each.

```scala mdoc:scalafmt
binPack.parentConstructors = OnelineIfPrimaryOneline
maxColumn = 30
---
object A {
  class Foo(a: Int, b: Int)
  extends Bar
  with Baz

  class Foo(
    a: Int,
    b: Int
  ) extends Bar with Baz
}
```

#### `binPack.parentConstructors=keep`

This option attempts to preserve breaks before each parent.

```scala mdoc:scalafmt
binPack.parentConstructors = keep
---
object A {
  class Foo(a: Int, b: Int) extends Bar(
    a,
    b
  ) with Baz
  with Qux
  class Foo(a: Int, b: Int)
    extends Bar(
      a,
      b
    ) with Baz
    with Qux
}
```

#### `binPack.parentConstructors=ForceBreak`

This option will enforce a break before each parent. As usual, the break is only
actually introduced if indented position on the next line is less than the current.
Added in 3.8.4.

```scala mdoc:scalafmt
binPack.parentConstructors = ForceBreak
maxColumn = 45
---
object A {
  class Foo(a: Int) extends Bar with Baz
  class Foo(a: Int, b: Int, c: String, d: Double) extends Bar with Baz
}
```

### `binPack.xxxSite`

Controls binpacking around method/type definition sites (`binPack.defnSite`) or
method call sites (`binPack.callSite`) (both were called
`unsafeXxxSite` up until v3.8.1). The following parameter values are supported
since v3.0.0:

- `Never` disables the functionality (also takes `false`)
- `Always` enables the functionality (also takes `true`)
- `Oneline` ensures multiline arguments are not binpacked, that is, they cannot
  be on the same line as any other argument; also, if the multiline argument is
  last and the call site is followed by either a curried argument clause or
  a chained method, it cannot be on the same line as the next identifier after
  the closing delimiter
- `OnelineSjs` ensures multiline arguments are not binpacked, like `Oneline` above, but
  ignores line breaks within expressions in braces

Additionally, when binpacking is enabled (i.e., not `Never`), some nested
expressions in brackets (if the binpacked clause uses brackets) or in braces
(if it uses parentheses) wouldn't be indented as would happen in usual cases;
this logic is always used for `OnelineSjs`, and only for single-element clauses
otherwise.

When not disabled, these parameters have complex interactions with
[`newline.source`](#newlinessource),
[`newlines.configStyle.xxxSite.prefer`](#newlinesconfigstylexxxsiteprefer)
(aka `cfgStyle` below) and
[`danglingParentheses.xxxSite`](#newlines-danglingparentheses) (aka `dangle`).
Keep in mind that when [config-style is forced](#forcing-config-style),
it takes precedence over options described below.

The code that is being binpacked might also yield an additional force over
the selection of breaks, such as when dealing with multiline arguments
(they behave differently under `Oneline` setting, or depending on
[alignment](#alignopenparencallsite)).

- `newlines.source=classic`
  - `cfgStyle=T`, `dangle=T`:
    use cfg-style if both parens had breaks, otherwise binpack without breaks
    - before v3.8.2, this formatting was used for `callSite` with `dangle=F` as well
  - `cfgStyle=T`, `dangle=F`:
    ([scala.js](#presetscalajs))
    use cfg-style if close paren had a break; otherwise, binpack without breaks
  - `cfgStyle=F`, `dangle=T`:
    binpack; if both parens had breaks, keep; otherwise, use no breaks
    - before v3.8.2, this formatting was used for `defnSite` with
      `cfgStyle=T` and any `dangle`
  - `cfgStyle=F`, `dangle=F`:
    binpack without breaks
    - before v3.8.2, this formatting was used for `callSite` with
      `cfgStyle=F` and any `dangle`, and for `defnSite` with
      `cfgStyle=F` and `dangle=F`
- `newlines.source=keep`: always preserve break after open paren
  - `cfgStyle=T`, `dangle=T`:
    use cfg-style if open paren had a break; otherwise, binpack and keep close break
  - `cfgStyle=T`, `dangle=F`:
    ([scala.js](#presetscalajs))
    use cfg-style if close paren had a break; otherwise, binpack and keep close break
  - `cfgStyle=F`, `dangle=T`:
    binpack; force close break if open paren had a break; otherwise, preserve it
  - `cfgStyle=F`, `dangle=F`:
    binpack; preserve close break
- `newlines.source=fold`: if single line is not possible:
  - `cfgStyle=T`, `dangle=T`:
    binpack with both breaks
  - `cfgStyle=T`, `dangle=F`:
    binpack with dangling open and tucked close
  - `cfgStyle=F`, `dangle=T`:
    binpack with tucked open and dangling close
    - if [`binPack.indentCallSiteOnce`](#binpackindentcallsiteonce) is set,
      we will not force dangling as it might lead to consecutive lines
      with a closing parenthesis at the same indentation level
  - `cfgStyle=F`, `dangle=F`:
    binpack without breaks
- `newlines.source=unfold`: if single line is not possible:
  - open dangles, close break matches `dangle`, `cfgStyle` is ignored

> Please also see [callSite indentation parameters](#indent-for-binpackcallsite).

### `binPack.bracketXxxSite`

> Since v3.0.4.

If set explicitly, will be used for type arguments or parameters,
instead of the respective [`binPack.xxxSite`](#binpackxxxsite).

### `binPack.importSelectors`

Import selectors (those grouped in `{...}`) will always be formatted on a single
line if they fit without exceeding `maxColumn`. This parameter controls how they
will be handled _if_ they overflow.
(Prior to v3.8.4, it was called `importSelectors`.)

Takes the following parameters:

- `unfold`: format one per line (prior to v3.8.4, called `noBinPack`)
- `fold`: fit as many as possible on each line (prior to v3.8.4, called `binPack`)
- `keep`: preserve breaks between selectors (added in v3.10.5)
  - this setting is not allowed if [`Imports`](#imports) rewrite rule is enabled
- `singleLine`: format all on one line

By default, the parameter will match [`newlines.source`](#newlinessource) if
the latter is explicitly specified,
and `unfold` otherwise (prior to v3.10.1, it would always be `unfold`).
See also [`danglingParentheses.importSite`](#danglingparenthesesimportsite).

However, if [`Imports`](#imports) rewrite rule is enabled, which discards
existing formatting and completely regenerates imports, the default for
[`newlines.source = keep`](#newlinessource) will be replaced with `fold` if
[`rewrite.imports.selectors = fold`](#imports-selectors), and `unfold` otherwise.

```scala mdoc:scalafmt
maxColumn = 10
binPack.importSelectors = unfold
---
import a.b.{c, d, e, f, g}
```

```scala mdoc:scalafmt
maxColumn = 10
binPack.importSelectors = fold
---
import a.b.{c, d, e, f, g}
```

```scala mdoc:scalafmt
maxColumn = 10
binPack.importSelectors = singleLine
---
import a.b.{c, d, e, f, g}
```

## Select chains

The parameters below control formatting of select chains, such as which select
expressions are considered to start a chain.

Generally, a chain can either be formatted on one line up to the last select, or
will have a break on the first select.

when

### `newlines.selectChains.style`

This parameter controls how select chains (sequences of `.method` invocations)
are formatted.

It takes the same values as [newlines.source](#newlinessource); use `null`
(default) to fall back on the current value of `newlines.source`.

> Since v3.0.0.

- `keep`: attempts to preserve break
- `fold`: attempts to avoid breaks
- `unfold`: forces breaks on each select unless all fit on a single line
- `classic` (i.e., `null` and `newlines.source` is not specified).

### `newlines.selectChains.enclose`

> - Added in v2.6.2
> - Before v3.8.4, was called `optIn.encloseClassicChains` and applied only to `style = classic`
> - By default, enabled unless `style = classic`

Controls what happens if a chain enclosed in parentheses is followed by
additional selects. Those additional selects will be considered part of the
enclosed chain if and only if this flag is false.

```scala mdoc:defaults
newlines.selectChains.enclose
```

```scala mdoc:scalafmt
newlines.selectChains.enclose = true
maxColumn = 30
---
(foo.map(_ + 1).map(_ + 1))
  .filter(_ > 2)
```

```scala mdoc:scalafmt
newlines.selectChains.enclose = false
maxColumn = 30
---
(foo.map(_ + 1).map(_ + 1))
  .filter(_ > 2)
```

### `newlines.selectChains.classicCanStartWithBraceApply`

> - Prior to v3.8.4, was called `includeCurlyBraceInSelectChains`
> - Applies only if `style = classic` (always enabled for others)

Controls if select followed by curly braces can _start_ a chain.

```scala mdoc:defaults
newlines.selectChains.classicCanStartWithBraceApply
```

```scala mdoc:scalafmt
newlines.selectChains.classicCanStartWithBraceApply = true
---
List(1).map { x =>
    x + 2
  }
  .filter(_ > 2)
```

```scala mdoc:scalafmt
newlines.selectChains.classicCanStartWithBraceApply = false
---
List(1)
  .map { x =>
    x + 2
  }
  .filter(_ > 2)
```

### `newlines.selectChains.classicCanStartWithoutApply`

> - Prior to v3.8.4, was called `includeNoParensInSelectChains`
> - Applies only if `style = classic` (always enabled for others)

Controls if select _not_ followed by an apply can _start_ a chain.

```scala mdoc:defaults
newlines.selectChains.classicCanStartWithoutApply
```

```scala mdoc:scalafmt
newlines.selectChains.classicCanStartWithoutApply = true
---
List(1).toIterator.buffered
  .map(_ + 2)
  .filter(_ > 2)
```

```scala mdoc:scalafmt
newlines.selectChains.classicCanStartWithoutApply = false
---
List(1).toIterator.buffered.map(_ + 2).filter(_ > 2)
```

### `newlines.selectChains.classicKeepFirst`

> - Prior to v3.8.4, was called `optIn.breakChainOnFirstMethodDot`
> - Applies only if `style = classic`

Keeps the break on the first select of the chain if the source contained one.
Has no effect if there was no newline in the source.

```scala mdoc:defaults
newlines.selectChains.classicKeepFirst
```

```scala mdoc:scalafmt
newlines.selectChains.classicKeepFirst = false
---
// collapse into a single line
foo
  .map(_ + 1)
  .filter(_ > 2)
```

```scala mdoc:scalafmt
newlines.selectChains.classicKeepFirst = true
---
// preserve break on first dot and break on subsequent dots
foo
  .map(_ + 1).filter(_ > 2)
```

### `newlines.selectChains.classicKeepAfterFirstBreak`

> - Prior to v3.8.4, was called `optIn.breaksInsideChains`
> - Applies only if `style = classic`

Controls whether to preserve a newline before each subsequent select when the
very first one used a line break; that is, this parameter doesn't prohibit
single-line formatting even if there are source breaks down the chain.

If false, each subsequent select within the chain will behave exactly like the
first, that is, either the entire chain will be formatted on one line, or will
contain a break on every select.

If true, preserves existence or lack of breaks on subsequent selects if the
first select was formatted with a newline.

```scala mdoc:defaults
newlines.selectChains.classicKeepAfterFirstBreak
```

```scala mdoc:scalafmt
newlines.selectChains.classicKeepAfterFirstBreak = true
maxColumn = 35
---
foo.bar(_ + 1)
  .baz(_ > 2).qux
foo.bar(_ + 1).baz(_ > 2).qux
foo.bar(_ + 1).baz(_ > 2).qux(_ * 12)
foo.bar(_ + 1).baz(_ > 2).qux { _ * 12 }
foo.bar(_ + 1)
  .baz(_ > 2).qux(_ * 12)
```

```scala mdoc:scalafmt
newlines.selectChains.classicKeepAfterFirstBreak = false
maxColumn = 35
---
foo.bar(_ + 1)
  .baz(_ > 2).qux
foo.bar(_ + 1).baz(_ > 2).qux
foo.bar(_ + 1).baz(_ > 2).qux(_ * 12)
foo.bar(_ + 1).baz(_ > 2).qux { _ * 12 }
foo.bar(_ + 1)
  .baz(_ > 2).qux(_ * 12)
```

## Miscellaneous

### `lineEndings`

This parameter controls which end-of-line character sequences will be output,
takes the following values:

- `unix`: outputs LF (`U+000A`)
- `windows`: uses CRLF (`U+000D U+000A`)
- `preserve`: if an input file _contains_ CRLF anywhere, use CRLF for every line; otherwise, LF

By default, this parameter is assumed to be set to `unix`. However, since
v3.8.3, if the formatter is invoked on Windows, [`project.git`](#projectgit)
is set and `git` parameter
[`core.autocrlf`](https://git-scm.com/book/en/v2/Customizing-Git-Git-Configuration#_core_autocrlf)
is configured, then default value will be changed to `windows` if
`autocrlf=true`, and `preserve` if `false`.

## Markdown Formatting

> Since v3.0.0.

Will format all `scala mdoc` fences inside Markdown files.

```conf
# Format default filetypes + Markdown files
project.includePaths."+" = ["glob:**.md"]

# *Only* format Markdown files
project.includePaths = [ "glob:**.md" ]
```

Before:

````markdown
Markdown prose beginning.

```scala mdoc
val x  =       3
```

Markdown prose end.
````

After:

````markdown
Markdown prose beginning.

```scala mdoc
val x = 3
```

Markdown prose end.
````

## Advanced: formatting process

Currently, the formatting process consists of several stages:

- code parsing using [scalameta parser](https://scalameta.org/)
  - keep in mind that any parsing errors output by scalafmt are
    in fact coming from scalameta
- format-agnostic rewrites
  - during this step, we apply most of the [rewrites rules](#rewrite-rules)
    (except those which depend on how we decided to format the code)
- preparing whitespace splits
  - we iterate over all pairs of adjacent non-whitespace tokens and identify
    possible options for the output whitespace between them
  - selection is between
    - a newline with a possible subsequent indentation
    - a single space
    - no space at all
  - each option also comes with a penalty and a possible policy on how to handle
    subsequent splits
    - for instance, this is how we ensure single-line formatting, by adding a
      policy which removes all newline splits before a given end token
- route search
  - we look for an optimal path through the space of non-whitespace tokens
    and splits between them
  - each partial state includes, among other things:
    - the last token considered
    - the split selection preceding it
    - total cumulative penalty
    - the partial state for the previous token
  - we maintain a priority queue containing these partial states, by preferring
    states with lower penalty and then higher token offset (i.e. those which are
    closer to the completion)
  - at each iteration, we select a state from that priority queue (we'll call
    this event **a state visit**), apply all available non-expired policies to
    available subsequent whitespace splits, insert the next state for each of
    them back into the priority queue
  - we stop as soon as the top state in the priority queue is completed (i.e.,
    refers to the `EOF` token)
- output
  - after we have obtained our final formatting state, we need to write out the
    code applying the splits selected during the route search
  - as part of this final stage, we perform a few steps:
    - apply format-dependent rewrites (as these frequently need to know how
      many line breaks we have selected to output for a given code passage);
      for instance,
      - [inserting](#inserting-braces) braces,
      - [adding](#rewritescala3endmarkerinsertminspan) or
        [removing](#rewritescala3endmarkerremovemaxspan) end markers
      - modifying [blank lines](#newlines-around-package-or-template-body)
    - rewrite comments and docstrings if configured
    - [rewrite trailing commas](#trailing-commas):
      as part of this rule, we _remove_ any trailing commas during the
      format-agnostic rewrites stage and _add_ them as needed during this
      stage, if our splits solution ended up with a line break before the
      closing paren or brace
    - alignment: determine any additional horizontal space to be added
      - if alignment is for some reason is not possible, we **don't** go back
        to the previous stage (route search) to determine a different splits
        solution which might possibly make alignment possible
      - this might happen, for instance, if alignment leads to
        [disallowed line overflow](#alignallowoverflow)

## Advanced: Route search optimizations

There are several configuration parameters (most of them under the
`runner.optimizer` section) which control some aspects of the algorithm during
this formatting stage.

### Route search optimizations: giving up

The following parameters control when we abort the search and throw a
`SearchStateExploded` exception:

- `runner.maxStateVisits`:
  when the total number of state visits exceeds this value
- `runner.optimizer.maxVisitsPerToken`:
  when the number of state visits for a given token exceeds this value (also see below)
- `runner.optimizer.escapeInPathologicalCases`:
  if this flag is disabled, ignore `maxVisitsPerToken` above

```scala mdoc:defaults
runner.maxStateVisits
runner.optimizer.maxVisitsPerToken
runner.optimizer.escapeInPathologicalCases
```

### Route search optimizations: discarding some states

Normally, the search algorithm considers any state as a possible intermediate
state which might complete a valid solution.

However, in some cases we might decide to require certain conditions to be met
for a state to be considered. A state need to qualify under at least one of
the checks controlled by these parameters:

- `runner.optimizer.pruneSlowStates`:
  - accepts `true` or `false`; also, since v3.8.4, `No`, `Yes` or `Only`
  - if this flag is disabled (`false` or `No`), any state qualifies
  - if it is enabled:
    - if the search algorithm is ultimately unable to find a completed solution
      (because some states might be discarded) and if the value is not `Only`,
      then it will disable the flag and make another attempt
    - during the first run, when the flag is enabled, a state qualifies if it
      is not "worse" than a previously recorded "best" state for this token
    - the "best" state is the first state with a newline split at the given
      token (recursive calls over subsequences are excluded, see sections below)
    - a state is "worse" when it has higher penalty and wider indentation
- `runner.optimizer.disableOptimizationsInsideSensitiveAreas`:
  a state qualifies if this flag is _disabled_ or the current token is within
  a so-called no-optimization zone (and if the state satisfies these criteria,
  we'll call it a no-optimization state):
  - between matching parentheses of method call (either
    those enclosing the method arguments, or the entire call itself)
  - between matching braces of a type refinement

```scala mdoc:defaults
runner.optimizer.disableOptimizationsInsideSensitiveAreas
runner.optimizer.pruneSlowStates
```

### Route search optimizations: preferential treatment

In some cases, we modify the search logic to boost priority of some states.
The following sections describe when and how this might be done.

#### Route search optimizations: optimal hints

Some splits might be marked as starting a token subsequence with an optimal end
token (for example, a segment which must be formatted without line breaks).

In such cases, if flag `runner.optimizer.acceptOptimalAtHints` is enabled and
the hint is deemed valid (see below), the route search algorithm calls itself
recursively with the state that ends in this "optimal" split and just on this
shorter subsequence, returning the first state which completed this subsequence,
with the following restrictions:

- the state penalty at the end must be the same as at the beginning, so no splits
  within the subsequence can have **non-zero** penalty
- the so-called nested depth is incremented (set to zero initially) and then
  must not exceed `runner.optimizer.maxDepth`

There are several checks that are performed to determine validity of the hint:

- no optimal subsequence has yet been identified for the current state
- there are multiple possible next states; we don't do this if there's only one
- the split has **zero** penalty, including from any unexpired policies

```scala mdoc:defaults
runner.optimizer.acceptOptimalAtHints
runner.optimizer.maxDepth
```

#### Route search optimizations: longer blocks

Another form of getting ahead faster is by recursing on a long-enough block
(whether enclosed in explicit braces or by using significant indentation),
where "long enough" is loosely defined as having sufficient non-whitespace
characters to fill at least _three_ dense `maxColumn` lines.

Unlike the case with optimal hints above, there's no restriction on the
penalty added by the splits within the block.

This optimization is enabled when:

- `runner.optimizer.recurseOnBlocks` is enabled, and
- the token is within a no-optimization zone.

```scala mdoc:defaults
runner.optimizer.recurseOnBlocks
```

#### Route search optimizations: statements

Another way to boost priority is simply by adding a new priority queue,
so that any earlier states, from the existing priority queue, wouldn't
be considered until the new one is exhausted.

We might create a new priority queue if we encounter a new block statement,
and this optimization is enabled when:

- `runner.optimizer.dequeueOnNewStatements` is enabled, and
- we are in a recursive call on a subsequence or the token is _not_ within
  a no-optimization zone.

```scala mdoc:defaults
runner.optimizer.dequeueOnNewStatements
```

#### Route search optimizations: arg or param clause

Similar to statements above, we might create a new priority queue when we
encounter a long-enough call-site argument or defn-site parameter clause.

```scala mdoc:defaults
runner.optimizer.callSite
runner.optimizer.defnSite
```

This optimization is enabled when all these criteria are satisfied:

- `xxxSite.minSpan`:
  must be non-negative and not exceed the character span covered by the entire
  clause, excluding any whitespace
  - prior to v3.8.1, this parameter was called `forceConfigStyleOnOffset`
  - prior to v3.8.4, the parameter had to be strictly less than the span, and
    the span calculation excluded the last token of the clause (a closing delim
    spanning a single-character, unless optional braces had been used)
- `xxxSite.minCount`:
  must be positive and not exceed the number of arguments
  (prior to v3.8.2, this parameter was called `forceConfigStyleMinCount`)

> These parameters cannot be [overridden within a file](#for-code-block)

## Edition

> Removed in 2.7.0

Editions are no longer used. They were kept for backwards compatibility with old
configuration files but new changes to the default Scalafmt formatting behavior
will not respect the `edition` setting.

## Other

To find all available configuration options, it's best to browse the source code
of Scalafmt. A good place to start is `ScalafmtConfig`. Observe that this
listing below is the top-level, there are more configuration options if you
visited nested fields like `spaces` and `newlines`.

> The values for parameters below are purely for informational purposes. They use
> pseudo-formatting similar to but incompatible with `scalafmt.conf`.
> Some of them show values which can't be explicitly specified.

```scala mdoc:defaults:all

```
