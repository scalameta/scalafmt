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

```tut:passthrough
website.default(_.maxColumn)
```

* Keep in mind that 80 characters fit perfectly on a split laptop screen with
  regular resolution.
* Github mobile view only shows 80 characters and sometimes you might review
  code on your phone.
* Consider refactoring your code before of choosing a value above 100.

### `docstrings`

```tut:passthrough
website.default(_.docstrings)
```

```tut:passthrough
website.exampleBlock(
  s"""
/** Align by second asterisk.
  *
  */
""",
  "docstrings = ScalaDoc")
```

```tut:passthrough
website.exampleBlock(
  s"""
/** Align by first asterisk.
 *
 */
""",
  "docstrings = JavaDoc")
```

### `assumeStandardLibraryStripMargin`

```tut:passthrough
website.default(_.assumeStandardLibraryStripMargin)
```

> May cause non-idempotent formatting in rare cases, see
> https://github.com/scalameta/scalafmt/issues/192.

If `true`, the margin character `|` is aligned with the opening triple quote
`"""` in interpolated and raw string literals.

```tut:passthrough
website.exampleBlock("""
  val example1 =
    s'''Examples:
       #  * one
       #  * two
       #  * $three
       #'''.stripMargin
""",
  "assumeStandardLibraryStripMargin = true"
)
```

The pipe character can immediately follow the opening `"""`

```tut:passthrough
website.exampleBlock("""
  val example2 =
    s'''|Examples:
        #  * one
        #  * two
        #  * $three
        #'''.stripMargin
""",
  "assumeStandardLibraryStripMargin = true"
)
```

## Indentation

### `continuationIndent.callSite`

```tut:passthrough
website.default(_.continuationIndent.callSite)
```

Example:

```tut:passthrough
website.exampleBlock(
  s"""
function(
  argument1 // indented by 2
)""",
  "continuationIndent.defnSite = 2"
)
```

### `continuationIndent.defnSite`

```tut:passthrough
website.default(_.continuationIndent.defnSite)
```

Same as `continuationIndent.callSite` except for definition site. Example:

```tut:passthrough
website.exampleBlock(
  s"""
def function(
argument1: Type1 // indented by 4
): ReturnType""",
  "continuationIndent.defnSite = 4"
)
```

## Alignment

Default: **some**

Align has several nested fields, which you can customize. However, it comes with
four possible defaults: none, some, more, & most.

### `align`

#### `align=some`

```tut:passthrough
website.exampleBlock(
  s"""
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
""",
  "align=some"
)
```

#### `align=none`

```tut:passthrough
website.exampleBlock(
  s"""
x match { // false for case arrows
  case 2  => 22 // also comments!
  case 22 => 222 // don't align me!
}

def foo(a: Int, // false for defn site
        b: String): Int
foo(a: Int, // false for call site
    b: String): Int
""",
  "align=none"
)
```

> **Pro tip**: Enable this setting to minimize git diffs/conflicts from
> renamings and other refactorings.

#### `align=more`

```tut:passthrough
website.exampleBlock(
  s"""
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
""",
  "align=more"
)
```

#### `align=most`

```tut:passthrough
website.exampleBlock(
  s"""
for {
  // align <- with =
  x <- List()
  yy = 2
  // aligns body by arrow
  zzz <- new Integer {
    def value = 3
  }
} yield x
""",
  "align=most"
)
```

> **Note**. Only for the truest vertical aligners. This is a new option, feel
> free to open PR enabling more crazy vertical alignment here. Expect changes.

### `align.tokens`

Default: **[caseArrow]**

An align token is a pair of `code`, which is the string literal of an operator
of token, and `owner`, which is the kind of the closest tree node that owns that
token. If no `owner` is provided, then all tree kinds will be matched.

```tut:passthrough
website.exampleBlock(
  s"""
x match {
  case 1 => 1 -> 2
  case 11 => 11 -> 22
}
""",
  """align.tokens = [{code = "=>", owner = "Case"}]"""
)
```

```tut:passthrough
website.exampleBlock(
  s"""
val x = List(
"org.scala-lang" %% "scala-compiler" % scalaVersion.value,
"com.lihaoyi" %% "sourcecode" % "0.1.1"
)
""",
  """align.tokens = [{code = "%", owner = "Term.ApplyInfix"}, {code = "%%", owner = "Term.ApplyInfix"}]"""
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

```tut:passthrough
website.default(_.align.arrowEnumeratorGenerator)
```

```tut:passthrough
website.exampleBlock(
  s"""
for {
  x <- new Integer {
    def value = 2
  }
} yield x
""",
  "align.arrowEnumeratorGenerator = false"
)
```

```tut:passthrough
website.exampleBlock(
  s"""
for {
  x <- new Integer {
    def value = 2
  }
} yield x
""",
  "align.arrowEnumeratorGenerator = true"
)
```

### `align.openParenCallSite`

```tut:passthrough
website.default(_.align.openParenCallSite)
```

```tut:passthrough
website.exampleBlock(
  s"""
foo(arg1, arg2)

function(arg1, // align by (
          arg2,
          arg3)
function(
  argument1,
  argument2)
""",
  "align.openParenCallSite = true"
)
```

```tut:passthrough
website.exampleBlock(
  s"""
foo(arg1, arg2)
function(
  arg1, // no align by (
  arg2,
  arg3)
function(
  argument1,
  argument2)
""".stripMargin,
  "align.openParenCallSite = false"
)
```

### `align.openParenDefnSite`

```tut:passthrough
website.default(_.align.openParenDefnSite)
```

```tut:passthrough
website.exampleBlock(
  s"""
class IntString(int: Int, string: String)

class IntStringLong(int: Int,
                    string: String,
                    long: Long)
""",
  "align.openParenDefnSite = true"
)
```

```tut:passthrough
website.exampleBlock(
  s"""
class IntString(int: Int, string: String)

class IntStringLong(
  int: Int,
  string: String,
  long: Long
)
""",
  "align.openParenDefnSite = false"
)
```

## Newlines

The `newlines.*` options are used to configure when and where `scalafmt` should
insert newlines.

// TODO(gabro): link to Vertical Multiline section

> You might be interested in the Vertical Multiline section.

### `newlines.alwaysBeforeTopLevelStatements`

```tut:passthrough
website.default(_.newlines.alwaysBeforeTopLevelStatements)
```

```tut:passthrough
website.exampleSource(
  s"""
import org.scalafmt
      
package core {
  object O {
    val x1 = 1
    val x2 = 2
    def A = "A"
    def B = "B"
  }
}
""",
  "newlines.alwaysBeforeTopLevelStatements = false"
)
```

```tut:passthrough
website.exampleSource(
  s"""
import org.scalafmt

package core {
  object O {
    val x1 = 1
    val x2 = 2
    def A = "A"
    def B = "B"
  }
}
""",
  "newlines.alwaysBeforeTopLevelStatements = true"
)
```

### `newlines.sometimesBeforeColonInMethodReturnType`

```tut:passthrough
website.default(_.newlines.sometimesBeforeColonInMethodReturnType)
```

```tut:passthrough
website.exampleBlock(
  """implicit def validatedInstances[E](implicit E: Semigroup[E]) : Traverse[Validated[E, ?]] with ApplicativeError[Validated[E, ?], E] = 2""",
  "maxColumn = 70",
  "newlines.sometimesBeforeColonInMethodReturnType = true"
)
```

```tut:passthrough
website.exampleBlock(
  """implicit def validatedInstances[E](implicit E: Semigroup[E]) : Traverse[Validated[E, ?]] with ApplicativeError[Validated[E, ?], E] = 2""",
  "maxColumn = 70",
  "newlines.sometimesBeforeColonInMethodReturnType = false"
)
```

### `newlines.penalizeSingleSelectMultiArgList`

```tut:passthrough
website.default(_.newlines.penalizeSingleSelectMultiArgList)
```

```tut:passthrough
website.exampleBlock(
  """
logger.elem(a,
  b,
  c)""",
  "newlines.penalizeSingleSelectMultiArgList = true"
)
```

```tut:passthrough
website.exampleBlock(
  "logger.elem(a, b, c)",
  "newlines.penalizeSingleSelectMultiArgList = true"
)
```

See [this comment](https://github.com/scalameta/scalafmt/pull/611#issue-196230948) for further motivation.


### `newlines.alwaysBeforeElseAfterCurlyIf`

```tut:passthrough
website.default(_.newlines.alwaysBeforeElseAfterCurlyIf)
```

```tut:passthrough
website.exampleBlock(
  """
if(someCond) {
  foo()
} else {
  bar()
}""",
  "newlines.alwaysBeforeElseAfterCurlyIf = true"
)
```

```tut:passthrough
website.exampleBlock(
  """
if(someCond) {
  foo()
} else {
  bar()
}
""",
  "newlines.alwaysBeforeElseAfterCurlyIf = false"
)
```

## Rewrite Rules

To enable a rewrite rule, add it to the config like this `rewrite.rules = [SortImports]`.
 
### `AvoidInfix`

```tut:passthrough
website.formatExample(
  """
a success b
a error (b, c)
a map { x =>
  x + 2
}
"o" % "a" % "v" c(D)
future recover {
  case e: Err => 0
} map (_.toString)
""",
  "rewrite.rules = [AvoidInfix]"
)
```

### `ExpandImportSelectors`

```tut:passthrough
website.formatExample(
  """
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
""",
  "rewrite.rules = [ExpandImportSelectors]"
)
```

### `RedundantBraces`

> Warning. This rewrite can cause non-idempotent formatting,
see [#1055](https://github.com/scalameta/scalafmt/issues/1055). 

```tut:passthrough
website.formatExample(
  """
def foo = {
  List(1, 2, 3).sum
}
""",
  "rewrite.rules = [RedundantBraces]"
)
```

```tut:passthrough
website.formatExample(
  """
q"Hello ${name}"
""",
  "rewrite.rules = [RedundantBraces]",
  "rewrite.redundantBraces.stringInterpolation = true"
)
```

Configuration options and default values: 

- `rewrite.redundantBraces.maxLines = 100`
- `rewrite.redundantBraces.includeUnitMethods = true`
- `rewrite.redundantBraces.stringInterpolation = true`
- `rewrite.redundantBraces.generalExpressions = false` (disabled by default due to #1147)

### `RedundantParens`

```tut:passthrough
website.formatExample(
  """
for {
  a <- b
  if (a.nonEmpty)
} yield a
""",
  "rewrite.rules = [RedundantParens]"
)
```

### `SortModifiers`
Modifiers are sorted based on the given order. Affects modifiers of the following definitions: trait, class, object, type, and val+var, both as fields and class parameters. 

```tut:passthrough
website.formatExample(
  """
final lazy private implicit val x = 42
lazy final implicit private val y = 42
""",
  "rewrite.rules = [SortModifiers]"
)
```
```tut:passthrough
website.formatExample(
  """
 class Test(
  implicit
  final private val i1: Int,
  private final val i2: String
)
""",
  "rewrite.rules = [SortModifiers]"
)
```

```tut:passthrough
website.formatExample(
  """
sealed protected[X] trait ADT
final private case object A1 extends ADT
private final case class A2(x: Int) extends ADT
""",
  "rewrite.rules = [SortModifiers]"
)
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

Replaces parentheses into curly braces in for comprehensions that contain multiple enumerator generators.

```tut:passthrough
website.formatExample(
  """
for(a <- as; b <- bs if b > 2)
 yield (a, b)
""",
  "rewrite.rules = [PreferCurlyFors]"
)
```

### `SortImports`

The imports are sorted by the groups: symbols, lower-case, upper-case.

```tut:passthrough
website.formatExample(
  """
import foo.{Zilch, bar, Random, sand}
""",
  "rewrite.rules = [SortImports]"
)
```

### `AsciiSortImports`

The imports are sorted by their Ascii codes

```tut:passthrough
website.formatExample(
  """
import foo.{~>, `symbol`, bar, Random}
""",
  "rewrite.rules = [AsciiSortImports]"
)
```
