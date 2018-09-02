---
id: gotchas
title: Gotchas
---

Scalafmt tries to automatically reformat as much as possible. However, sometimes
you need to help scalafmt decide how to format your code.

## Infix applications

Infix applications are methods calls that use the syntax like `a + b` instead of
`a.+(b)`.

Scalafmt preserves your line breaks in infix applications, even if this means
the `maxColumn` setting is not respected.

```scala
// column limit |
// if you have long infix appplications
a.b(c) && d.e(f, g, h)

// then scalafmt may format like this
a.b(c) && d.e(
  f, g, h)

// which is ugly. You can fix it by inserting
// a newline after && and it will look like this
a.b(c) &&
  d.e(f, g, h)
```

## Config style

You can use "config style" to tell scalafmt to break a function application.

```scala
// Put newline after opening (
// and newline before closing )
// to force one argument on each line.

// OK: Config style
function(
  longerArg1 = defaultValue1,
  longerArg2 = defaultValue2,
  longerArg3 = defaultValue3
)
// NOT Config style
function(longerArg1 = defaultValue1,
          longerArg2 = defaultValue2,
          longerArg3 = defaultValue3)
```
