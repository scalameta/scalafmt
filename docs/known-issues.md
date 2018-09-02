---
id: known-issues
title: Known Issues
---

[Developing code formatters is notoriously hard](http://journal.stuffwithstuff.com/2015/09/08/the-hardest-program-ive-ever-written/)
and scalafmt has been no exception.

The following are the biggest known issues with scalafmt:

## Deeply nested code

AKA. "Search state exploded"

0.5.0 made big improvements on this issue. In my test corpus, only 13 out of
27.000 source files trigger this error now.

Scalafmt cannot yet format all files with deeply nested functions calls. Deeply
nested code is troublesome because the number of possible formatting options
grows exponentially with each new layer of nesting. Instead of taking seconds or
minutes to complete formatting, scalafmt chooses to bail early and leave the
source file unformatted.

There are two workaround if you are affected by this issue:

* Wrap the offending block with `// format: off`. The SBT/IntelliJ/CLI
  integrations should point you to the line where scalafmt starts to struggle.

* Try `align.openParenCallSite = false` to shrink the search space.

* Refactor your code to eliminate deeply nested function calls. Binding large
  function arguments to variables is a good start.

* Other cool code formatters like ClangFormat, dartfmt and rfmt use better
  techniques to solve this issue, which scalafmt can maybe learn from.

## Non-idempotent

Scalafmt is non-idempotent for certain files. See
https://github.com/scalameta/scalafmt/issues/339. This means you should be
careful about enforcing scalafmt in your CI build.

In my test corpus, as of v0.5.0, only 12 files out of 27.000 source files are
affected by this issue. If sources are already formatted by scalafmt, no source
file in the test corpus triggers non-idempotent formatting.

> **Pro tip**. As awkward as I feel recommending this, you may want to run
> scalafmt twice on your codebase for the first time you reformat it.

## Performance

Scalafmt is 6x slower than Scalariform. For 98% of source files this won't be a
problem if you have a decently modern laptop. However, if you only work in files
with 4.000 LOC it might be a problem. I'm quite sure that micro-optimizations
can squeeze out at least ~2x performance improvements, maybe even more.
Moreover, I think incremental formatting has the possibility to increase the
performance by several orders of magnitude in interactive IDE environments where
scalafmt is invoked on file save.
