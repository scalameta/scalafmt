# scalafmt [![Build Status](https://travis-ci.org/olafurpg/scalafmt.svg?branch=master)](https://travis-ci.org/olafurpg/scalafmt) [![Join the chat at https://gitter.im/olafurpg/scalafmt](https://badges.gitter.im/olafurpg/scalafmt.svg)](https://gitter.im/olafurpg/scalafmt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge) [![Latest version](https://index.scala-lang.org/olafurpg/scalafmt/scalafmt/latest.svg?color=orange)](https://index.scala-lang.org/olafurpg/scalafmt/scalafmt-core)

### [User documentation][docs]
Head over to [the user docs][docs] for instructions on how to install scalafmt.

### Quick help

- Run formatting tests: `core/testOnly org.scalafmt.FormatTests`.
- Write new formatting test: read [this doc](core/src/test/resources/readme.md).
- Build docs: `sbt readme/run` will create the docs, which you can open with
  `open readme/target/scalatex/index.html`. Docs are built with [Scalatex](http://www.lihaoyi.com/Scalatex/).
- Hack on IntelliJ plugin: see [this doc](intellij/readme.md).
- Hack scalafmt: see [tutorial](tutorial.md).
- Hack on SBT plugin: run `sbt scripted`.
- Run jmh benchmarks: `run-benchmarks.sh`.
- Run formatter on millions of lines of code: `core/test:runMain  org.scalafmt.FormatExperimentApp`:
- Debug performance: after each test run in `FormatTests`, a flamegraph report
  like [this one](https://github.com/olafurpg/scalafmt/issues/140)
  is generated in `target/index.html`. 
  I usually keep a browser tab open at `localhost:3000/target/index.html`
  along with this background process:
  `browser-sync start --server --files "target/*.html"`.
  See [Browsersync](https://www.browsersync.io/).

### Tutorial
There's a details scalafmt: see [tutorial](tutorial.md).

[docs]: http://scalafmt.org

## Acknowledgements

<a href="http://www.ej-technologies.com/products/jprofiler/overview.html">
  <img src="https://www.ej-technologies.com/images/product_banners/jprofiler_large.png" align="right" />
 </a>
[JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html) generously supports scalafmt with its full-featured Java Profiler.
