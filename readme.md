# scalafmt
[![Build Status](https://travis-ci.org/scalameta/scalafmt.svg?branch=master)](https://travis-ci.org/scalameta/scalafmt)
[![Build status](https://ci.appveyor.com/api/projects/status/7gha7cxm5lw8fsc3)](https://ci.appveyor.com/project/olafurpg/scalafmt/branch/master)
[![Join the chat at https://gitter.im/olafurpg/scalafmt](https://badges.gitter.im/olafurpg/scalafmt.svg)](https://gitter.im/olafurpg/scalafmt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Latest version](https://index.scala-lang.org/scalameta/scalafmt/scalafmt-core/latest.svg?color=orange)](https://index.scala-lang.org/scalameta/scalafmt/scalafmt-core)

### [User documentation][docs]
Head over to [the user docs][docs] for instructions on how to install scalafmt.

### Quick help

- `sbt compile` on a clean machine will fail to compile the `scalafmt-intellij` project.
  - if you plan to develop the intellij plugin, run `downloadIdea` first to fetch the IntelliJ SDK (~600mb).
  - or, run `sbt test` or `sbt core/compile` (specific project).
- Run all unit tests: `sbt test`
- Run only formatting tests: `tests/testOnly *FormatTests`.
- Write new formatting test: read [this doc](scalafmt-tests/src/test/resources/readme.md).
- Build docs: See instructions on the [site][docs-guide] or locally at `./docs/contributing-website.md`.
- Hack on IntelliJ plugin: see [this doc](scalafmt-intellij/readme.md).
- Hack on SBT plugin: run `sbt scripted`.
- Run jmh benchmarks: `./bin/run-benchmarks.sh`.
- Run formatter on millions of lines of code: `tests/test:runMain  org.scalafmt.ScalafmtProps` (slow, and requires a lot of memory/cpu)
- Debug performance: after each test run in `FormatTests`, a flamegraph report
  like [this one](https://github.com/scalameta/scalafmt/issues/140)
  is generated in `target/index.html`. 
  I usually keep a browser tab open at `localhost:3000/target/index.html`
  along with this background process:
  `browser-sync start --server --files "target/*.html"`.
  See [Browsersync](https://www.browsersync.io/).
- `intellij/compile:compileIncremental` failed? Run `downloadIdea` to download custom IntelliJ plugin.
  The download may take a while.

### Team
The current maintainers (people who can merge pull requests) are:

* Ólafur Páll Geirsson - [`@olafurpg`](https://github.com/olafurpg)
* Pedro J Rodriguez Tavarez - [`@pjrt`](https://github.com/pjrt)
* Iurii Susuk - [`@ysusuk`](https://github.com/ysusuk)
* Paul Draper - [`@pauldraper`](https://github.com/pauldraper)

An up-to-date list of contributors is available here: https://github.com/scalameta/scalafmt/graphs/contributors

We strive to offer a welcoming environment to learn, teach and contribute.

## Acknowledgements

<a href="http://www.ej-technologies.com/products/jprofiler/overview.html">
  <img src="https://www.ej-technologies.com/images/product_banners/jprofiler_large.png" align="right" />
 </a>

[JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html) generously supports scalafmt with its full-featured Java Profiler.

[docs]: http://scalafmt.org
[docs-guide]: http://scalafmt.org/docs/contributing-website.html

