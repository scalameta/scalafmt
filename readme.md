# scalafmt
[![Build Status](https://platform-ci.scala-lang.org/api/badges/scalameta/scalafmt/status.svg)](https://platform-ci.scala-lang.org/scalameta/scalafmt)
[![Build status](https://ci.appveyor.com/api/projects/status/ek7iwlgmbw0c5n3d?svg=true)](https://ci.appveyor.com/project/olafurpg/scalafmt-wocrd)
[![Join the chat at https://gitter.im/olafurpg/scalafmt](https://badges.gitter.im/olafurpg/scalafmt.svg)](https://gitter.im/olafurpg/scalafmt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Latest version](https://index.scala-lang.org/olafurpg/scalafmt/scalafmt-core/latest.svg?color=orange)](https://index.scala-lang.org/olafurpg/scalafmt/scalafmt-core)

### [User documentation][docs]
Head over to [the user docs][docs] for instructions on how to install scalafmt.

### Quick help

- Run all unit tests: `sbt test`
- Run only formatting tests: `core/testOnly org.scalafmt.FormatTests`.
- Write new formatting test: read [this doc](core/src/test/resources/readme.md).
- Build docs: `sbt readme/run` will create the docs, which you can open with
  `open readme/target/scalatex/index.html`. Docs are built with [Scalatex](http://www.lihaoyi.com/Scalatex/).
- Hack on IntelliJ plugin: see [this doc](intellij/readme.md).
- Hack on scalafmt: see [tutorial](Tutorial.md).
- Hack on SBT plugin: run `sbt scripted`.
- Run jmh benchmarks: `./bin/run-benchmarks.sh`.
- Run formatter on millions of lines of code: `core/test:runMain  org.scalafmt.FormatExperimentApp`:
- Debug performance: after each test run in `FormatTests`, a flamegraph report
  like [this one](https://github.com/scalameta/scalafmt/issues/140)
  is generated in `target/index.html`. 
  I usually keep a browser tab open at `localhost:3000/target/index.html`
  along with this background process:
  `browser-sync start --server --files "target/*.html"`.
  See [Browsersync](https://www.browsersync.io/).
- `intellij/compile:compileIncremental` failed? Run `downloadIdea` to download custom IntelliJ plugin.
  The download may take a while.

### Tutorial
If you'd like to find out how to work on scalafmt, see this [tutorial](Tutorial.md).

### Team
The current maintainers (people who can merge pull requests) are:

* Ólafur Páll Geirsson - [`@olafurpg`](https://github.com/olafurpg)
* Pedro J Rodriguez Tavarez - [`@pjrt`](https://github.com/pjrt)
* Iurii Susuk - [`@ysusuk`](https://github.com/ysusuk)

An up-to-date list of contributors is available here: https://github.com/scalameta/scalafmt/graphs/contributors

We strive to offer a welcoming environment to learn, teach and contribute.

## Acknowledgements

<a href="http://www.ej-technologies.com/products/jprofiler/overview.html">
  <img src="https://www.ej-technologies.com/images/product_banners/jprofiler_large.png" align="right" />
 </a>
[JProfiler](http://www.ej-technologies.com/products/jprofiler/overview.html) generously supports scalafmt with its full-featured Java Profiler.

[docs]: http://scalafmt.org

