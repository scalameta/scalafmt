# scalafmt [![codecov.io](https://codecov.io/github/olafurpg/scalafmt/coverage.svg?branch=master)](https://codecov.io/github/olafurpg/scalafmt?branch=master) [![Build Status](https://travis-ci.org/olafurpg/scalafmt.svg?branch=master)](https://travis-ci.org/olafurpg/scalafmt) [![Join the chat at https://gitter.im/olafurpg/scalafmt](https://badges.gitter.im/olafurpg/scalafmt.svg)](https://gitter.im/olafurpg/scalafmt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

### [User documentation][docs]
Head over to [the user docs][docs] for instructions on how to install scalafmt.

### Developer documentation - contributing

* `core/testOnly org.scalafmt.FormatTests`: runs the fast, most relevant, unit tests.
    * After each run, a performance report is generated in `target/index.html`. 
    I usually keep a browser tab open at `localhost:3000/target/index.html`
    along with this background process:
    `browser-sync start --server --files "target/*.html"`.
    See [Browsersync](https://www.browsersync.io/).
* `core/testOnly org.scalafmt.FidelityTest`: runs the formatter on all files in
  this project.
* `run-benchmarks.sh` script to run jmh benchmarks.
* `core/test:runMain  org.scalafmt.FormatExperimentApp`:
  1. Uses `wget` to download a ~20mb tar
     ([repos.tar.gz](https://github.com/olafurpg/scalafmt/releases/tag/v0.1.4))
     that contains ~28.000 Scala source files from public Github repos,
  2. untars with `tar`,
  3. runs scalafmt on a subset of the files, specified in `FormatExperiment`.
     Runs various property based checks under timing constraints (around 10s per file),
  4. prints summary report to console.
* instructions for the tests [are here](core/src/test/resources).

[docs]: http://scalafmt.org
