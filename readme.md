# scalafmt

[![Coverage Status](https://coveralls.io/repos/olafurpg/scalafmt/badge.svg?branch=master&service=github)](https://coveralls.io/github/olafurpg/scalafmt?branch=master)
[![Build Status](https://travis-ci.org/olafurpg/scalafmt.svg?branch=master)](https://travis-ci.org/olafurpg/scalafmt)
[![Join the chat at https://gitter.im/olafurpg/scalafmt](https://badges.gitter.im/olafurpg/scalafmt.svg)](https://gitter.im/olafurpg/scalafmt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Codacy Badge](https://api.codacy.com/project/badge/grade/7461ebc94f9a4db6ac91befb5bc98e4c)](https://www.codacy.com/app/olafurpg/scalafmt)

Scalafmt is a code formatter for Scala that can wrap your lines if they get too long.


### Warning
This project is under active development and is not ready for production use.

### Documentation

For now, [the tests](src/test/resources) are the most up-to-date documentation.


### Updates

* Feb 9rd
   * pre-release https://github.com/olafurpg/scalafmt/releases/tag/v0.1.0
* Feb 3rd
    * [Sneak peek output from the
    formatter](https://htmlpreview.github.io/?https://github.com/olafurpg/scalafmt/blob/htmlpreview/reports/scalafmt-feb3.html).
    The red regions are spots where the formatter's performance still suffers.
    * The actual implementation, under `src/main/scala/`, is 684 LOC, excluding blank lines and comments and including some debugging stuff.
    * Running time for a ~500 LOC file is around 5 seconds. Will hopefully
    reach <100ms soon. Last week the formatter choked on 10 LOC (exponential
    growth, yay).
    * There is one style to begin with, adapted from the [Scala.js coding
    style](https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md).
    Support for
    configuration flags comes later.
    * The formatter uses [scala.meta's](https://github.com/scalameta/scalameta)
    tokens and AST. The implementation works mostly on the token level, but
    relies heavily on the AST for "smarter" features.
    * Still haven't covered some common cases (like breaking before `.` in
    `a.b(c)`), so the output looks weird in some cases.

