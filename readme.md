# scalafmt

[![Coverage Status](https://coveralls.io/repos/olafurpg/scalafmt/badge.svg?branch=master&service=github)](https://coveralls.io/github/olafurpg/scalafmt?branch=master)
[![Build Status](https://travis-ci.org/olafurpg/scalafmt.svg?branch=master)](https://travis-ci.org/olafurpg/scalafmt)
[![Join the chat at https://gitter.im/olafurpg/scalafmt](https://badges.gitter.im/olafurpg/scalafmt.svg)](https://gitter.im/olafurpg/scalafmt?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Scalafmt is a code formatter for Scala. It wraps your lines when they get too long.

![scalafmt](https://cloud.githubusercontent.com/assets/1408093/12928034/99d3ffe8-cf6b-11e5-9ec5-42de7c4e0155.gif)

### Warning
This project is under active development and is far from production ready.

Main issues:
* Slow for large files (100+ LOC)
* Produces ugly output in some cases.

### Documentation
For now, [the tests](src/test/resources) are the most up-to-date documentation.

### Installation

*NOTE.* Installing is awkward at the moment. It will get easier once the tool matures.

Go to [releases](https://github.com/olafurpg/scalafmt/releases) and download the latest `scalafmt.jar`.

Create a script named `scalafmt` somewhere in your `PATH` with the command

```
java -Droot-level=error -jar </PATH/TO>scalafmt.jar $@
```

Then you can run the formatter on a file like this:

```
scalafmt --file MyCode.scala
```

For more details

```
scalafmt --help
```

#### Vim

* install [vim-autoformat](https://github.com/Chiel92/vim-autoformat)
* add to your `.vimrc`

```vim
noremap <F5> :Autoformat<CR> "" Optional
let g:formatdef_scalafmt = "'scalafmt'"
let g:formatters_scala = ['scalafmt']
```
* run `:Autoformat` or `F5`

#### IntelliJ

*NOTE.* Does not support undo after formatting :(

* Open settings, create a `scalafmt` "external tool"

<img width="519" alt="scalafmt-intellij1" src="https://cloud.githubusercontent.com/assets/1408093/12949316/6963ffce-d007-11e5-847e-65956d5cc781.png">
<img width="485" alt="scalafmt-intellij2" src="https://cloud.githubusercontent.com/assets/1408093/12949336/854a029c-d007-11e5-8856-743a0d76861e.png">
* Assign a shortcut to `scalafmt`

<img width="513" alt="scalafmt-intellij3" src="https://cloud.githubusercontent.com/assets/1408093/12949347/9c07dda6-d007-11e5-96d4-8cd53394a52c.png">


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

