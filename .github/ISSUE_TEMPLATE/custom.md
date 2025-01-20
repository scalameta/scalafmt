This template is a strict requirement when it comes to the sections marked `(required)`.

## Required: Do not submit if... ##

This repository is not the appropriate place for this submission if:

- you're running into parsing errors, that is, anything to do with the source code
  - issues regarding parsing should be submitted to [Scalameta](http://github.com/scalameta/scalameta) 
  - this tool is only about formatting, i.e., what happens on the output
- you're running this using `sbt`, `Intellij` or any other integration
  - Please make sure that the issue is observable using the latest version of `scalafmt` and
    within the `scalafmt` **CLI**: `https://scalameta.org/scalafmt/docs/installation.html#cli`
  - Using the CLI is _mandatory_, in one of these configurations:
    - if the problem is not with dynamic version loading:
      - latest version of CLI and latest version in `.scalafmt.conf`
    - if the problem is with dynamic loading, then both:
      - previous version of CLI and latest version in `.scalafmt.conf`
      - latest version of CLI and previous version in `.scalafmt.conf`

## Required: Configuration ##

Please paste the **smallest** possible set of `.scalafmt.conf`
configuration parameters that reproduces the problem:
```
version = <please enter the version here and make sure it's the latest>
...
```

NB: before submitting, please confirm that the problem is observed in the
*latest published* version of the formatter! We don't publish hotfixes for older
versions, and the problem you have observed in an older version may have already
been fixed.

## Required: Command-line parameters ##

When I run scalafmt via CLI like this: `<command-line parameter>`

If you were not using CLI initially:
- please check using `scalafmt` CLI before submitting here (required)
- if the problem does not present via CLI, DO NOT SUBMIT
  here: this is NOT a formatter issue
  - it could be an issue with your integration (sbt-scalafmt, metals, intellij etc.)
  - instead please submit the issue in the appropriate tool's repository
  - for instance, if the tool is `sbt` with the `sbt-scalafmt` plugin: please
    submit in `sbt-scalafmt` and include scalafmt parameters from `build.sbt`
    and the `sbt` command used
  - conversely, do not submit formatting issues into other tools; you must first
    verify using `scalafmt` CLI

## Steps

Given code like this:
```scala
THE SMALLEST POSSIBLE SNIPPET OF ORIGINAL CODE
```

## Problem

Scalafmt formats code like this:
```scala
OUTPUT FROM SCALAFMT
```

## Expectation

I would like the formatted output to look like this:
```scala
EXPECTED FORMATTING OUTPUT
```

## Workaround

I've found that by...

## Notes

See also...
