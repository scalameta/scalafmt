This template is mostly a guideline, not a strict requirement.

Strict requirements: 
- explicitly using the latest version of `scalafmt`
- using the `scalafmt` CLI: `https://scalameta.org/scalafmt/docs/installation.html#cli`

## Configuration (required) ##
```
version = <please enter the version here and make sure it's the latest>
...
```

NB: before submitting, please confirm that the problem is observed in the
*latest published* version of the formatter! We don't publish hotfixes for older
versions, and the problem you have observed in an older version may have already
been fixed.

## Command-line parameters (required) ##

When I run scalafmt via CLI like this: `<command-line parameter>`

If you were not using CLI initially:
- please check using `scalafmt` CLI before submitting here
- if the problem does not present via CLI, instead please submit the issue in
  the appropriate tool's repository
  - for instance, if the tool is `sbt` with the `sbt-scalafmt` plugin: please
    submit in `sbt-scalafmt` and include scalafmt parameters from `build.sbt`
    and the `sbt` command used

## Steps

Given code like this:
```scala
ORIGINAL CODE
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
