---
id: contributing-scalafmt
title: Contributing
---

## Compiling project

- It's recommended to use Java 8, the project has not been tested with Java 11.
- The project is built with `sbt`, to install sbt see https://www.scala-sbt.org/
- Use `sbt tests/test` instead of `sbt test`, the former runs modestly fast unit
  tests for the JVM while `sbt test` is slower because it runs cross-platform
  test for Scala.js, tests benchmark code and also compiles the documentation.
- You should be able to import the project into IntelliJ as normal:
  `File -> New -> Project from existing source` and pick the `build.sbt` file.

## Opening pull requests

- **All code PRs should come with**: a meaningful description, inline-comments
  for important things, unit tests (positive and negative), and a green build in
  [CI](https://travis-ci.org/scalameta/scalafmt).
- **Format your code with scalafmt**. Run `./scalafmt` from the project root
  directory.
- **PRs for features should generally come with _something_ added to the
  [Documentation](https://scalameta.org/scalafmt)**, so people can discover
  that it exists. The docs are written in `readme/Readme.scalatex`.
- **Be prepared to discuss/argue-for your changes if you want them merged**! You
  will probably need to refactor so your changes fit into the larger codebase -
  **If your code is hard to unit test, and you don't want to unit test it,
  that's ok**. But be prepared to argue why that's the case!
- **It's entirely possible your changes won't be merged**, or will get ripped
  out later. This is also the case for my changes, as the Author!
- **Even a rejected/reverted PR is valuable**! It helps explore the solution
  space, and know what works and what doesn't. For every line in the repo, at
  least three lines were tried, committed, and reverted/refactored, and more
  than 10 were tried without committing.
- **Feel free to send Proof-Of-Concept PRs** that you don't intend to get
  merged.

## Opening issues

Please always include concrete code examples with obtained and expected output.

## Releasing

- Write release notes here: https://github.com/scalameta/scalafmt/releases/new
- Pushing a git tag should trigger a release to Maven Central from the CI.
- Once CI has finished releasing, update sbt-scalafmt to the latest release.
- Update the changelog on the website with the following command (assuming you
  have `docker` installed)
```
# Step 1
PREVIOUS_VERSION=v2.0.0-RC5
docker run -it --rm -v $(pwd):/project markmandel/github-changelog-generator --user scalameta --project scalafmt --since-tag $PREVIOUS_VERSION --no-issues --token 387f5c8b32fffb6614e7a1985d44905abe7258eb

# Step 2
# copy-paste relevant lines from CHANGELOG.md to docs/CHANGELOG.md

# Step 3
# Remove auto-generated `CHANGELOG.md` from step 1.
```

## Random stuff

- Try to keep complex logic out of Router.scala, instead move it into a utility
  function like `OneArgOneLine` or `infixSplit`.

Where do utility functions go?

- `FormatOps`: anything that requires access to fields in `FormatOps` (for
  example `prev/next/owners`), which is custom for each source file.
- `Token/TreeOps`: anything that does not require fields in `FormatOps`,
  agnostic to the source file being formatted.
- `Token/TreeClasses`: groups of trees/tokens that belong together and support
  the `Tree/Token.is[T]` syntax.
