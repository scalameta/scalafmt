---
id: contributing-scalafmt
title: Contributing
---

## Compiling project

- The project is built with `sbt`, to install sbt see https://www.scala-sbt.org/
- Use `sbt tests/test` instead of `sbt test`, the former runs modestly fast unit
  tests for the JVM while `sbt test` is slower because it runs cross-platform
  test for Scala.js, tests benchmark code and also compiles the documentation.
- You should be able to import the project into IntelliJ as normal:
  `File -> New -> Project from existing source` and pick the `build.sbt` file.
- sbt builds each project of this build once per Scala version and platform.
  Several of those rows use the same source directories. Two system properties
  control which rows an IDE imports: the build sets `bspEnabled := false` on
  the other rows, and sbt then leaves them out of the BSP workspace.
  - `-Dide.scala=X` — sbt keeps only the rows for Scala version `X`.
    - matches full or binary version
    - if unspecified or empty: keep all scala versions
      - IntelliJ only: will be forced to `2.13`; see below why IntelliJ can't
        load multiple versions.
  - `-Dide.platform=Y` — sbt keeps only the rows for the platforms in `Y`, a
    comma-separated list.
    - matches `jvm`, `js`, or `native`
    - if unspecified: keep every platform
    - `-Dide.platform=`, with nothing after it, keeps every platform
- IntelliJ cannot import the whole matrix. It puts the sources that several
  rows use into one module, and then compiles the Scala 2 and the Scala 3
  sources of a project together. It starts sbt with `-Didea.managed=true`. If
  you do not set `-Dide.scala`, that property selects 2.13. To choose another
  version, add `-Dide.scala=X` under `Settings -> Build, Execution, Deployment -> Build Tools -> sbt -> VM parameters`, then reload the sbt project.
- These properties change what an IDE imports over BSP. A command-line `sbt` is
  not a BSP client, so it still sees every row and builds and tests them all.
- An sbt server runs with the system properties from its own command line. A
  later `sbt` in the same directory attaches to that server, so a property you
  pass then changes nothing. Run `sbt shutdown` before you test a change to
  these properties.

## Testing

Take a look at https://github.com/scalameta/scalafmt/blob/master/scalafmt-tests/shared/src/test/resources/readme.md to see how to test your changes.

## Opening pull requests

- **All code PRs should come with**: a meaningful description, inline-comments
  for important things, unit tests (positive and negative), and a green build in
  [CI](https://travis-ci.org/scalameta/scalafmt).
- **Format your code with scalafmt**. Run `./scalafmt` from the project root
  directory.
- **Format documentation with Prettier**. Run `yarn install && yarn format`
  from the project root directory.
- **PRs for features should generally come with _something_ added to the
  [Documentation](https://scalameta.org/scalafmt)**, so people can discover that
  it exists. The docs are written in `docs/configuration.md`.
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
- Attach GraalVM Native Images to GitHub release
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

## Building a native image

To build a native image of the command-line interface using
[GraalVM native-image](https://www.graalvm.org/docs/reference-manual/native-image/):

- Make sure that your installation of `java` is GraalVM v19.3 or later (earlier
  versions are not supported)
- Run `gu install native-image`
- Run the following command: `sbt native-image`
- The generated binary lives here: `scalafmt-cli/graalvm-native-image/cli`

## Assembling new CLI artifacts

- From the project root directory,
  - run `sbt cli/assembly`
  - run `java -jar scalafmt-cli/jvm/target/scala-2.13/scalafmt.jar`, to execute recently built artifacts

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
