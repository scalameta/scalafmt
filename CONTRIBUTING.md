## Opening pull requests

Shamelessly stolen from [lihaoyi/ammonite](https://github.com/lihaoyi/Ammonite).

- **All code PRs should come with**: a meaningful description, inline-comments
  for important things, unit tests (positive and negative), and a green build
  in [CI](https://travis-ci.org/olafurpg/scalafmt).
- **Format your code with the lastest release of scalafmt, default style.**.
- **PRs for features should generally come with *something* added to the
  [Documentation](https://olafurpg.github.io/scalafmt)**, so people can discover
  that it exists. The docs are written in `readme/Readme.scalatex`.
- **Be prepared to discuss/argue-for your changes if you want them merged**!
  You will probably need to refactor so your changes fit into the larger
  codebase - **If your code is hard to unit test, and you don't want to unit
  test it, that's ok**. But be prepared to argue why that's the case!
- **It's entirely possible your changes won't be merged**, or will get ripped
  out later. This is also the case for my changes, as the Author!
- **Even a rejected/reverted PR is valuable**! It helps explore the solution
  space, and know what works and what doesn't. For every line in the repo, at
  least three lines were tried, committed, and reverted/refactored, and more
  than 10 were tried without committing.
- **Feel free to send Proof-Of-Concept PRs** that you don't intend to get merged.

## Opening issues

* Examples with diffs are better than words
* Always include your expected behavior
* Make it reproducible
* No complaining

