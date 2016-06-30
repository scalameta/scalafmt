#!/usr/bin/env bash
# Unfortunately, I can't seem to be able to use the sbt-scalafmt plugin for this :(
scalafmt-nightly -i -f core/src/main/scala
scalafmt-nightly -i -f core/src/test/scala
scalafmt-nightly -i -f benchmarks/src/main/scala
scalafmt-nightly -i -f benchmarks/src/test/scala
scalafmt-nightly -i -f cli/src/main/scala
scalafmt-nightly -i -f cli/src/test/scala
scalafmt-nightly -i -f scalafmtSbt/src/main/scala
scalafmt-nightly -i -f scalafmtSbt/src/test/scala
scalafmt-nightly -i -f intellij/src
scalafmt-nightly -i -f readme
