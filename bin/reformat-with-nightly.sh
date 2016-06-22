#!/usr/bin/env bash
# Unfortunately, I can't seem to be able to use the sbt-scalafmt plugin for this :(
scalafmt -i -f core/src/main/scala
scalafmt -i -f core/src/test/scala
scalafmt -i -f benchmarks/src/main/scala
scalafmt -i -f benchmarks/src/test/scala
scalafmt -i -f cli/src/main/scala
scalafmt -i -f cli/src/test/scala
scalafmt -i -f scalafmtSbt/src/main/scala
scalafmt -i -f scalafmtSbt/src/test/scala
scalafmt -i -f intellij/src
scalafmt -i -f readme
