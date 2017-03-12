#!/usr/bin/env bash
# sbt cli/assembly
# I can't reproduce this error in sbt, see https://github.com/scalameta/scalafmt/issues/492
set -e
echo "Issue 492"
in_file=bin/issue492.scala
sbt cli/assembly
java -jar cli/target/scala-2.11/scalafmt.jar -i -f $in_file

