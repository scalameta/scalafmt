#!/bin/bash
set -e

sbt clean test
sbt "core/test:runMain org.scalafmt.FormatExperimentApp"
sbt "; intellij/updateIdea ; publishLocal ; scripted"

