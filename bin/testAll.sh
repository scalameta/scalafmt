#!/bin/bash
set -e

sbt clean intellij/updateIdea test
sbt "core/test:runMain org.scalafmt.FormatExperimentApp"
sbt "; publishLocal ; scripted"

