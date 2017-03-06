#!/bin/bash
set -e
./sbt clean "very test"
./sbt "core/test:runMain org.scalafmt.FormatExperimentApp"
./sbt "; publishLocal ; scripted"

