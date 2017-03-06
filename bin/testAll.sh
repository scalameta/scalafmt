#!/bin/bash
set -e
./scalafmt --test
./sbt clean "very test"
./sbt "core/test:runMain org.scalafmt.FormatExperimentApp"
./sbt "; publishLocal ; scripted"

