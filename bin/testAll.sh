#!/bin/bash
set -e

#sbt clean coverage test
sbt clean test
sbt "core/test:runMain org.scalafmt.FormatExperimentApp"
./bin/issue492.sh
#sbt coverageAggregate

