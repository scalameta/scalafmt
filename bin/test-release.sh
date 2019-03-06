#!/usr/bin/env bash
set -eux

version=$1

coursier fetch org.scalameta:scalafmt-dynamic_2.12:$version -r sonatype:public
coursier fetch \
    "org.scalameta:sbt-scalafmt;sbtVersion=1.0;scalaVersion=2.12:$version" \
    --sbt-plugin-hack -r sonatype:public
