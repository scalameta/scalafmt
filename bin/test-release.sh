#!/usr/bin/env bash
set -eux

version=$1

coursier resolve \
  org.scalameta:scalafmt-cli_2.13:$version \
  org.scalameta:scalafmt-cli_2.12:$version \
  org.scalameta:scalafmt-cli_2.11:$version
