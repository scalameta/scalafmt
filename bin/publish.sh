#!/bin/bash
set -eux

version=$1
test -n $version
tag="v${version}"
current_branch=$(git rev-parse --abbrev-ref HEAD)

function assert-installed() {
  binary=$1
  command -v ${binary} >/dev/null 2>&1 || { echo >&2 "Missing dependency ${binary}, exiting."; exit 1; }
}

function assert-dependencies-are-installed() {
  assert-installed sbt
}

function assert-preconditions() {
    if [[ "$current_branch" != "master" && "$current_branch" != "HEAD" ]]; then
      echo "On branch $current_branch! You should be on master branch."
      exit 1
    fi
    assert-dependencies-are-installed
}

function confirm-release() {
    read -p "Release ${tag}? (y/n): " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]
    then
        exit 1
    fi
}

function tag-create() {
    git tag ${tag} -m "See changelog."
}
function tag-push() {
    git push --tags
}

function maven-publish() {
    sbt -Dpublish.sonatype=true clean "+ publishSigned" sonatypeRelease
}


assert-preconditions
if [[ ! $1 == "-q" ]]; then
  confirm-release
fi
tag-create
maven-publish
tag-push
echo "Released ${tag}!"
