#!/bin/bash
set -e

tag=$(sed -n -e 's/.*val scalafmt = "\(.*\)"/\1/p' core/src/main/scala/org/scalafmt/Versions.scala)
version="v${tag}"

read -p "Release ${version}? (y/n): " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

# Run tests, create jar.
sbt assembly

# Git tag.
git tag -a ${version}
git push --tags

# maven
sbt publishSigned sonatypeRelease

# gh-pages
git checkout gh-pages
git reset --hard master
sbt readme/run --validate
cp -r readme/target/scalatex/* .
git add .
git commit -m "Update ghpages."
git push -f origin gh-pages
git checkout master

# Github release.
github-release release \
    --user olafurpg \
    --repo scalafmt \
    --tag ${version} \
    --name "New release." \
    --description "See changelog in docs." \
    --file cli/target/scalafmt.tar.gz

# TODO(olafur) Homebrew release.
# TODO(olafur) IntelliJ release.
