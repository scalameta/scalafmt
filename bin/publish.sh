#!/bin/bash
set -e

version=$(sed -n -e 's/.*val scalafmt = "\(.*\)"/\1/p' core/src/main/scala/org/scalafmt/Versions.scala)
tag="v${version}"

read -p "Release ${tag}? (y/n): " -n 1 -r
echo
if [[ ! $REPLY =~ ^[Yy]$ ]]
then
    exit 1
fi

# Run tests, create jar.
sbt assembly

# Git tag.
git tag -a ${tag}
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
tarfile="cli/target/scalafmt.tar.gz"
rm -f ${tarfile}
tar -cvzf ${tarfile} bin/scalafmt cli/target/scala-2.11/scalafmt.jar bin/configure

echo "Creating github release..."
github-release release \
    --user olafurpg \
    --repo scalafmt \
    --tag ${tag} \
    --name "New release." \
    --description "See changelog in user docs: http://scalafmt.org"

echo "Uploading tar..."
github-release upload \
    --user olafurpg \
    --repo scalafmt \
    --tag ${tag} \
    --name "scalafmt.tar.gz" \
    --file ${tarfile}

# TODO(olafur) Homebrew release.
# TODO(olafur) Intellij, the following unfortunately doesn't work.
# sed -i '' -e "s/\(com.geirsson:scalafmt_2.11:\)[^\"]*\"/\1${version}\"/" intellij/scalafmt-intellij.iml
# cd intellij
# ant
# cd ..
