#!/bin/bash
set -e

echo "Updating gh-pages..."

SUBDIR="gh-pages"
SOURCE_BRANCH="master"
TARGET_BRANCH="gh-pages"

git checkout master
REPO=`git config remote.origin.url`
SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
SHA=`git rev-parse --verify HEAD`
sbt "readme/run --validate-links"

rm -rf ${SUBDIR}
git clone ${SSH_REPO} ${SUBDIR}
cd ${SUBDIR}
git checkout ${TARGET_BRANCH} || git checkout --orphan ${TARGET_BRANCH}
cd ..
cp -r readme/target/scalatex/* ${SUBDIR}

cd ${SUBDIR}
# If there are no changes to the compiled out (e.g. this is a README update) then just bail.
if [[ -z `git diff --exit-code` ]]; then
    echo "No changes to the output on this push; exiting."
    exit 0
fi

git add .
git commit --no-verify  -m "Deploy to GitHub Pages: ${SHA}"

if [[ ${TRAVIS} == "true" ]]; then
  git config user.name "Travis CI"
  git config user.email "olafurpg@gmail.com"
fi

git push --no-verify -f origin gh-pages
git checkout master
rm -rf gh-pages

echo "Done!"
