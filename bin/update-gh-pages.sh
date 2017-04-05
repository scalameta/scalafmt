#!/usr/bin/env bash
set -eu

echo "Updating gh-pages..."

SUBDIR="gh-pages"
SOURCE_BRANCH="master"
TARGET_BRANCH="gh-pages"
AUTH=${GITHUB_AUTH:-}
SETUP_GIT=${DRONE:-false}

git checkout master
REPO=`git config remote.origin.url`
SSH_REPO=${REPO/https:\/\/github.com\//git@github.com:}
HTTP_REPO=${REPO/github.com/${AUTH}github.com}
SHA=`git rev-parse --verify HEAD`
sbt "readme/run --validate-links"

rm -rf ${SUBDIR}
git clone ${REPO} ${SUBDIR}
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

if [[ ${SETUP_GIT} == "true" ]]; then
  git config user.name "scalametabot"
  git config user.email "scalametabot@gmail.com"
fi

git add .
git commit -m "Deploy to GitHub Pages: ${SHA}"

git push -f $HTTP_REPO gh-pages
git checkout master
cd ..
rm -rf gh-pages

echo "Done!"

