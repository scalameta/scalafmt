#!/bin/bash
# Saves the commit has for each clones repo in target/repos
set -e

for repo in target/repos/*; do
  echo $repo ...
  git --git-dir="$repo/.git" curr > $repo/COMMIT
  git --git-dir="$repo/.git" config --get remote.origin.url > $repo/URL
done
