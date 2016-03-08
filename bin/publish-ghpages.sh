#!/bin/bash
git checkout gh-pages
git reset --hard master
sbt readme/run --validate
cp -r readme/target/scalatex/* .
git add .
git commit -m "Update ghpages."
git push -f origin gh-pages
git checkout master
