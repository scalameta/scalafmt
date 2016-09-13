#!/usr/bin/env bash
set -e

DEPLOY_KEY="deploy_key"

function setupDeployKey() {
  echo "Setting up deploy key..."
  # openssl aes-256-cbc -K $encrypted_11bd8d7de852_key -iv $encrypted_11bd8d7de852_iv -in deploy_key.enc -out deploy_key -d
  # openssl aes-256-cbc -K $encrypted_11bd8d7de852_key -iv $encrypted_11bd8d7de852_iv -in secring.gpg.enc -out secring.gpg -d
  chmod 600 ${DEPLOY_KEY}
  eval `ssh-agent -s`
  ssh-add ${DEPLOY_KEY}
  echo "Done!"
}

if [[ ${TRAVIS_SECURE_ENV_VARS} == "true" && ${TRAVIS_BRANCH} == "master" ]]; then
  echo "Publishing snapshot..."
  setupDeployKey
  if [[ -n ${TRAVIS_TAG} ]]; then
    echo "Tag ${TRAVIS_TAG} got pushed, skipping.."
  else
    # Assert that nightly is set to snapshot.
    grep "nightly.*SNAPSHOT" core/src/main/scala/org/scalafmt/Versions.scala
    # Save some useful information
    sbt publish
  fi
  ./bin/update-gh-pages.sh
else
  echo "Skipping publish"
fi
