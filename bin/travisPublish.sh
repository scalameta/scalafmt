#!/usr/bin/env bash
set -e

function setupDeployKey() {
  echo "Setting up deploy key..."
  echo $SCALAFMT_PASSWORD | gpg --passphrase-fd 0 ${DEPLOY_KEY}.gpg
  chmod 600 ${DEPLOY_KEY}
  eval `ssh-agent -s`
  ssh-add ${DEPLOY_KEY}
  git config user.name "Travis CI"
  git config user.email "$COMMIT_AUTHOR_EMAIL"
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
