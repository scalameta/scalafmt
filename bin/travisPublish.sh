#!/usr/bin/env bash
set -e

function setupDeployKey() {
  echo "Setting up deploy key..."
  echo $SCALAFMT_PASSWORD | gpg --passphrase-fd 0 ${DEPLOY_KEY}.gpg
  chmod 600 ${DEPLOY_KEY}
  eval `ssh-agent -s`
  ssh-add ${DEPLOY_KEY}
  git config --global user.name "Travis CI"
  git config --global user.email "$COMMIT_AUTHOR_EMAIL"
  echo "Done!"
}

if [[ ${TRAVIS_SECURE_ENV_VARS} == "true" && ${TRAVIS_BRANCH} == "master" ]]; then
  echo "Publishing snapshot..."
  setupDeployKey
  # only publish if nightly is a snapshot.
  if grep "nightly.*SNAPSHOT" core/target/scala-2.11/src_managed/main/sbt-buildinfo/Versions.scala; then
    echo "Publishing snapshot"
    sbt publish # snapshot
  else
    echo "This is a release, skipping..."
  fi
  ./bin/update-gh-pages.sh
else
  echo "Skipping publish"
fi
