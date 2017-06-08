#!/usr/bin/env bash
set -eu
PUBLISH=${CI_PUBLISH:-false}

set-up-ssh() {
  echo "Setting up ssh..."
  mkdir -p $HOME/.ssh
  ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts
  git config --global user.email "olafurpg@gmail.com"
  git config --global user.name "Travis CI bot"
  git config --global push.default simple
  DEPLOY_KEY_FILE=$HOME/.ssh/id_rsa
  echo "$GITHUB_DEPLOY_KEY" | base64 --decode > ${DEPLOY_KEY_FILE}
  chmod 600 ${DEPLOY_KEY_FILE}
  eval "$(ssh-agent -s)"
  ssh-add ${DEPLOY_KEY_FILE}
}

if [[ "$TRAVIS_BRANCH" == "master" && "$TRAVIS_PULL_REQUEST" = "false" && "$PUBLISH" == "true" ]]; then
  git log | head -n 20
  echo "Running publish from $(pwd)"
  set-up-ssh
  mkdir -p $HOME/.bintray
  cat > $HOME/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = ${BINTRAY_USERNAME}
password = ${BINTRAY_API_KEY}
EOF
  sbt ci-publish
else
  echo "Skipping publish, branch=$TRAVIS_BRANCH publish=$PUBLISH test=$CI_TEST"
fi


