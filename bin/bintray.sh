#!/usr/bin/env bash
set -eu
PUBLISH=${CI_PUBLISH:-false}
echo $PUBLISH

if [[ "$DRONE_BRANCH" == "master" && "$PUBLISH" == "true" ]]; then
  echo "Running publish from $(pwd)"
  git log | head -n 20
  mkdir -p $HOME/.bintray
  cat > $HOME/.bintray/.credentials <<EOF
realm = Bintray API Realm
host = api.bintray.com
user = ${BINTRAY_USERNAME}
password = ${BINTRAY_API_KEY}
EOF
  /usr/bin/sbt ci-publish
  ./bin/update-gh-pages.sh
else
  echo "Skipping publish, branch=$DRONE_BRANCH test=$CI_TEST"
fi


