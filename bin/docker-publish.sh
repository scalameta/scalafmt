#!/usr/bin/env bash

set -ex

# CD over to docker/
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
DOCKERDIR="$(dirname "$DIR")/docker"
cd "$DOCKERDIR" || exit 1

# Extract version, build and push
SCALAFMT_VERSION=$(git describe --tags | sed 's/^v//')
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
docker build \
    --no-cache \
    --build-arg SCALAFMT_VERSION="$SCALAFMT_VERSION" \
    --progress=plain \
    --tag "scalameta/$DOCKER_USERNAME:$SCALAFMT_VERSION" \
    --tag "scalameta/$DOCKER_USERNAME:latest" \
    .
docker push "scalameta/$DOCKER_USERNAME:$SCALAFMT_VERSION"
# Don't tag releaes candidate (RC) versions with 'latest'
case "$SCALAFMT_VERSION" in
    *RC*)
        ;;
    *)
        docker push "scalameta/$DOCKER_USERNAME:latest"
        ;;
esac
