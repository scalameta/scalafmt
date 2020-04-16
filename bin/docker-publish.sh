#!/usr/bin/env bash

# TODO: we could also possibly use
# https://github.com/marketplace/actions/build-and-push-docker-images
# in place of this.

set -ex

if [[ -z "$DOCKER_USERNAME" ]]; then
    echo "Error: Missing DOCKER_USERNAME" >&2
    exit 1
fi

if [[ -z "$DOCKER_PASSWORD" ]]; then
    echo "Error: Missing DOCKER_PASSWORD" >&2
    exit 1
fi

# CD over to docker/
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
DOCKERDIR="$(dirname "$DIR")/docker"
cd "$DOCKERDIR" || exit 1

# Extract version, build and push
SCALAFMT_VERSION=$(git describe --tags | sed 's/^v//')
echo "$DOCKER_PASSWORD" | docker login -u "$DOCKER_USERNAME" --password-stdin
docker image build \
    --no-cache \
    --build-arg SCALAFMT_VERSION="$SCALAFMT_VERSION" \
    --progress=plain \
    --tag "scalameta/$DOCKER_USERNAME:$SCALAFMT_VERSION" \
    --tag "scalameta/$DOCKER_USERNAME:latest" \
    .
docker image ls --format 'table {{.Repository}}\t{{.Tag}}\t{{.ID}}\t{{.Size}}'
# docker image push "scalameta/$DOCKER_USERNAME:$SCALAFMT_VERSION"
# Don't tag release candidate (RC) versions with 'latest'
case "$SCALAFMT_VERSION" in
    *RC*)
        ;;
    *)
        echo "PUSHING LATEST"
        # docker image push "scalameta/$DOCKER_USERNAME:latest"
        ;;
esac
