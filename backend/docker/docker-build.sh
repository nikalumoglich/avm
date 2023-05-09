#!/bin/sh

remove_image() {
  echo "Removing image $1..."
  docker images -q "$1" | uniq | xargs -r docker rmi -f
}

echo "Removing old images"

set -e
remove_image avm-backend
set +e

echo "Building new images"

set -ex
docker build ./ -f ./docker/Dockerfile -t avm-backend:1.0
