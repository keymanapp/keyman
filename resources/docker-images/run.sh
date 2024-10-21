#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/resources/build/minimum-versions.inc.sh"

################################ Main script ################################

builder_describe \
  "Run build.sh script inside of a docker image. Pass the build script and parameters after --." \
  "android" \
  "core" \
  "linux" \
  "web" \
  "--ubuntu-version=UBUNTU_VERSION  The Ubuntu version (default: ${KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER})"

builder_parse "$@"

run_android() {
  docker run -it --rm -v ${KEYMAN_ROOT}:/home/build/build \
    -v ${KEYMAN_ROOT}/core/build/docker-core:/home/build/build/core/build \
    keymanapp/keyman-android-ci:default \
    "${builder_extra_params[@]}"
}

run_core() {
  docker run -it --rm -v ${KEYMAN_ROOT}:/home/build/build \
    -v ${KEYMAN_ROOT}/core/build/docker-core:/home/build/build/core/build \
    keymanapp/keyman-core-ci:default \
    "${builder_extra_params[@]}"
}

run_linux() {
  mkdir -p ${KEYMAN_ROOT}/linux/build/docker-linux
  docker run -it --privileged --rm -v ${KEYMAN_ROOT}:/home/build/build \
    -v ${KEYMAN_ROOT}/core/build/docker-core:/home/build/build/core/build \
    -v ${KEYMAN_ROOT}/linux/build/docker-linux:/home/build/build/linux/build \
    -e DESTDIR=/tmp \
    keymanapp/keyman-linux-ci:default \
    "${builder_extra_params[@]}"
}

run_web() {
  docker run -it --privileged --rm -v ${KEYMAN_ROOT}:/home/build/build \
    -v ${KEYMAN_ROOT}/core/build/docker-core:/home/build/build/core/build \
    keymanapp/keyman-web-ci:default \
    "${builder_extra_params[@]}"
}

mkdir -p ${KEYMAN_ROOT}/core/build/docker-core

builder_run_action android  run_android
builder_run_action core     run_core
builder_run_action linux    run_linux
builder_run_action web      run_web
