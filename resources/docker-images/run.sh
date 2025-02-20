#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "${KEYMAN_ROOT}/resources/build/minimum-versions.inc.sh"
. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

################################ Main script ################################

builder_describe \
  "Run build.sh script inside of a docker image. Pass the build script and parameters after --." \
  "android" \
  "core" \
  "linux" \
  "web" \
  "--distro=DISTRO                  The distribution (debian or ubuntu, default: ubuntu)" \
  "--distro-version=DISTRO_VERSION  The Ubuntu/Debian version (default: ${KEYMAN_DEFAULT_VERSION_UBUNTU_CONTAINER})"

builder_parse "$@"

run_android() {
  docker run -it --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    "keymanapp/keyman-android-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

run_core() {
  docker run -it --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    "keymanapp/keyman-core-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

run_linux() {
  mkdir -p "${KEYMAN_ROOT}/linux/build/docker-linux/${build_dir}"
  mkdir -p "${KEYMAN_ROOT}/linux/keyman-system-service/build/docker-linux/${build_dir}"
  docker run -it --privileged --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    -v "${KEYMAN_ROOT}/linux/build/docker-linux/${build_dir}":/home/build/build/linux/build \
    -v "${KEYMAN_ROOT}/linux/keyman-system-service/build/docker-linux/${build_dir}":/home/build/build/linux/keyman-system-service/build \
    -e DESTDIR=/tmp \
    "keymanapp/keyman-linux-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

run_web() {
  docker run -it --privileged --rm -v "${KEYMAN_ROOT}":/home/build/build \
    -v "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}":/home/build/build/core/build \
    "keymanapp/keyman-web-ci:${image_version}" \
    "${builder_extra_params[@]}"
}

if [[ -z "${DISTRO_VERSION:-}" ]]; then
  image_version=default
  build_dir=default
else
  image_version="${DISTRO:-}-${DISTRO_VERSION}-java${KEYMAN_VERSION_JAVA}-node$(_print_expected_node_version)-emsdk${KEYMAN_MIN_VERSION_EMSCRIPTEN}"
  build_dir="${DISTRO:-}-${DISTRO_VERSION}"
fi

mkdir -p "${KEYMAN_ROOT}/core/build/docker-core/${build_dir}"

builder_run_action android  run_android
builder_run_action core     run_core
builder_run_action linux    run_linux
builder_run_action web      run_web
