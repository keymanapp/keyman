#!/bin/bash

# Build dist tarballs or Debian orig tarballs
# and put them in dist/

# parameters: ./dist.sh [origdist] [proj]
# origdist = create Debian orig.tar.xz
# proj = only make tarball for this project

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

# shellcheck disable=SC2154
. "${KEYMAN_ROOT}/linux/scripts/package-build.inc.sh"

BASEDIR=$(pwd)

cd "${KEYMAN_ROOT}/linux"

if [[ ! -z ${1+x} ]] && [[ "$1" == "origdist" ]]; then
    create_origdist=1
    shift
fi

rm -rf dist
mkdir -p dist

# dist for keyman
cp -a debian ../
cd "${KEYMAN_ROOT}"
echo "3.0 (native)" > debian/source/format
# shellcheck disable=SC2154
dch keyman --newversion "${KEYMAN_VERSION}" --force-bad-version --nomultimaint

# Create the tarball

# We always include these files which are the minimum files and
# folder required for Ubuntu/Debian packaging
# shellcheck disable=2034  # to_include appears to be unused
to_include=(
  common/build.sh \
  common/cpp \
  common/include \
  common/linux \
  common/test/keyboards/baseline \
  core \
  linux \
  resources/build/*.sh \
  resources/build/meson \
  resources/standards-data \
  resources/*.sh \
  ./*.md \
  ./build.sh \
  ./*.json \
)

# files and subfolders to exclude from paths included in 'to_include',
# i.e. the exceptions to 'to_include'.

# shellcheck disable=2034  # to_exclude appears to be unused
to_exclude=(
  build \
  common/test/keyboards/baseline/kmcomp-*.zip \
  linux/builddebs \
  linux/docs/help \
  linux/keyman-config/keyman_config/version.py \
  linux/keyman-config/buildtools/build-langtags.py \
)

if [[ -z "${create_origdist+x}" ]]; then
  # If we build a full source tarball we include additional files
  # so that it's possible to run `${KEYMAN_ROOT}/build.sh` on Linux

  # shellcheck disable=2034  # to_include appears to be unused
  to_include+=(
    common/tools/hextobin \
    common/web/keyman-version \
    common/web/langtags \
    common/web/types \
    common/windows/cpp \
    common/windows/include \
    developer/src/common/include \
    developer/src/common/web \
    developer/src/ext/json \
    developer/src/kmc \
    developer/src/kmc-analyze \
    developer/src/kmc-copy \
    developer/src/kmc-generate \
    developer/src/kmc-keyboard-info \
    developer/src/kmc-kmn \
    developer/src/kmc-ldml \
    developer/src/kmc-model \
    developer/src/kmc-model-info \
    developer/src/kmc-package \
    developer/src/kmcmplib \
    docs/minimum-versions.md.in
    resources/build \
    resources/standards-data \
  )

  # additional files and subfolders to exclude from paths included in 'to_include',
  # i.e. the exceptions to 'to_include'.
  # shellcheck disable=2034  # to_exclude appears to be unused
  to_exclude+=(
    *.exe \
    resources/build/history \
    resources/build/l10n \
    resources/build/mac \
    resources/build/win \
    resources/build/*.lua \
  )
fi

# array to store list of --tar-ignore parameters generated from to_include and to_exclude.
ignored_files=()

generate_tar_ignore_list "./" to_include to_exclude ignored_files "$(basename "${KEYMAN_ROOT}")"

# Note: explicitly specify the --tar-ignores here for files/folders that we always
# want to ignore regardless of their location. Having them here allows us to pass
# the wildcards to dpkg-source - whereas the wildcards in 'to_exclude' will be
# resolved and replaced with multiple --tar-ignore entries.
dpkg-source \
  --tar-ignore=*~ \
  --tar-ignore=.git \
  --tar-ignore=.gitattributes \
  --tar-ignore=.gitignore \
  --tar-ignore=experiments \
  --tar-ignore=debian \
  --tar-ignore=.github \
  --tar-ignore=.pc \
  --tar-ignore=.vscode \
  --tar-ignore=.devcontainer \
  --tar-ignore=.pc \
  --tar-ignore=__pycache__ \
  --tar-ignore=node_modules \
  --tar-ignore=keyman_1* \
  --tar-ignore=dist \
  --tar-ignore=VERSION \
  \
  "${ignored_files[@]}" \
  \
  -Zxz -b .

mv ../keyman_"${KEYMAN_VERSION}".tar.xz linux/dist/keyman-"${KEYMAN_VERSION}".tar.xz
echo "3.0 (quilt)" > debian/source/format
cd "${BASEDIR}"

# create orig.tar.xz
if [[ ! -z "${create_origdist+x}" ]]; then
    cd "${KEYMAN_ROOT}/linux/dist"
    pkgvers="keyman-${KEYMAN_VERSION}"
    tar xfJ keyman-"${KEYMAN_VERSION}".tar.xz
    mv -v keyman "${pkgvers}" 2>/dev/null || mv -v "$(find . -mindepth 1 -maxdepth 1 -type d)" "${pkgvers}"
    tar cfJ "keyman_${KEYMAN_VERSION}.orig.tar.xz" "${pkgvers}"
    rm "keyman-${KEYMAN_VERSION}.tar.xz"
    rm -rf "${pkgvers}"
fi
