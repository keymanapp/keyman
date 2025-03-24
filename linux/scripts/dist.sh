#!/bin/bash

# Build dist tarballs or Debian orig tarballs
# and put them in dist/

# parameters: ./dist.sh [origdist] [proj]
# origdist = create Debian orig.tar.gz
# proj = only make tarball for this project

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

BASEDIR=$(pwd)

if [[ ! -z ${1+x} ]] && [[ "$1" == "origdist" ]]; then
    create_origdist=1
    shift
fi

rm -rf dist
mkdir -p dist

# dist for keyman
cp -a debian ../
cd ..
echo "3.0 (native)" > debian/source/format
dch keyman --newversion "${VERSION}" --force-bad-version --nomultimaint

# Create the tarball
# Note: the files end up in subdirectories under `keyman`, so we can
# include that when matching files and directories to ignore.
dpkg-source \
  --tar-ignore=*~ \
  --tar-ignore=.git \
  --tar-ignore=.gitattributes \
  --tar-ignore=.gitignore \
  --tar-ignore=experiments \
  --tar-ignore=debian \
  --tar-ignore=.github \
  --tar-ignore=.vscode \
  --tar-ignore=.devcontainer \
  --tar-ignore=__pycache__ \
  --tar-ignore=node_modules \
  --tar-ignore=keyman_1* \
  --tar-ignore=dist \
  --tar-ignore=VERSION \
  \
  --tar-ignore=keyman/android \
  --tar-ignore=keyman/artifacts \
  \
  --tar-ignore=keyman/common/mac \
  --tar-ignore=keyman/common/models \
  --tar-ignore=keyman/common/resources \
  --tar-ignore=keyman/common/schemas \
  --tar-ignore=keyman/common/test/keyboards/build.* \
  --tar-ignore=keyman/common/test/resources \
  --tar-ignore=keyman/common/web \
  --tar-ignore=keyman/common/windows \
  \
  --tar-ignore=keyman/core/build \
  --tar-ignore=keyman/developer \
  --tar-ignore=keyman/docs \
  --tar-ignore=keyman/ios \
  --tar-ignore=keyman/linux/build \
  --tar-ignore=keyman/linux/builddebs \
  --tar-ignore=keyman/linux/docs/help \
  --tar-ignore=keyman/linux/ibus-keyman/build \
  --tar-ignore=keyman/linux/keyman-config/keyman_config/version.py \
  --tar-ignore=keyman/linux/keyman-config/buildtools/build-langtags.py \
  --tar-ignore=keyman/linux/keyman-system-service/build \
  --tar-ignore=keyman/mac \
  --tar-ignore=keyman/oem \
  --tar-ignore=keyman/resources/devbox \
  --tar-ignore=keyman/resources/environment.sh \
  --tar-ignore=keyman/resources/git-hooks \
  --tar-ignore=keyman/resources/scopes \
  --tar-ignore=keyman/resources/build/*.lua \
  --tar-ignore=keyman/resources/build/jq* \
  --tar-ignore=keyman/results \
  --tar-ignore=keyman/tmp \
  --tar-ignore=keyman/web \
  --tar-ignore=keyman/windows \
  \
  -Zgzip -b .
mv ../keyman_"${VERSION}".tar.gz linux/dist/keyman-"${VERSION}".tar.gz
echo "3.0 (quilt)" > debian/source/format
cd "${BASEDIR}"

# create orig.tar.gz
if [[ ! -z "${create_origdist+x}" ]]; then
    cd dist
    pkgvers="keyman-$VERSION"
    tar xfz keyman-"${VERSION}".tar.gz
    mv -v keyman "${pkgvers}" 2>/dev/null || mv -v "$(find . -mindepth 1 -maxdepth 1 -type d)" "${pkgvers}"
    tar cfz "keyman_${VERSION}.orig.tar.gz" "${pkgvers}"
    rm "keyman-${VERSION}.tar.gz"
    rm -rf "${pkgvers}"
fi
