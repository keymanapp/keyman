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

if [[ ! -z ${1+x} ]] && [ "$1" == "origdist" ]; then
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
dpkg-source --tar-ignore=*~ \
    --tar-ignore=.git \
    --tar-ignore=.gitattributes \
    --tar-ignore=.gitignore \
    --tar-ignore=experiments \
    --tar-ignore=debian \
    --tar-ignore=.github \
    --tar-ignore=.vscode \
    --tar-ignore=android \
    --tar-ignore=.devcontainer \
    --tar-ignore=artifacts \
    \
    --tar-ignore=common/models \
    --tar-ignore=common/resources \
    --tar-ignore=common/schemas \
    --tar-ignore=common/test/keyboards/build.* \
    --tar-ignore=common/test/resources \
    --tar-ignore=common/web \
    --tar-ignore=common/windows \
    \
    --tar-ignore=core/build \
    --tar-ignore=developer \
    --tar-ignore=docs \
    --tar-ignore=ios \
    --tar-ignore=linux/keyman-config/keyman_config/version.py \
    --tar-ignore=linux/keyman-config/buildtools/build-langtags.py \
    --tar-ignore=__pycache__ \
    --tar-ignore=linux/help \
    --tar-ignore=mac \
    --tar-ignore=node_modules \
    --tar-ignore=oem \
    --tar-ignore=linux/build \
    --tar-ignore=linux/builddebs \
    --tar-ignore=linux/ibus-keyman/build \
    --tar-ignore=linux/keyman-system-service/build \
    --tar-ignore=resources/devbox \
    --tar-ignore=resources/environment.sh \
    --tar-ignore=resources/git-hooks \
    --tar-ignore=resources/scopes \
    --tar-ignore=resources/build/*.lua \
    --tar-ignore=resources/build/jq* \
    --tar-ignore=results \
    --tar-ignore=tmp \
    --tar-ignore=web \
    --tar-ignore=windows \
    --tar-ignore=keyman_1* \
    --tar-ignore=dist \
    --tar-ignore=VERSION \
    \
    -Zgzip -b .
mv ../keyman_"${VERSION}".tar.gz linux/dist/keyman-"${VERSION}".tar.gz
echo "3.0 (quilt)" > debian/source/format
cd "$BASEDIR"

# create orig.tar.gz
if [ ! -z "${create_origdist+x}" ]; then
    cd dist
    pkgvers="keyman-$VERSION"
    tar xfz keyman-"${VERSION}".tar.gz
    mv -v keyman "${pkgvers}" 2>/dev/null || mv -v "$(find . -mindepth 1 -maxdepth 1 -type d)" "${pkgvers}"
    tar cfz "keyman_${VERSION}.orig.tar.gz" "${pkgvers}"
    rm "keyman-${VERSION}.tar.gz"
    rm -rf "${pkgvers}"
fi
