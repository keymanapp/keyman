#!/bin/bash

# Build dist tarballs or Debian orig tarballs
# and put them in dist/

# parameters: [BUILD_LEGACY=1] ./dist.sh [origdist] [proj]
# origdist = create Debian orig.tar.gz
# proj = only make tarball for this project
# BUILD_LEGACY = additionaly build legacy KMFL tarballs

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

BASEDIR=$(pwd)
legacy_projects=""
extra_projects="keyman"

if [ -n "$BUILD_LEGACY" ]; then
    legacy_projects="kmflcomp libkmfl ibus-kmfl"
fi

if [ "$1" == "origdist" ]; then
    create_origdist=1
    shift
fi

if [ "$1" != "" ]; then
    echo "$1"
    if [ "$1" == "keyman" ]; then
        legacy_projects=""
        extra_projects="$1"
    elif [ -d "legacy/$1" ]; then
        legacy_projects="$1"
        extra_projects=""
    else
        echo "project $1 does not exist"
        exit 1
    fi
fi

rm -rf dist
mkdir -p dist

# configure and make dist for autotool projects
for proj in ${legacy_projects}; do
    echo "configure $proj"
    cd legacy/$proj
    rm -rf ../../build-$proj
    mkdir -p ../../build-$proj
    cd ../../build-$proj
    GTK_CFLAGS="-I/usr/include/gtk-3.0" GTK_LIBS="-lgtk-3 -lgdk-3" JSON_GLIB_CFLAGS="-I/usr/include/json-glib-1.0" JSON_GLIB_LIBS="-ljson-glib-1.0" KEYMAN_PROC_CFLAGS="-I/usr/include/keyman" KEYMAN_PROC_LIBS="-lkmnkbp" ../legacy/$proj/configure
    make dist
    mv *.tar.gz ../dist
    cd $BASEDIR
done

for proj in ${extra_projects}; do
    # dist for keyman
    cp -a debian ../
    cd ..
    echo "3.0 (native)" > debian/source/format
    dch keyman --newversion ${VERSION} --force-bad-version --nomultimaint
    dpkg-source --tar-ignore=*~ --tar-ignore=.git --tar-ignore=.gitattributes \
        --tar-ignore=.gitignore --tar-ignore=experiments --tar-ignore=debian \
        --tar-ignore=.github --tar-ignore=.vscode --tar-ignore=android \
        --tar-ignore=common/models --tar-ignore=common/predictive-text \
        --tar-ignore=common/resources --tar-ignore=common/schemas \
        --tar-ignore=common/test --tar-ignore=common/web --tar-ignore=common/windows \
        --tar-ignore=developer --tar-ignore=docs --tar-ignore=ios \
        --tar-ignore=linux/keyman-config/keyman_config/version.py \
        --tar-ignore=linux/keyman-config/buildtools/build-langtags.py --tar-ignore=__pycache__ \
        --tar-ignore=linux/help --tar-ignore=linux/Jenkinsfile \
        --tar-ignore=linux/keyboardprocessor --tar-ignore=linux/legacy \
        --tar-ignore=mac --tar-ignore=node_modules --tar-ignore=oem \
        --tar-ignore=linux/build* --tar-ignore=core/build \
        --tar-ignore=resources/devbox --tar-ignore=resources/git-hooks \
        --tar-ignore=resources/scopes \
        --tar-ignore=resources/build/*.lua --tar-ignore=resources/build/jq* \
        --tar-ignore=resources/build/vswhere* --tar-ignore=results \
        --tar-ignore=web --tar-ignore=windows --tar-ignore=keyman_1* \
        --tar-ignore=dist --tar-ignore=.pbuilderrc --tar-ignore=VERSION \
        --tar-ignore=scripts -Zgzip -b .
    mv ../keyman_${VERSION}.tar.gz linux/dist/keyman-${VERSION}.tar.gz
    echo "3.0 (quilt)" > debian/source/format
    cd $BASEDIR
done

# create orig.tar.gz
if [ -n "$create_origdist" ]; then
    cd dist
    for proj in ${legacy_projects}; do
        tmp=$(basename ${proj}*.tar.gz .tar.gz)
        origname=${tmp/$proj-/$proj\_}
        index=$(expr index "${origname}" _)
        mv $proj*.tar.gz ${origname}.orig.tar.gz
    done

    for proj in ${extra_projects}; do
        pkgvers="keyman-$VERSION"
        tar xfz keyman-${VERSION}.tar.gz
        mv -v keyman ${pkgvers} 2>/dev/null || mv -v $(find . -mindepth 1 -maxdepth 1 -type d) ${pkgvers}
        tar cfz keyman_${VERSION}.orig.tar.gz ${pkgvers}
        rm keyman-${VERSION}.tar.gz
        rm -rf ${pkgvers}
    done
fi
