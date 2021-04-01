#!/bin/bash

# Build dist tarballs or Debian orig tarballs
# and put them in dist/

# parameters: ./dist.sh [origdist] [proj]
# origdist = create Debian orig.tar.gz
# proj = only make tarball for this project

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

BASEDIR=$(pwd)
autotool_projects="kmflcomp libkmfl ibus-kmfl ibus-keyman"
extra_projects="keyboardprocessor keyman-config"

if [ "$1" == "origdist" ]; then
    if [ "$2" != "" ]; then
        echo "$2"
        if [ ! -d "$2" ]; then
            echo "project $2 does not exist"
            exit 1
        fi
        if [ "$2" == "keyman-config" ] || [ "$2" == "keyboardprocessor" ]; then
            autotool_projects=""
            extra_projects="$2"
        else
            autotool_projects="$2"
            extra_projects=""
        fi
    fi
else
    if [ "$1" != "" ]; then
        echo "$1"
        if [ ! -d "$1" ]; then
            echo "project $1 does not exist"
            exit 1
        fi
        if [ "$1" == "keyman-config" ] || [ "$1" == "keyboardprocessor" ]; then
            autotool_projects=""
            extra_projects="$1"
        else
            autotool_projects="$1"
            extra_projects=""
        fi
    fi
fi


rm -rf dist
mkdir -p dist

# configure and make dist for autotool projects
for proj in ${autotool_projects}; do
    echo "configure $proj"
    cd $proj
    rm -rf ../build-$proj
    mkdir -p ../build-$proj
    cd ../build-$proj
    GTK_CFLAGS="-I/usr/include/gtk-3.0" GTK_LIBS="-lgtk-3 -lgdk-3" JSON_GLIB_CFLAGS="-I/usr/include/json-glib-1.0" JSON_GLIB_LIBS="-ljson-glib-1.0" KEYMAN_PROC_CFLAGS="-I/usr/include/keyman" KEYMAN_PROC_LIBS="-lkmnkbp" ../$proj/configure
    make dist
    mv *.tar.gz ../dist
    cd $BASEDIR
done

for proj in ${extra_projects}; do
    # dist for keyman-config
    if [ "${proj}" == "keyman-config" ]; then
        cd keyman-config
        echo "3.0 (native)" > debian/source/format
        dch keyman-config --newversion ${VERSION} --force-bad-version --nomultimaint
        dpkg-source --tar-ignore=*~ --tar-ignore=.git --tar-ignore=.gitattributes \
            --tar-ignore=.gitignore --tar-ignore=experiments --tar-ignore=debian \
            --tar-ignore=buildtools --tar-ignore=__pycache__ -Zgzip -b .
        mv ../keyman-config_*.tar.gz ../dist/keyman-config-${VERSION}.tar.gz
        echo "3.0 (quilt)" > debian/source/format
    elif [ "${proj}" == "keyboardprocessor" ]; then
        cd ../common/core
        kbpvers="keyman-keyboardprocessor-$VERSION"
        cp -a desktop $kbpvers
        cp ../../VERSION.md $kbpvers
        tar cvzf $kbpvers.tar.gz --exclude=debian --exclude=build --exclude=.gitignore $kbpvers
        rm -rf $kbpvers
        cp $kbpvers.tar.gz ../../linux/dist
    fi
    cd $BASEDIR
done

# create orig.tar.gz
if [ "$1" == "origdist" ]; then
    cd dist
    for proj in ${autotool_projects}; do
        tmp=$(basename ${proj}*.tar.gz .tar.gz)
        origname=${tmp/$proj-/$proj\_}
        index=$(expr index "${origname}" _)
        mv $proj*.tar.gz ${origname}.orig.tar.gz
    done

    for proj in ${extra_projects}; do
        if [ "${proj}" == "keyman-config" ]; then
            pkgvers="keyman-config-$VERSION"
            tar xfz keyman-config-${VERSION}.tar.gz
            mv keyman-config ${pkgvers}
            tar cfz keyman-config_${VERSION}.orig.tar.gz ${pkgvers}
            rm keyman-config-${VERSION}.tar.gz
            rm -rf ${pkgvers}
        elif [ "${proj}" == "keyboardprocessor" ]; then
            tmp=$(basename keyman-keyboardprocessor*.tar.gz .tar.gz)
            origname=${tmp/keyman-keyboardprocessor-/keyman-keyboardprocessor\_}
            index=$(expr index "${origname}" _)
            mv keyman-keyboardprocessor*.tar.gz ${origname}.orig.tar.gz
        fi
    done
fi
