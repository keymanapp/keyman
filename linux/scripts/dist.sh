#!/bin/bash

# Build dist tarballs or Debian orig tarballs
# and put them in dist/

# parameters: ./dist.sh [origdist] [proj]
# origdist = create Debian orig.tar.gz
# proj = only make tarball for this project

set -e

BASEDIR=`pwd`
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
    oldvers=`cat VERSION`
    vers=`./configure -version|grep -E 'kmfl|keyman'|grep -Po "${proj} configure \K[^a-z]*"`
    echo "$vers" > VERSION
    rm -rf ../build-$proj
    mkdir -p ../build-$proj
    cd ../build-$proj
    GTK_CFLAGS="-I/usr/include/gtk-3.0" GTK_LIBS="-lgtk-3 -lgdk-3" JSON_GLIB_CFLAGS="-I/usr/include/json-glib-1.0" JSON_GLIB_LIBS="-ljson-glib-1.0" KEYMAN_PROC_CFLAGS="-I/usr/include/keyman" KEYMAN_PROC_LIBS="-lkmnkbp" ../$proj/configure
    make dist
    mv *.tar.gz ../dist
    cd $BASEDIR
    echo "$oldvers" > VERSION
done

for proj in ${extra_projects}; do
    # dist for keyman-config
    if [ "${proj}" == "keyman-config" ]; then

        cd keyman-config
        vers=`cat ../../VERSION.md`
        echo "3.0 (native)" > debian/source/format
        dch keyman-config --newversion ${vers} --force-bad-version --nomultimaint
        dpkg-source --tar-ignore=*~ --tar-ignore=.git --tar-ignore=.gitattributes \
            --tar-ignore=.gitignore --tar-ignore=experiments --tar-ignore=debian \
            --tar-ignore=__pycache__ -Zgzip -b .
        mv ../keyman-config_*.tar.gz ../dist/keyman-config.tar.gz
        echo "3.0 (quilt)" > debian/source/format
    elif [ "${proj}" == "keyboardprocessor" ]; then
        cd ../common/core
        vers=`cat ../../VERSION.md`
        kbpvers="keyman-keyboardprocessor-$vers"
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
        tmp=`basename ${proj}*.tar.gz .tar.gz`
        origname=${tmp/$proj-/$proj\_}
        index=`expr index "${origname}" _`
        mv $proj*.tar.gz ${origname}.orig.tar.gz
    done

    for proj in ${extra_projects}; do
        if [ "${proj}" == "keyman-config" ]; then
            vers=`cat ../../VERSION.md`
            pkgvers="keyman-config-$vers"
            tar xfz keyman-config.tar.gz
            mv keyman-config ${pkgvers}
            tar cfz keyman-config_${vers}.orig.tar.gz ${pkgvers}
            rm keyman-config.tar.gz
            rm -rf ${pkgvers}
        elif [ "${proj}" == "keyboardprocessor" ]; then
            tmp=`basename keyman-keyboardprocessor*.tar.gz .tar.gz`
            origname=${tmp/keyman-keyboardprocessor-/keyman-keyboardprocessor\_}
            index=`expr index "${origname}" _`
            mv keyman-keyboardprocessor*.tar.gz ${origname}.orig.tar.gz
        fi
    done
fi
