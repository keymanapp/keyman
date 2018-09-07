#!/bin/bash

# Build dist tarballs or Debian orig tarballs
# and put them in dist/

# parameters: ./dist.sh [origdist] [proj]
# origdist = create Debian orig.tar.gz
# proj = only make tarball for this project

set -e

BASEDIR=`pwd`
autotool_projects="kmflcomp libkmfl ibus-kmfl"
extra_project="keyman-config"

if [ "$1" == "origdist" ]; then
    if [ "$2" != "" ]; then
        if [ ! -d "$2" ]; then
            echo "project $2 does not exist"
            exit 1
        fi
        if [ "$2" == "keyman-config" ]; then
            autotool_projects=""
        else
            autotool_projects="$2"
            extra_project=""
        fi
    fi
else
    if [ "$1" != "" ]; then
        if [ ! -d "$1" ]; then
            echo "project $1 does not exist"
            exit 1
        fi
        if [ "$1" == "keyman-config" ]; then
            autotool_projects=""
        else
            autotool_projects="$1"
            extra_project=""
        fi
    fi
fi


rm -rf dist
mkdir -p dist

# configure and make dist for autotool projects
for proj in ${autotool_projects}; do
	cd $proj
    rm -rf ../build-$proj
    mkdir -p ../build-$proj
    cd ../build-$proj
    ../$proj/configure
    make dist
    mv *.tar.gz ../dist
	cd $BASEDIR
done


# dist for keyman-config
if [ "${extra_project}" == "keyman-config" ]; then
    cd kmflcomp
    baseversion=`cat VERSION`
    echo "baseversion: ${baseversion}"
    distversion=`./configure --version|grep kmfl|grep -Po 'kmflcomp configure \K[^a-z]*'`
    echo "distversion: ${distversion}"

    cd ../keyman-config
    rm -rf dist
    if [ "${distversion}" == "${baseversion}" ]; then
        python3 setup.py sdist
    else
        datever=${distversion##*.}
        echo "datever: ${datever}"
        python3 setup.py egg_info -b.${datever} sdist
    fi
    cp dist/*.tar.gz ../dist
fi
cd $BASEDIR

# create orig.tar.gz
if [ "$1" == "origdist" ]; then
    cd dist
    for proj in ${autotool_projects}; do
        tmp=`basename ${proj}*.tar.gz .tar.gz`
        origname=${tmp/$proj-/$proj\_}
        index=`expr index "${origname}" _`
        mv $proj*.tar.gz ${origname}.orig.tar.gz
    done

    if [ "${extra_project}" == "keyman-config" ]; then
        tmp=`basename keyman\_config*.tar.gz .tar.gz`
        origname=${tmp/keyman\_config-/keyman-config\_}
        index=`expr index "${origname}" _`
        kcversion=${tmp:$index}

        echo "keyman-config version: ${kcversion}"

        tar xf keyman_config-${kcversion}.tar.gz
        mv keyman_config-${kcversion} keyman-config-${kcversion}
        tar cfz keyman-config_${kcversion}.orig.tar.gz keyman-config-${kcversion}
        rm -rf keyman-config-${kcversion} keyman_config-${kcversion}.tar.gz
    fi
fi
