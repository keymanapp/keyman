#!/bin/bash

# Build Debian packages of Keyman with pbuilder

# It must be run from the keyman/linux directory


set -e

distributions="bionic xenial"
autotool_projects="kmflcomp libkmfl ibus-kmfl"
all_projects="${autotool_projects} keyman-config"
echo "distributions: ${distributions}"
echo "autotool_projects: ${autotool_projects}"
echo "all_projects: ${all_projects}"

declare -A proj_versions=()
BASEDIR=`pwd`

# autoreconf the projects
for proj in ${autotool_projects}; do
	cd $proj
	echo "Reconfiguring $proj"
	autoreconf
	cd $BASEDIR
done

mkdir -p builddebs
mkdir -p builddebs/xenial
mkdir -p builddebs/bionic

for proj in ${autotool_projects}; do
	rm -rf deb-$proj
	mkdir deb-$proj
	cd deb-$proj
	../$proj/configure
	make dist
	tmp=`basename *.tar.gz .tar.gz`
	origname=${tmp/$proj-/$proj\_}
	index=`expr index "${origname}" _`
	proj_versions[$proj]=${tmp:$index}
	cp $proj*.tar.gz ../builddebs/${origname}.orig.tar.gz
	cd ${BASEDIR}
done

cd keyman-config
rm -rf dist make_deb build
python3 setup.py egg_info -b.`date -u +"%Y%m%d%H%M%S"` sdist
cd dist
ls *.tar.gz
tmp=`basename *.tar.gz .tar.gz`
origname=${tmp/keyman\_config-/keyman-config\_}
index=`expr index "${origname}" _`
proj_versions[keyman-config]=${tmp:$index}
kcversion=${proj_versions[keyman-config]}

echo "keyman-config version: ${kcversion}"

tar xf keyman_config-${kcversion}.tar.gz
mv keyman_config-${kcversion} keyman-config-${kcversion}
tar cfz keyman-config_${kcversion}.orig.tar.gz keyman-config-${kcversion}

cp keyman-config_${kcversion}.orig.tar.gz ${BASEDIR}/builddebs/
cd ${BASEDIR}

for proj in ${all_projects}; do
	cd builddebs
	echo "$proj version ${proj_versions[$proj]}"
	rm -rf ${proj}-${proj_versions[$proj]}
	tar xfz ${proj}_${proj_versions[$proj]}.orig.tar.gz
	cp -a ../$proj/debian ${proj}-${proj_versions[$proj]}
	cd ${proj}-${proj_versions[$proj]}
	for dist in ${distributions}; do
		dch -v ${proj_versions[$proj]}-1${dist} "local build"
		echo "$dist"
		pdebuild --pbuilder cowbuilder --buildresult /var/cache/pbuilder/result/${dist} -- --basepath /var/cache/pbuilder/base-${dist}.cow --distribution ${dist} --override-config --othermirror="deb [trusted=yes] file:/var/cache/pbuilder/result/${dist} ./" --bindmounts /var/cache/pbuilder/result/${dist}  --hookdir /var/cache/pbuilder/hook.d/${dist}
	done
	cd ${BASEDIR}
done
