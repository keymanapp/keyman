#!/bin/bash -e

. $HOME/ci-builder-scripts/bash/common.sh
init --no-package

keyman_projects="keyman-keyboardprocessor kmflcomp libkmfl ibus-kmfl keyman-config ibus-keyman"

if [ "$1" == "keyman-keyboardprocessor" ]; then
	sourcename="keyboardprocessor"
	sourcedir="../common/engine/keyboardprocessor"
else
	# check if project is known
	if [[ $keyman_projects =~ (^|[[:space:]])$1($|[[:space:]]) ]]; then
		sourcename="$1"
		sourcedir="$1"
	else
		stderr "$1 not in known projects ($keyman_projects)"
		exit -1
	fi
fi

# clean up prev deb builds
log "cleaning previous builds of $1"


rm -rf builddebs
rm -rf $sourcedir/${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}
rm -rf $sourcedir/../${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}

log "Make source package for $sourcename"
log "reconfigure"
if [ "$1" == "keyman-keyboardprocessor" ]; then
	mkdir -p keyboardprocessor
else
	JENKINS="yes" ./scripts/reconf.sh $sourcename
fi
log "Make origdist"
./scripts/dist.sh origdist $sourcename
log "Make deb source"
./scripts/deb.sh sourcepackage $1

#sign source package
for file in `ls builddebs/*.dsc`; do
	log "Signing source package $file"
	debsign -k$2 $file
done

mkdir -p $sourcedir
mv builddebs/* $sourcedir
