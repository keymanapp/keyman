#!/bin/bash -e

. $HOME/ci-builder-scripts/bash/common.sh
init --no-package

if [ "$1" == "keyman-keyboardprocessor" ]; then
	sourcename="keyboardprocessor"
else
	sourcename="$1"
fi

# clean up prev deb builds
log "cleaning previous builds of $1"
rm -rf builddebs
rm -rf $1/${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}
rm -rf ${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}

log "Make source package for $sourcename"
log "reconfigure"
JENKINS="yes" ./scripts/reconf.sh $sourcename
log "Make origdist"
./scripts/dist.sh origdist $sourcename
log "Make deb source"
./scripts/deb.sh sourcepackage $1

#sign source package
for file in `ls builddebs/*.dsc`; do
	log "Signing source package $file"
	debsign -k$2 $file
done

mkdir -p $1
mv builddebs/* $1
