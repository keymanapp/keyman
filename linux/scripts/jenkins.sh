#!/bin/bash -e

. $HOME/ci-builder-scripts/bash/common.sh
init --no-package

# clean up prev deb builds
log "cleaning previous builds of $1"
rm -rf builddebs
rm -rf $1/${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}
rm -rf ${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}

log "Make source package for $1"
./scripts/reconf.sh dev $1
./scripts/dist.sh origdist $1
./scripts/deb.sh sourcepackage $1

#sign source package
for file in `ls builddebs/*.dsc`; do
	log "Signing source package $file"
	debsign -k$2 $file
done

mv builddebs/* $1
