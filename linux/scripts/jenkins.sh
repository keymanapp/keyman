#!/bin/bash -e
# $1 - project name with appended tier, e.g. ibus-kmfl-alpha

PROGRAM_NAME="$(basename "$0")"

. $HOME/ci-builder-scripts/bash/common.sh
init --no-package

keyman_projects="keyman-keyboardprocessor kmflcomp libkmfl ibus-kmfl keyman-config ibus-keyman"

tier="stable"

if [[ "$1" =~ "-alpha" ]]; then
    tier="alpha"
elif [[ "$1" =~ "-beta" ]]; then
    tier="beta"
fi

proj="$1"
proj=${proj%"-alpha"}
proj=${proj%"-beta"}

if [ "$proj" == "keyman-keyboardprocessor" ]; then
	fullsourcename="keyboardprocessor"
	sourcedir="../common/engine/keyboardprocessor"
else
	# check if project is known
	if [[ $keyman_projects =~ (^|[[:space:]])$proj($|[[:space:]]) ]]; then
		fullsourcename="$1"
		sourcedir="$proj"
	else
		stderr "$proj not in known projects ($keyman_projects)"
		exit -1
	fi
fi
sourcename=${fullsourcename%"-alpha"}
sourcename=${sourcename%"-beta"}

checkAndInstallRequirements()
{
	local TOINSTALL
	if ! dpkg -l | grep -q dh-python; then
		TOINSTALL="$TOINSTALL dh-python"
	fi

	if [ ! -f /usr/bin/help2man ]; then
		TOINSTALL="$TOINSTALL help2man"
	fi

	if [ ! -f /usr/bin/meson ]; then
		TOINSTALL="$TOINSTALL meson"
	fi

	if [ -n "$TOINSTALL" ]; then
		log "Installing prerequisites:$TOINSTALL"
		sudo apt-get update
		sudo apt-get -qy install $TOINSTALL
	fi
}

checkAndInstallRequirements

# clean up prev deb builds
log "cleaning previous builds of $1"


rm -rf builddebs
rm -rf $sourcedir/${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}
rm -rf $sourcedir/../${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}

log "Make source package for $fullsourcename"
log "reconfigure"
if [ "$proj" == "keyman-keyboardprocessor" ]; then
	mkdir -p keyboardprocessor
fi
JENKINS="yes" TIER="$tier" ./scripts/reconf.sh $sourcename

log "Make origdist"
./scripts/dist.sh origdist $sourcename
log "Make deb source"
./scripts/deb.sh sourcepackage $proj

#sign source package
for file in `ls builddebs/*.dsc`; do
	log "Signing source package $file"
	debsign -k$2 $file
done

mkdir -p $sourcedir
mv builddebs/* $sourcedir
