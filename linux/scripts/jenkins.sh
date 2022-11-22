#!/bin/bash
# $1 - project name with appended tier, e.g. ibus-kmfl-alpha
# $2 - GPG key used for signing the source package

set -e
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

keyman_projects="keyman"

tier="stable"

if [[ "$1" =~ "-alpha" ]]; then
    tier="alpha"
elif [[ "$1" =~ "-beta" ]]; then
    tier="beta"
fi

proj="$1"
proj=${proj%"-alpha"}
proj=${proj%"-beta"}

fullsourcename="keyman"
sourcedir="$KEYMAN_ROOT"
sourcename=${fullsourcename%"-alpha"}
sourcename=${sourcename%"-beta"}

# set Debian/changelog environment
export DEBFULLNAME="${fullsourcename} Package Signing Key"
export DEBEMAIL='jenkins@sil.org'

checkAndInstallRequirements()
{
	local TOINSTALL=""

	for p in devscripts equivs
	do
		if ! dpkg -s $p >/dev/null 2>&1; then
			TOINSTALL="$TOINSTALL $p"
		fi
	done

	export DEBIAN_FRONTEND=noninteractive

	if [ -n "$TOINSTALL" ]; then
		sudo apt-get update
		sudo apt-get -qy install $TOINSTALL
	fi

	sudo mk-build-deps debian/control
	sudo apt-get -qy --allow-downgrades install ./keyman-build-deps_*.deb
	sudo rm -f keyman-buid-deps_*
}

checkAndInstallRequirements

# clean up prev deb builds
echo_heading "cleaning previous builds of $1"

rm -rf builddebs
rm -rf $sourcedir/${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}
rm -rf $sourcedir/../${1}_*.{dsc,build,buildinfo,changes,tar.?z,log}

echo_heading "Make source package for $fullsourcename"
echo_heading "reconfigure"
JENKINS="yes" TIER="$tier" ./scripts/reconf.sh $sourcename

echo_heading "Make origdist"
./scripts/dist.sh origdist $sourcename
echo_heading "Make deb source"
./scripts/deb.sh sourcepackage $proj

#sign source package
for file in $(ls builddebs/*.dsc); do
	echo_heading "Signing source package $file"
	debsign -k$2 $file
done

if [ "$proj" == "keyman" ]; then
    mv builddebs/* ..
else
    mkdir -p $sourcedir
    mv builddebs/* $sourcedir
fi
