#!/bin/bash

function checkPrerequisites() {
    if [ "${UPLOAD:=}" == "yes" ]; then
        SIM=""
    else
        SIM="-s"
    fi

    # Check the tier
    if [[ -z "${TIER:=}" ]]; then
        echo "TIER.md or \${TIER} must be set to (alpha, beta, stable) to use this script"
        exit 1
    fi

    if ! which xmllint > /dev/null; then
        echo "you must install xmllint (libxml2-utils package) to use this script"
        exit 1
    fi

    # shellcheck disable=SC2034
    projects="${PROJECT:=keyman}"
}

function downloadSource() {
    local packageDir
    packageDir=$1

    if [ "${proj:=}" == "keyman" ]; then
       cd "${BASEDIR}" || exit
    fi

    if [ "${proj}" == "keyman" ]; then
        make clean
    fi

    # Update tier in Debian watch files (replacing any previously set tier) and remove comment
    sed -e "s/\$tier\|alpha\|beta\|stable/${TIER}/g" -e "s/^# .*$//" "$BASEDIR"/scripts/watch.in > debian/watch

    version=$(uscan --report --dehs|xmllint --xpath "//dehs/upstream-version/text()" -)
    dirversion=$(uscan --report --dehs|xmllint --xpath "//dehs/upstream-url/text()" - | cut -d/ -f6)
    echo "${proj} version is ${version}"
    uscan || (echo "ERROR: No new version available for ${proj}" >&2 && exit 1)
    cd ..
    mv "${proj}-${version}" "${BASEDIR}/${packageDir}"
    mv "${proj}_${version}.orig.tar.gz" "${BASEDIR}/${packageDir}"
    mv "${proj}-${version}.tar.gz" "${BASEDIR}/${packageDir}"
    mv "${proj}"*.asc "${BASEDIR}/${packageDir}"
    rm "${proj}"*.debian.tar.xz
    cd "${BASEDIR}/${packageDir}" || exit
    wget -N "https://downloads.keyman.com/linux/${TIER}/${dirversion}/SHA256SUMS"
    sha256sum -c --ignore-missing SHA256SUMS |grep "${proj}"
}

function checkAndInstallRequirements()
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
		# shellcheck disable=SC2086
		sudo apt-get -qy install $TOINSTALL
	fi

	sudo mk-build-deps debian/control
	sudo apt-get -qy --allow-downgrades install ./keyman-build-deps_*.deb
	sudo rm -f keyman-buid-deps_*
}
