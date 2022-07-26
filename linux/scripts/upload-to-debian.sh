#!/bin/bash
# Upload the latest release to mentors.debian.net. Call from stable-* branch.
# Usage: upload-to-debian.sh -k <Debian signing key>
# Requires an entry in ~/.dput.cf:
# ```
# [mentors]
# fqdn = mentors.debian.net
# incoming = /upload
# method = https
# allow_unsigned_uploads = 0
# progress_indicator = 2
# allowed_distributions = .*
# ```

set -e

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

PROGRAM_NAME="$(basename "$0")"
RED='\033[0;31m'
GREEN='\033[0;32m'
NC='\033[0m' # No Color

error()
{
	echo -e "${RED}$PROGRAM_NAME: $1${NC}" >&2
}

log()
{
	echo -e "${GREEN}$PROGRAM_NAME: $1${NC}"
}

usage()
{
    echo ""
    echo "Usage:"
    echo "${0} -k <Debian signing key> [-n] [--push]"
    echo ""
    echo "Create source package for latest stable release and upload to Debian,"
    echo "and update (and optionally push) changelog file."
    echo ""
    echo "    -k <key>    The PGP key used to sign the source package"
    echo "    -n          Simulate upload"
    echo "    --push      Push changelog changes to GitHub"
    echo "    --help      Display usage help"
}

while (( $# )); do
    case $1 in
        -k) shift; debkeyid=$1;;
        -n) NOOP=: ;;
        --help) usage ; exit 0 ;;
        --push) PUSH=1 ;;
        *) error "Error: Unexpected argument \"$1\". Exiting." ; exit 1 ;;
    esac
    shift || (error "Error: The last argument is missing a value. Exiting."; false) || exit 2
done

if [ -z "$debkeyid" ]; then
    usage
    exit 3
fi

if ! git diff --quiet; then
    error "You have changed files in your git working directory. Exiting."
    exit 4
fi

stable_branch=$(git branch -r | grep origin/stable- | sort | tail -1)
stable_branch=${stable_branch##* }
git checkout ${stable_branch#origin/}

cd $KEYMAN_ROOT/linux
log "Building source package"
DIST=unstable scripts/debian.sh
cd debianpackage/
log "Signing source package"
debsign -k$debkeyid --re-sign *.changes
log "Uploading packages to mentors.debian.net"
$NOOP dput mentors *.changes
cd ..

log "Updating changelog"
git fetch -p origin
git checkout -B chore/linux/changelog $stable_branch
cp debianpackage/keyman-*/debian/changelog debian/
git add debian/changelog
git commit -m "chore(linux): Update debian changelog"
if [ -n "$PUSH" ]; then
    $NOOP git push origin chore/linux/changelog
fi

git checkout -B chore/linux/cherry-pick/changelog origin/master
git cherry-pick -x chore/linux/changelog
if [ -n "$PUSH" ]; then
    $NOOP git push origin chore/linux/cherry-pick/changelog
fi
