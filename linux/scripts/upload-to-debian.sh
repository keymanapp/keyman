#!/usr/bin/env bash
set -eu

usage()
{
    cat - <<- END

${0} -k <Debian signing key> [-n] [--push]

Create source package for latest stable release and upload to Debian,
and update (and optionally push) changelog file.

    -k <key>    The PGP key used to sign the source package
    -n          Simulate upload
    --push      Push changelog changes to GitHub
    --debian-revision <revision>
                The debian revision to use. Default: 1
    --help      Display usage help

Requires an entry in ~/.dput.cf:
    [mentors]
    fqdn = mentors.debian.net
    incoming = /upload
    method = https
    allow_unsigned_uploads = 0
    progress_indicator = 2
    allowed_distributions = .*
END
}

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

NOOP=
PUSH=
DEBKEYID=
REVISION=1

while (( $# )); do
    case $1 in
        -k) shift
            if [ ! $# -eq 0 ]; then
                DEBKEYID=$1
            else
                builder_die "Error: The -k argument is missing a value. Exiting."
            fi;;
        -n) NOOP=: ;;
        --help) usage ; exit 0 ;;
        --push) PUSH=1 ;;
        --debian-revision)
            shift
            if [ ! $# -eq 0 ]; then
                REVISION=$1
            else
                builder_die "Error: The --debian-revision argument is missing a value. Exiting."
            fi;;
        *) builder_die "Error: Unexpected argument \"$1\". Exiting." ;;
    esac
    shift || builder_die "Error: The last argument is missing a value. Exiting."
done

if [ -z "$DEBKEYID" ]; then
    usage
    exit 2
fi

if ! git diff --quiet; then
    builder_die "You have changed files in your git working directory. Exiting."
fi

if [[ "$(cat TIER.md)" == "beta" ]]; then
    ISBETA=true
else
    ISBETA=false
fi

builder_heading "Fetching latest changes"
git fetch -p origin
if ${ISBETA}; then
  git checkout beta
  git pull origin beta
else
  stable_branch=$(git branch -r | grep origin/stable- | sort | tail -1)
  stable_branch=${stable_branch##* }
  # Checkout stable branch so that `scripts/debian.sh` picks up correct version
  git checkout "${stable_branch#origin/}"
  git pull origin "${stable_branch#origin/}"
fi

cd "$KEYMAN_ROOT/linux"
builder_heading "Building source package"
DIST=unstable DEBREVISION=$REVISION scripts/debian.sh
cd debianpackage/
builder_heading "Signing source package"
debsign -k"$DEBKEYID" --re-sign ./*.changes
builder_heading "Uploading packages to mentors.debian.net"
$NOOP dput mentors ./*.changes
cd ..

builder_heading "Updating changelog"
if ${ISBETA}; then
    CLBRANCH=origin/beta
else
    CLBRANCH="${stable_branch}"
fi

# base changelog branch on remote stable/beta branch
git checkout -B chore/linux/changelog "${CLBRANCH}"
cp debianpackage/keyman-*/debian/changelog debian/
git add debian/changelog
git commit -m "chore(linux): Update debian changelog"
if [ -n "$PUSH" ]; then
    $NOOP git push --force-with-lease origin chore/linux/changelog
    $NOOP gh pr create --draft --base "${CLBRANCH#origin/}" --title "chore(linux): Update debian changelog" --body "@keymanapp-test-bot skip"
fi

git checkout -B chore/linux/cherry-pick/changelog origin/master
git cherry-pick -x chore/linux/changelog
if [ -n "$PUSH" ]; then
    $NOOP git push --force-with-lease origin chore/linux/cherry-pick/changelog
    $NOOP gh pr create --draft --base master --title "chore(linux): Update debian changelog 🍒" --body "@keymanapp-test-bot skip"
fi

builder_heading "Finishing"
if $ISBETA; then
    git checkout beta
else
    git checkout master
fi
