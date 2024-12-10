#!/usr/bin/env bash
set -eu

usage()
{
    cat - <<- END

${0} -k <Debian signing key> [-n] [--push]

Create source package for latest stable or beta release and upload to Debian,
and update (and optionally push) changelog file.

    -k <key>    The PGP key used to sign the source package
    -n          Simulate upload
    --push      Push changelog changes to GitHub
    --beta      Deploy beta release. Default: latest stable release.
    --stable    Deploy latest stable release.
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
IS_BETA=false
CURRENT_BRANCH=$(git rev-parse --abbrev-ref HEAD)

while (( $# )); do
    case $1 in
        -k) shift
            if [[ ! $# -eq 0 ]]; then
                DEBKEYID=$1
            else
                builder_die "Error: The -k argument is missing a value. Exiting."
            fi;;
        -n) NOOP=: ;;
        --help) usage ; exit 0 ;;
        --push) PUSH=1 ;;
        --beta) IS_BETA=true ;;
        --stable) IS_BETA=false ;;
        --debian-revision)
            shift
            if [[ ! $# -eq 0 ]]; then
                REVISION=$1
            else
                builder_die "Error: The --debian-revision argument is missing a value. Exiting."
            fi;;
        *) builder_die "Error: Unexpected argument \"$1\". Exiting." ;;
    esac
    shift || builder_die "Error: The last argument is missing a value. Exiting."
done

if [[ -z ${DEBKEYID} ]]; then
    usage
    exit 2
fi

if ! git diff --quiet; then
    builder_die "You have changed files in your git working directory. Exiting."
fi

function get_latest_stable_branch_name() {
  # Get the list of stable branches and take the one with the highest number.
  # Extract `origin/stable-16.0` from `  origin/stable-16.0`
  local branches stable_branch
  branches=$(git branch -r | grep origin/stable-)
  stable_branch=$(echo "${branches}" | sort | tail -1)
  if [[ -z ${stable_branch} ]]; then
    builder_die "Can't find stable-* branch" > /dev/stderr
    exit 2
  fi
  echo "${stable_branch##* }"
}

function push_to_github_and_create_pr() {
  local BRANCH=$1
  local BASE=$2
  local COMMIT_MSG=$3
  local PR_MSG=$4
  local pr_number

  if [[ -n "${PUSH}" ]]; then
      ${NOOP} git push --force origin "${BRANCH}"
      pr_number=$(gh pr list --draft --search "${COMMIT_MSG}" --base "${BASE}" --json number --jq '.[].number')
      if [[ -n ${pr_number} ]]; then
        builder_echo "PR #${pr_number} already exists"
      else
        ${NOOP} gh pr create --draft --base "${BASE}" --title "${PR_MSG}" --body "@keymanapp-test-bot skip"
      fi
  fi
}

builder_heading "Fetching latest changes"
git fetch -p origin

if ${IS_BETA}; then
  DEPLOY_BRANCH=origin/beta
else
  # shellcheck disable=2311
  DEPLOY_BRANCH=$(get_latest_stable_branch_name)
fi

# Checkout stable/beta branch so that `scripts/debian.sh` picks up correct version
git checkout "${DEPLOY_BRANCH#origin/}"
git pull origin "${DEPLOY_BRANCH#origin/}"

builder_heading "Building source package"
cd "${KEYMAN_ROOT}/linux"
DIST=unstable DEBREVISION=${REVISION} scripts/debian.sh
cd debianpackage/
builder_heading "Signing source package"
debsign -k"${DEBKEYID}" --re-sign ./*.changes
builder_heading "Uploading packages to mentors.debian.net"
${NOOP} dput mentors ./*.changes
cd ..

builder_heading "Updating changelog"

# base changelog branch on remote stable/beta branch
git checkout -B chore/linux/changelog "${DEPLOY_BRANCH}"
cp debianpackage/keyman-*/debian/changelog debian/
git add debian/changelog
COMMIT_MESSAGE="chore(linux): Update debian changelog"
git commit -m "${COMMIT_MESSAGE}"
push_to_github_and_create_pr chore/linux/changelog "${DEPLOY_BRANCH#origin/}" "${COMMIT_MESSAGE}" "${COMMIT_MESSAGE}"

# Create cherry-pick on master branch
git checkout -B chore/linux/cherry-pick/changelog origin/master
git cherry-pick -x chore/linux/changelog
push_to_github_and_create_pr chore/linux/cherry-pick/changelog master "${COMMIT_MESSAGE}" "${COMMIT_MESSAGE} 🍒"

builder_heading "Finishing"
git checkout "${CURRENT_BRANCH}"
