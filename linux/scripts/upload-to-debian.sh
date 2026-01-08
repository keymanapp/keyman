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
. "${THIS_SCRIPT%/*}/../../resources/build/builder-basic.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/build/utils.inc.sh"

NOOP=
PUSH=
DEBKEYID=
REVISION=1
IS_BETA=false

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

# Push the changelog changes to GitHub and create a PR. Returns the PR#
# in the environment variable PR_NUMBER.
function push_to_github_and_create_pr() {
  local BRANCH=$1           # `chore/linux/changelog` or `chore/linux/cherry-pick/changelog`
  local BASE=$2             # stable branch, `beta` or `master`
  local PR_TITLE=$3         # `Update debian changelog`
  local PR_BODY=$4          # `@keymanapp-test-bot skip`

  if [[ -n "${PUSH}" ]]; then
    # Push to origin. We force push to reset the branch the commit we just made.
    # There shouldn't be any other commits on ${BRANCH} except the one we want to replace
    # (if any).
    ${NOOP} git push --force origin "${BRANCH}"
    PR_NUMBER=$(gh pr list --draft --search "${PR_TITLE}" --base "${BASE}" --json number --jq '.[].number')
    if [[ -n ${PR_NUMBER} ]]; then
      builder_echo "PR #${PR_NUMBER} already exists"
    else
      local PR_URL
      PR_URL=$(gh pr create --draft --base "${BASE}" --title "${PR_TITLE}" --body "${PR_BODY}")
      PR_NUMBER="${PR_URL##*/}"
    fi
  else
    PR_NUMBER=""
  fi
}

function cleanup_worktree() {
  if [[ -d "${WORKTREE_DIR}/linux/debianpackage" ]]; then
    cp -r "${WORKTREE_DIR}/linux/debianpackage" "${KEYMAN_ROOT}/linux"
  fi

  cd "${PREV_DIR}"

  if [[ -n "${WORKTREE_DIR}" ]] && [[ -d "${WORKTREE_DIR}" ]]; then
    builder_echo "Removing temporary worktree"
    git worktree remove -f "${WORKTREE_DIR}"
    git branch -D "${WORKTREE_BRANCH}"
  fi

  local TEMP_DIR
  TEMP_DIR=$(dirname "${WORKTREE_DIR}")
  rm -rf "${TEMP_DIR}"
}

builder_heading "Fetching latest changes"
git fetch -p origin

if ${IS_BETA}; then
  ORIGIN_DEPLOY_BRANCH=origin/beta
else
  # shellcheck disable=2311
  ORIGIN_DEPLOY_BRANCH=$(get_latest_stable_branch_name)
fi

DEPLOY_BRANCH="${ORIGIN_DEPLOY_BRANCH#origin/}"
BRANCH_BASENAME="tmp-packaging-${DEPLOY_BRANCH}"
WORKTREE_BRANCH="maint/linux/${BRANCH_BASENAME}"
PREV_DIR="${PWD}"

# cleanup any residues of previous runs
WORKTREE_DIR="$(git worktree list --porcelain | awk -v name="${BRANCH_BASENAME}" \
  '$1 == "worktree" && $2 ~ name { print $2 }')"
if [[ -n "${WORKTREE_DIR}" ]] && [[ -d "${WORKTREE_DIR}" ]]; then
  git worktree remove -f "${WORKTREE_DIR}"
fi
if git branch | grep -q "${WORKTREE_BRANCH}"; then
  git branch -D "${WORKTREE_BRANCH}"
fi

# Create a temporary worktree so that we start with a clean copy of the
# target branch (stable/beta). Checkout stable/beta branch so that
# scripts/debian.sh picks up correct version
WORKTREE_DIR="$(mktemp -d)/${BRANCH_BASENAME}"
builder_heading "Creating temporary worktree at ${WORKTREE_DIR}"
git worktree add -f -b "${WORKTREE_BRANCH}" "${WORKTREE_DIR}" "${DEPLOY_BRANCH}"

cd "${WORKTREE_DIR}"

trap cleanup_worktree EXIT

git pull origin "${DEPLOY_BRANCH}"

builder_heading "Building source package"
cd "${WORKTREE_DIR}/linux"
DIST=unstable DEBREVISION=${REVISION} scripts/debian.sh
cd debianpackage/
builder_heading "Signing source package"
debsign -k"${DEBKEYID}" --re-sign ./*.changes
builder_heading "Uploading packages to mentors.debian.net"
${NOOP} dput mentors ./*.changes
cd ..

builder_heading "Updating changelog"

# base changelog branch on remote stable/beta branch
git checkout -B chore/linux/changelog "${ORIGIN_DEPLOY_BRANCH}"
cp debianpackage/keyman-*/debian/changelog debian/
git add debian/changelog
COMMIT_MESSAGE="chore(linux): Update debian changelog"
git commit -m "${COMMIT_MESSAGE}"
push_to_github_and_create_pr chore/linux/changelog "${DEPLOY_BRANCH}" "${COMMIT_MESSAGE} 🏠" "Build-bot: skip
Test-bot: skip"

# Create cherry-pick on master branch
git checkout -B chore/linux/cherry-pick/changelog origin/master
git cherry-pick -x chore/linux/changelog
push_to_github_and_create_pr chore/linux/cherry-pick/changelog master "${COMMIT_MESSAGE} 🍒" \
  "Cherry-pick-of: #${PR_NUMBER}
Build-bot: skip
Test-bot: skip"

builder_heading "Finishing"
cleanup_worktree
