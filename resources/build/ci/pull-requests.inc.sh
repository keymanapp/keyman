#
# Commit and upload files as a PR to s.keyman.com, help.keyman.com, or other
# repositories
#

# Defaults for CI pull requests
# TODO: this may belong better somewhere else
if [[ -z ${KEYMAN_PR_USERNAME+x} ]]; then
  KEYMAN_PR_USERNAME="Keyman Build Server"
fi

if [[ -z ${KEYMAN_PR_EMAIL+x} ]]; then
  KEYMAN_PR_EMAIL="keyman-server@users.noreply.github.com"
fi

# Add files to the target repository. Saves and restores PWD
#
# Usage:
#   ci_add_files repo_path pathspec...
# Parameters:
#   1: repo_path    local path to root of the target git repository
#   2: pathspec...  one or more paths to add
# Requires:
#   * git
#
function ci_add_files {
  local repo="$1"
  shift
  pushd "$repo" >/dev/null
  # Git will return an error if the pathspec does not exist, but will not return
  # an error if the pathspec exists but there are no new files to add. This is
  # the behaviour we want.
  git add "$*" || builder_die "git returned error $? when attempting to add $*"
  popd >/dev/null
}

# Test if there are any changes to the git cache in the target repository.
# Used in conjunction with `ci_add_files` to verify that files have or have
# not been added, depending on the scenario
#
# Usage:
#   if ci_repo_has_cached_changes repo_path; then ... fi
# Parameters:
#   1: repo_path    local path to root of the target git repository
# Requires:
#   * git
#
function ci_repo_has_cached_changes {
  local repo="$1"
  local return_code=1
  pushd "$repo" >/dev/null
  if ! git diff --cached --no-ext-diff --quiet --exit-code; then
    return_code=0
  fi
  popd >/dev/null
  return $return_code
}

# Opens a pull request on the target repository, first committing cached files
# to a new branch (with a hopefully-unique branch name). git vars user.name and
# user.email will be configured if not already present. Adds the label 'auto'
# to the pull request.
#
# Usage:
#   ci_open_pull_request repo branch_base commit_message
# Parameters:
#   1: repo_path       local path to root of the target git repository
#   2: branch_base     branch name to use (a unique value `/<unique>` will be
#                      appended)
#   3: commit_message  commit message and pull request title to use
# Requires:
#   * git
#   * hub
#   * uuidgen (or TeamCity `$BUILD_NUMBER` if `uuidgen` not present on system)
# Example:
#   ci_open_pull_request "$S_KEYMAN_COM" auto/keymanweb/release "auto: KeymanWeb release $VERSION_RELEASE""
#
function ci_open_pull_request {
  local repo="$1"
  local branch_base="$2"
  local commit_message="$3"
  pushd "$repo" >/dev/null

  if ! git config user.name > /dev/null; then
    git config user.name "$KEYMAN_PR_USERNAME"
  fi
  if ! git config user.email > /dev/null; then
    git config user.email "$KEYMAN_PR_EMAIL"
  fi

  # We want a unique branch name, so we append either a random or the TeamCity
  # build number
  local uuid=
  if [[ -z ${BUILD_NUMBER+x} ]]; then
    uuid=$(uuidgen)
  else
    uuid=TC-$BUILD_NUMBER
  fi

  local branch="$branch_base/$uuid"
  local current_branch="$(git branch --show-current)"

  builder_echo "Creating new branch '$branch' on '$repo'"
  git switch -c "$branch"
  builder_echo "$commit_message"
  git commit -m "$commit_message"
  git push origin "$branch"
  builder_echo "Push complete"

  hub pull-request --force --message "$commit_message" --labels auto
  builder_echo "Pull request created"

  git switch "$current_branch"
  builder_echo "Switched back to $current_branch"

  popd >/dev/null

  builder_echo "Pull request successfully created"
  return 0
}