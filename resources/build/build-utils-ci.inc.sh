#!/usr/bin/env bash
#
# This script gets CI / pull request details for builds, part of the build-utils
# builder_ suite of functions. All functions and variables in this file have the
# prefix builder_pull_.
#

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

#
# Returns 0 if current build is in CI and triggered from a pull request. If it
# returns 0, then a call is made to GitHub to get pull request details, and the
# PR details are added to $builder_pull_title, $builder_pull_number, and
# the $builder_pull_labels array.
#
# Note that the GitHub REST call is made with credentials if $GITHUB_TOKEN is
# available; without credentials it will be subject to IP-based rate limits.
#
builder_pull_get_details() {
  builder_pull_title=
  builder_pull_number=
  builder_pull_labels=()
  if [[ ! ${TEAMCITY_PR_NUMBER-} =~ ^[0-9]+$ ]]; then
    return 1
  fi

  if [ -z "${GITHUB_TOKEN-}" ]; then
    local pull_data=`curl -s https://api.github.com/repos/keymanapp/keyman/pulls/$TEAMCITY_PR_NUMBER`
  else
    local pull_data=`curl -H "Authorization: Bearer $GITHUB_TOKEN" -s https://api.github.com/repos/keymanapp/keyman/pulls/$TEAMCITY_PR_NUMBER`
  fi

  builder_pull_title=`echo $pull_data | $JQ .title`

  # Simple poor bash test if data returned from API is valid
  if [[ -z builder_pull_title ]]; then
    return 1
  fi

  builder_pull_number=$TEAMCITY_PR_NUMBER
  builder_pull_labels=(`echo $pull_data | $JQ -jr '.labels[].name|.," "'`)

  return 0
}


#
# Returns 0 if the current PR has a particular label. Requires
# builder_pull_get_details to have successfully run first
#
# Usage:
#   builder_pull_has_label "label-name"
# Parameters
#   1: $label       label to test
#
function builder_pull_has_label() {
  local label="$1"
  if [[ " ${builder_pull_labels[*]} " =~ " $label " ]]; then
    return 0
  fi
  return 1
}

#
# Publishes the package in `cwd` to npm
#
# If the `--dry-run` option is available and specified as a command-line
# parameter, will do a dry run
#
# Note that `package.json` will be dirty after this command, as the `version`
# field will be added to it. This change should not be committed to the
# repository.
#
# Usage:
# ```bash
#   builder_publish_to_npm
# ```
#
function builder_publish_to_npm() {
  local dist_tag=$TIER dry_run=

  if [[ $TIER == stable ]]; then
    dist_tag=latest
  fi

  if builder_has_option --dry-run; then
    dry_run=--dry-run
  fi

  # We use --no-git-tag-version because our CI system controls version numbering and
  # already tags releases. We also want to have the version of this match the
  # release of Keyman Developer -- these two versions should be in sync. Because this
  # is a large repo with multiple projects and build systems, it's better for us that
  # individual build systems don't take too much ownership of git tagging. :)
  npm version --allow-same-version --no-git-tag-version --no-commit-hooks "$VERSION_WITH_TAG"

  # Note: In either case, npm publish MUST be given --access public to publish
  # a package in the @keymanapp scope on the public npm package index.
  #
  # See `npm help publish` for more details.
  echo "Publishing $dry_run npm package $THIS_SCRIPT_IDENTIFIER with tag $dist_tag"
  npm publish $dry_run --access public --tag $dist_tag
}
