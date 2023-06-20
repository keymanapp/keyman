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
# field will be added to it, and @keymanapp dependency versions will also be
# modified. This change should not be committed to the repository.
#
# builder_publish_to_pack and builder_publish_to_npm are similar:
#  * builder_publish_to_npm publishes to the public registry
#  * builder_publish_to_pack creates a local tarball which can be used to test
#
# Usage:
# ```bash
#   builder_publish_to_npm
# ```
#
function builder_publish_to_npm() {
  _builder_publish_npm_package publish
}

function builder_publish_to_pack() {
  _builder_publish_npm_package pack
}

function _builder_publish_npm_package() {
  local action=$1
  local dist_tag=$TIER dry_run=

  if [[ $TIER == stable ]]; then
    dist_tag=latest
  fi

  if builder_has_option --dry-run; then
    dry_run=--dry-run
  fi

  _builder_write_npm_version

  # Note: In either case, npm publish MUST be given --access public to publish a
  # package in the @keymanapp scope on the public npm package index.
  #
  # See `npm help publish` for more details.
  if [[ $action == pack ]]; then
    # We can use --publish-to-pack to locally test a package
    # before publishing to the package registry
    echo "Packing $dry_run npm package $THIS_SCRIPT_IDENTIFIER with tag $dist_tag"
    npm pack $dry_run --access public --tag $dist_tag
  else # $action == publish
    echo "Publishing $dry_run npm package $THIS_SCRIPT_IDENTIFIER with tag $dist_tag"
    npm publish $dry_run --access public --tag $dist_tag
  fi
}

function _builder_write_npm_version() {
  # We use --no-git-tag-version because our CI system controls version numbering
  # and already tags releases. We also want to have the version of this match
  # the release of Keyman Developer -- these two versions should be in sync.
  # Because this is a large repo with multiple projects and build systems, it's
  # better for us that individual build systems don't take too much ownership of
  # git tagging. :)
  if ! "$JQ" -e '.version' package.json > /dev/null; then
    pushd "$KEYMAN_ROOT" > /dev/null
    npm version --allow-same-version --no-git-tag-version --no-commit-hooks --workspaces "$VERSION_WITH_TAG"
    popd > /dev/null
  fi

  # Updates all @keymanapp/* [*]dependencies in all package.jsons to the current
  # version-with-tag, so that the published version has precise dependencies, and
  # we don't accidentally end up with either older or newer deps. This overwrites
  # the local package.json files, so they do need to be restored afterwards
  find "$KEYMAN_ROOT" -name "package.json" -not -path '*/node_modules/*' -print0 | \
    while IFS= read -r -d '' line; do
      cat "$line" | "$JQ" --arg VERSION_WITH_TAG "$VERSION_WITH_TAG" \
        '
          . +
          (try { dependencies: (.dependencies | to_entries | . + map(select(.key | match("@keymanapp/.*")) .value |= $VERSION_WITH_TAG) | from_entries) } catch {}) +
          (try { devDependencies: (.devDependencies | to_entries | . + map(select(.key | match("@keymanapp/.*")) .value |= $VERSION_WITH_TAG) | from_entries) } catch {}) +
          (try { bundleDependencies: (.bundleDependencies | to_entries | . + map(select(.key | match("@keymanapp/.*")) .value |= $VERSION_WITH_TAG) | from_entries) } catch {}) +
          (try { optionalDependencies: (.optionalDependencies | to_entries | . + map(select(.key | match("@keymanapp/.*")) .value |= $VERSION_WITH_TAG) | from_entries) } catch {})
        ' > "${line}_"
      mv -f "${line}_" "$line"
    done
}