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
# If --npm-publish is set:
#  * then builder_publish_npm publishes to the public registry
#  * else builder_publish_npm creates a local tarball which can be used to test
#
# Usage:
# ```bash
#   builder_publish_npm
# ```
#
function builder_publish_npm() {
  if builder_has_option --npm-publish; then
    # Require --dry-run if local or test to avoid accidentally clobbering npm packages
    if [[ $VERSION_ENVIRONMENT =~ local|test ]] && ! builder_has_option --dry-run; then
      builder_die "publish --npm-publish must use --dry-run flag for local or test builds"
    fi
    _builder_publish_npm_package publish
  else
    _builder_publish_npm_package pack
  fi
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

  _builder_publish_cache_package_json
  _builder_write_npm_version
  _builder_prepublish

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
          (try { optionalDependencies: (.optionalDependencies | to_entries | . + map(select(.key | match("@keymanapp/.*")) .value |= $VERSION_WITH_TAG) | from_entries) } catch {})
        ' > "${line}_"
      mv -f "${line}_" "$line"
    done
}

#
# Due to https://github.com/npm/cli/issues/3466, we manually create all
# bundleDependencies (__NOT__ bundledDependencies, beware typos) from
# the target's package.json in its node_modules folder. Must run from
# the target's folder.
#
function _builder_prepublish() {
  mkdir -p node_modules/@keymanapp
  local packages=($(cat package.json | "$JQ" --raw-output '.bundleDependencies | join(" ")'))
  local package

  # For each @keymanapp/ package, we'll do a local symlink, note that Windows
  # mklink is internal to cmd!
  for package in "${packages[@]}"; do
    if [[ $package =~ ^@keymanapp/ ]]; then
      # Creating local symlink under node_modules
      local link_source=node_modules/$package

      # lookup the link_target from top-level package.json/dependencies
      local link_target="$(cat "$KEYMAN_ROOT/builder_package_publish.json" | "$JQ" -r .dependencies.\"$package\")"

      if [[ $link_target =~ ^file: ]]; then
        link_target="$KEYMAN_ROOT"/${link_target#file:}

        builder_echo "Manually linking $link_source -> $link_target (see https://github.com/npm/cli/issues/3466)"
        rm -rf $link_source
        if [[ $BUILDER_OS == win ]]; then
          link_source="$(cygpath -w "$link_source")"
          link_target="$(cygpath -w "$link_target")"
          cmd //c mklink //j "$link_source" "$link_target"
        else
          ln -sr "$link_target" "$link_source"
        fi
      fi
    fi
  done
}

#
# We need to cache /package.json before npm version gets its sticky fingers on
# it, because afterwards, we lose the file: paths that help us to resolve
# dependencies easily. Part of the https://github.com/npm/cli/issues/3466
# workaround.
#
function _builder_publish_cache_package_json() {
  if [[ -f "$KEYMAN_ROOT/builder_package_publish.json" ]]; then
    return 0
  fi

  if "$JQ" -e '.version' "$KEYMAN_ROOT/developer/src/kmc/package.json" > /dev/null; then
    builder_die "npm version has already been run. Revert the version changes to all package.json files before re-running"
  fi

  cp  "$KEYMAN_ROOT/package.json" "$KEYMAN_ROOT/builder_package_publish.json"
}

function builder_publish_cleanup() {
  rm -f "$KEYMAN_ROOT/builder_package_publish.json"
}