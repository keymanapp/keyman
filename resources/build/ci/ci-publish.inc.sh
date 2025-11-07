# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.
#
# This script gets CI / pull request details for builds, part of the build-utils
# builder_ suite of functions. All functions and variables in this file have the
# prefix builder_pull_.
#

. "$KEYMAN_ROOT/resources/build/jq.inc.sh"

# Used only by npm-publish.sh
function ci_publish_npm_package() {
  local action="$1"
  local package="$2"
  local dist_tag=$KEYMAN_TIER dry_run=

  if [[ $KEYMAN_TIER == stable ]]; then
    dist_tag=latest
  fi

  if builder_has_option --dry-run; then
    dry_run=--dry-run
  fi

  pushd "${KEYMAN_ROOT}/${package}" >/dev/null

  _ci_publish_cache_package_json
  _ci_write_npm_version
  _ci_prepublish

  # Note: In either case, npm publish MUST be given --access public to publish a
  # package in the @keymanapp scope on the public npm package index.
  #
  # See `npm help publish` for more details.
  if [[ $action == pack ]]; then
    # We can use --publish-to-pack to locally test a package
    # before publishing to the package registry
    echo "Packing $dry_run npm package ${package} with tag $dist_tag"
    npm pack $dry_run --access public --tag $dist_tag
  else # $action == publish
    echo "Publishing $dry_run npm package ${package} with tag $dist_tag"
    npm publish $dry_run --access public --tag $dist_tag
  fi

  popd >/dev/null
}

function _ci_write_npm_version() {
  # We use --no-git-tag-version because our CI system controls version numbering
  # and already tags releases. We also want to have the version of this match
  # the release of Keyman Developer -- these two versions should be in sync.
  # Because this is a large repo with multiple projects and build systems, it's
  # better for us that individual build systems don't take too much ownership of
  # git tagging. :)
  if ! "$JQ" -e '.version' package.json > /dev/null; then
    pushd "$KEYMAN_ROOT" > /dev/null
    npm version --allow-same-version --no-git-tag-version --no-commit-hooks --workspaces "$KEYMAN_VERSION_WITH_TAG"
    popd > /dev/null
  fi

  # Updates all @keymanapp/* [*]dependencies in all package.jsons to the current
  # version-with-tag, so that the published version has precise dependencies, and
  # we don't accidentally end up with either older or newer deps. This overwrites
  # the local package.json files, so they do need to be restored afterwards
  find "$KEYMAN_ROOT" -name "package.json" -not -path '*/node_modules/*' -print0 | \
    while IFS= read -r -d '' line; do
      cat "$line" | "$JQ" --arg KEYMAN_VERSION_WITH_TAG "$KEYMAN_VERSION_WITH_TAG" \
        '
          . +
          (try { dependencies: (.dependencies | to_entries | . + map(select(.key | match("@keymanapp/.*")) .value |= $KEYMAN_VERSION_WITH_TAG) | from_entries) } catch {}) +
          (try { devDependencies: (.devDependencies | to_entries | . + map(select(.key | match("@keymanapp/.*")) .value |= $KEYMAN_VERSION_WITH_TAG) | from_entries) } catch {}) +
          (try { optionalDependencies: (.optionalDependencies | to_entries | . + map(select(.key | match("@keymanapp/.*")) .value |= $KEYMAN_VERSION_WITH_TAG) | from_entries) } catch {})
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
function _ci_prepublish() {
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
        if builder_is_windows; then
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
function _ci_publish_cache_package_json() {
  if [[ -f "$KEYMAN_ROOT/builder_package_publish.json" ]]; then
    return 0
  fi

  if "$JQ" -e '.version' "$KEYMAN_ROOT/developer/src/kmc/package.json" > /dev/null; then
    builder_die "npm version has already been run. Revert the version changes to all package.json files before re-running"
  fi

  cp  "$KEYMAN_ROOT/package.json" "$KEYMAN_ROOT/builder_package_publish.json"
}

function ci_publish_cleanup() {
  rm -f "$KEYMAN_ROOT/builder_package_publish.json"
}
