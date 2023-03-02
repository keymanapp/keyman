#!/usr/bin/env bash

#
# This script requires builder.inc.sh to already be included
#

function npm_publish() {
  if [[ $TIER == stable ]]; then
    npm_dist_tag=latest
  else
    npm_dist_tag=$TIER
  fi

  set_npm_version

  if builder_has_option --dry-run; then
    DRY_RUN=--dry-run
  else
    DRY_RUN=
  fi

  # Note: In either case, npm publish MUST be given --access public to publish
  # a package in the @keymanapp scope on the public npm package index.
  #
  # See `npm help publish` for more details.
  echo "Publishing $DRY_RUN npm package with tag $npm_dist_tag"
  npm publish $DRY_RUN --access public --tag $npm_dist_tag || builder_die "Could not publish $npm_dist_tag release."
}
