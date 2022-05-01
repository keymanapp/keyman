#!/usr/bin/env bash

##
## Writes a status check message to GitHub against the reported SHA
## Requires the variable GITHUB_TOKEN to be set to a valid token
##
write_github_status_check() {
  local CONTEXT=$1
  local STATE=$2
  local MESSAGE=$3
  local TARGETURL=$4
  local SHA=$5

  if [ -z $TARGETURL ]; then
    local JSON='{"state": $state, "description": $description, "context": $context}'
  else
    local JSON='{"state": $state, "target_url": $targeturl, "description": $description, "context": $context}'
  fi

  JSON=$($JQ --null-input -c \
    --arg state "$STATE" \
    --arg targeturl "$TARGETURL" \
    --arg description "$MESSAGE" \
    --arg context "$CONTEXT" \
    "$JSON")

  # On Windows, charset encoding is a pain. Simplest
  # solution is to sidestep it with a temp file.

  JSON_TEMPFILE=`mktemp`
  echo "$JSON" > "$JSON_TEMPFILE"

  curl \
    -s -w "%{stderr}%{http_code}%{stdout}" \
    -X POST \
    -H "Authorization: token $GITHUB_TOKEN" \
    -H "Content-Type: application/json; charset=utf-8" \
    -H "Accept: application/vnd.github.v3+json" \
    https://api.github.com/repos/keymanapp/keyman/statuses/$SHA \
    --data-binary @"$JSON_TEMPFILE" > "$JSON_TEMPFILE.out" 2>"$JSON_TEMPFILE.err"

  # Check that the HTTP result code is 2xx

  if ! grep \^2 "$JSON_TEMPFILE.err" > /dev/null; then
    echo "GitHub transaction failed."
    cat "$JSON_TEMPFILE.out"
    EXITCODE=1
  else
    EXITCODE=0
  fi

  rm -f "$JSON_TEMPFILE"
  rm -f "$JSON_TEMPFILE.out"
  rm -f "$JSON_TEMPFILE.err"

  return $EXITCODE
}

##
## Writes a status check message to GitHub against the SHA that the current
## TeamCity build is running on. BUILD_VCS_NUMBER must be set to a valid git SHA.
## Requires the variable GITHUB_TOKEN to be set to a valid token.
##
write_github_status_check_from_teamcity() {
  local CONTEXT=$1
  local STATE=$2
  local MESSAGE=$3
  local TARGETURL=$4
  local SHA=$BUILD_VCS_NUMBER
  report_github_status "$CONTEXT" "$RESULT_STATE" "$RESULT_MESSAGE" "" "$SHA"
}
