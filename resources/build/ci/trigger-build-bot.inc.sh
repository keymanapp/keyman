# shellcheck shell=bash
# Keyman is copyright (C) SIL Global. MIT License.

# set -eu

# . ./trigger-definitions.inc.sh

#
# Check the Test-bot command in the PR body for commands relevant to the build
# bot, and update global build_platforms accordingly
#
# If the Test-bot command is 'skip', or there are no test bot instructions, then we
# will default to a 'build' for all default build platforms, instead of
# 'release'; this can be overridden by any Build-bot commands in commits or in
# the PR body.
#
# Parameters:
#   1: PR number (not currently used)
#   2: PR JSON data from api.github.com/repos/keymanapp/keyman/pulls/#
#
function test_bot_check_pr_body() {
  local PRNUM=$1
  local prinfo="$2"
  local prbody prTestCommand prTestBody

  set -o noglob
  IFS=$'\n'
  prbody="$(echo "${prinfo}" | "${JQ}" -r '.body')"
  prTestCommand="$(echo "${prbody}" | grep 'Test-bot:' | cut -d: -f 2 - | cut -d' ' -f 1 -)"
  prTestBody="$(echo "${prbody}" | grep -i '# User Testing' || true)"
  unset IFS
  set +o noglob

  if { [[ "${prTestCommand}" == skip ]] || [[ -z "${prTestCommand// }" ]]; } && [[ -z "${prTestBody// }" ]]; then
    local platform
    for platform in "${!build_platforms[@]}"; do
      build_platforms["${platform}"]=build
    done
  fi
}

#
# Check the Keyman-Api-Check command whether or not to run API checks.
# Any value other than 'skip' or no Keyman-Api-Check command will run
# the API checks.
#
# Parameters:
#   1: PR number (not currently used)
#   2: PR JSON data from api.github.com/repos/keymanapp/keyman/pulls/#
#
function is_skip_keyman_api_check() {
  local PRNUM=$1
  local prinfo="$2"
  local prbody prApiCheck

  set -o noglob
  IFS=$'\n'
  prbody="$(echo "${prinfo}" | "${JQ}" -r '.body')"
  prApiCheck="$(echo -e "${prbody}" | grep '^Keyman-Api-Check:' | cut -d: -f 2 - | tr -d '[:space:]')"
  unset IFS
  set +o noglob

  if [[ "${prApiCheck}" == "skip" ]]; then
    echo "true"
  else
    echo "false"
  fi
}

#
# Check PR commit messages for Build-bot commands. Later commands override
# earlier ones. Also checks PR body for any overriding commands. Note, only
# checks the first 250 commits, so later build commands will be ignored; in this
# situation, use the PR body command. Also ignores build commands in PR
# comments, for sake of performance
#
# The bot will also check the Test-bot: trailer in the PR body, if it exists, but
# will not check any other commits. If the Test-bot: command is just 'skip', then
# by default, all builds will be set to 'build' instead of 'release'
#
# Parameters:
#   1: PR number to check
#   2: PR JSON data from api.github.com/repos/keymanapp/keyman/pulls/#
#   3: Commit JSON data from api.github.com/repos/keymanapp/keyman/pulls/#/commits
#
function build_bot_check_messages() {
  local PRNUM=$1
  local prinfo="$2"
  local prcommits="$3"

  # We don't want any globbing happening in our parse

  set -o noglob

  # Extract the Build-bot commands from commit messages and the PR body


  local buildBotShas=($(echo "$prcommits" | "${JQ}" -r '.[].sha'))
  IFS=$'\n'
  local prCommands=($(echo "$prinfo" | "${JQ}" -r '.body' | tr -d '\r' | grep 'Build-bot:' | cut -c 11- -))
  unset IFS

  # The PR body Build-bot comment will be read last, which allows it to override
  # all previous commands

  local buildBotCommand
  local sha
  for sha in "${buildBotShas[@]}"; do

    IFS=$'\n'
    local buildBotCommands=($(echo "$prcommits" | "${JQ}" -r '.[] | select(.sha | contains("'$sha'")) | .commit.message' | grep 'Build-bot:' | cut -c 11- -))
    unset IFS

    for buildBotCommand in "${buildBotCommands[@]}"; do
      buildBotCommand=$(builder_trim "${buildBotCommand}")
      builder_echo heading "Build-bot: Found command in commit ${sha}: '${buildBotCommand}'"

      # Block illegal Build-bot: commands
      if [[ ! "$buildBotCommand" =~ ^[a-z_,\ :,]+$ ]]; then
        builder_echo warning "WARNING[Build-bot]: ignoring invalid command: '${buildBotCommand}'"
        continue
      fi

      # We now know that our command has only a-z, comma, colon, and space, so we
      # can parse without risking escaping our bash jail

      if [[ ! -z "${buildBotCommand// }" ]]; then
        _build_bot_update_commands $buildBotCommand
      fi
    done
  done

  if [[ ${#prCommands[@]} -gt 0 ]]; then
    for buildBotCommand in "${prCommands[@]}"; do
      buildBotCommand=$(builder_trim "${buildBotCommand}")
      builder_echo heading "Build-bot: Found command in body of PR #${PRNUM}: '${buildBotCommand}'"

      # Block illegal Build-bot: commands
      if [[ ! "$buildBotCommand" =~ ^[a-z_\ :,]+$ ]]; then
        builder_echo warning "WARNING[Build-bot]: ignoring invalid command: '${buildBotCommand}'"
        continue
      fi

      # We now know that our command has only a-z, comma, colon, underline, and
      # space, so we can parse without risking escaping our bash jail

      if [[ ! -z "${buildBotCommand// }" ]]; then
        _build_bot_update_commands $buildBotCommand
      fi
    done

  fi

  set +o noglob
}

#
# parses a 'Build-bot: <level>[[:| ]platform,...]' command and modifies
# the global build_platforms associative array with new levels.
#
# Note that this function assumes that inputs are sanitized, see
# build_bot_check_messages
#
function _build_bot_update_commands() {
  local level=
  local platforms=
  local command="$*"

  local re='^(build|skip|release)( [a-z,]+)?$'
  if [[ "${command}" =~ ${re} ]]; then
    # legacy (until aug 2025) format is "level [platform]" (comma format never used)
    if [[ $# == 1 ]]; then
      level=$1
      read -r -a platforms <<< "${!build_platforms[@]}"
    else
      level=$1
      shift

      # remaining parameters are comma/space separated platforms
      IFS=','
      read -r -a platforms <<< "$*"
      unset IFS
    fi

    if [[ ! ${level} =~ ^${valid_build_levels}$ ]]; then
      # Just skip this build command
      builder_echo warning "WARNING[Build-bot]: ignoring invalid build level '${level}' in command '${command}'"
      return 0
    fi

    builder_echo grey "Build-bot: Platforms to be updated from command '${command}' are: ${platforms[*]}"

    _build_bot_verify_platforms platforms

    local platform
    for platform in "${platforms[@]}"; do
      builder_echo "Build-bot: Updating build level for ${platform} to ${level}"
      build_platforms["${platform}"]=${level}
    done
  else
    # modern format is "level[:platform[,platform...]][ level[:platform[,platform...]]...]"
    declare -a commands
    IFS=' '
    read -r -a commands <<< "${command}"
    unset IFS

    for command in "${commands[@]}"; do
      declare -a params
      IFS=:
      read -r -a params <<< "${command}"
      unset IFS
      level=${params[0]}
      if [[ ! ${level} =~ ^${valid_build_levels}$ ]]; then
        # Just skip this build command
        builder_echo warning "WARNING[Build-bot]: ignoring invalid build level '${level}' in command '${command}'"
        continue
      fi

      if [[ ${#params[@]} == 1 ]]; then
        read -r -a platforms <<< "${!build_platforms[@]}"
      else
        # remaining parameters are comma separated platforms
        IFS=','
        read -r -a platforms <<< "${params[1]}"
        unset IFS
      fi

      _build_bot_verify_platforms platforms

      local platform
      for platform in "${platforms[@]}"; do
        if [[ "${build_platforms[${platform}]+x}" != "${level}" ]]; then
          builder_echo "Build-bot: Updating build level for ${platform} to ${level}"
          build_platforms["${platform}"]=${level}
        fi
      done
    done
  fi
}

#
# Strip any unrecognized platforms and expand 'all' to actual platforms
#
# Parameters:
#   1: name of platforms array parameter (byref)
#
function _build_bot_verify_platforms() {
  local -n input_platforms=$1
  local output_platforms=()
  local platform
  for platform in "${input_platforms[@]}"; do
    # We'll emit a warning with invalid platforms, then remove them from the array
    if [[ ! ${platform} =~ ^(all|${available_platforms_regex})$ ]]; then
      builder_echo warning "WARNING[Build-bot]: ignoring invalid platform '${platform}'"
    elif [[ ${platform} == all ]]; then
      input_platforms=("${available_platforms[@]}")
      return
    else
      if [[ ! "${output_platforms[@]}" =~ [[:\<:]]${platform}[[:\>:]] ]]; then
        output_platforms+=("${platform}")
      fi
    fi
  done
  input_platforms=("${output_platforms[@]}")
}
