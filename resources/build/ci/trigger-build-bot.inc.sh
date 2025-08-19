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

  set -o noglob
  IFS=$'\n'
  local prbody="$(echo "$prinfo" | "${JQ}" -r '.body')"
  local prTestCommand="$(echo "$prbody" | grep 'Test-bot:' | cut -d: -f 2 - | cut -d' ' -f 1 -)"
  local prTestBody="$(echo "$prbody" | grep -i '# User Testing')"
  unset IFS
  set +o noglob

  if ([[ "$prTestCommand" == skip ]] || [[ -z "${prTestCommand// }" ]]) && [[ -z "${prTestBody// }" ]]; then
    local platform
    for platform in "${!build_platforms[@]}"; do
      build_platforms[$platform]=build
    done
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


  IFS=$'\n'
  local buildBotCommands=($(echo "$prcommits" | "${JQ}" -r '.[].commit.message' | grep 'Build-bot:' | cut -c 11- -))
  local prCommands=($(echo "$prinfo" | "${JQ}" -r '.body' | tr -d '\r' | grep 'Build-bot:' | cut -c 11- -))
  unset IFS

  # The PR body Build-bot comment will be read last, which allows it to override
  # all previous commands

  if [[ ${#prCommands[@]} -gt 0 ]]; then
    buildBotCommands+=("${prCommands[@]}")
  fi

  for buildBotCommand in "${buildBotCommands[@]}"; do
    # Block illegal Build-bot: commands
    if [[ ! "$buildBotCommand" =~ ^[a-z,\ :]+$ ]]; then
      builder_echo warning "WARNING[Build-bot]: ignoring invalid command: '${buildBotCommand}'"
      continue
    fi

    # debug_echo "buildBotCommand:{$buildBotCommand}"

    # We now know that our command has only a-z, comma, colon, and space, so we
    # can parse without risking escaping our bash jail

    if [[ ! -z "${buildBotCommand// }" ]]; then
      build_bot_update_commands $buildBotCommand
    fi
  done

  set +o noglob
}

#
# parses a 'Build-bot: <level>[[:| ]platform,...]' command and modifies
# the global build_platforms associative array with new levels.
#
# Note that this function assumes that inputs are sanitized, see
# build_bot_check_messages
#
function build_bot_update_commands() {
  local level=
  local platforms=
  local command="$*"

  local re='^(build|skip|release)( [a-z,]+)?$'
  if [[ "$command" =~ $re ]]; then
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

    if [[ ! $level =~ ^$valid_build_levels$ ]]; then
      # Just skip this build command
      builder_echo warning "WARNING[Build-bot]: ignoring invalid build level '$level' in command '$command'"
      return 0
    fi

    builder_echo blue "Platforms to be updated from command '$command' are: ${platforms[@]}"

    build_bot_verify_platforms platforms

    local platform
    for platform in "${platforms[@]}"; do
      builder_echo "Build-bot: Updating build level for $platform to $level"
      build_platforms[$platform]=$level
    done
  else
    # modern format is "level[:platform[,platform...]][ level[:platform[,platform...]]...]"
    declare -a commands
    IFS=' '
    read -r -a commands <<< "$command"
    unset IFS

    for command in "${commands[@]}"; do
      declare -a params
      IFS=:
      read -r -a params <<< "$command"
      level=${params[0]}
      if [[ ! $level =~ ^$valid_build_levels$ ]]; then
        # Just skip this build command
        builder_echo warning "WARNING[Build-bot]: ignoring invalid build level '$level' in command '$command'"
        continue
      fi

      if [[ ${#params[@]} == 1 ]]; then
        platforms="${!build_platforms[@]}"
      else
        # remaining parameters are comma separated platforms
        IFS=','
        read -r -a platforms <<< "${params[1]}"
        unset IFS
      fi

      build_bot_verify_platforms platforms

      local platform
      for platform in "${platforms[@]}"; do
        if [[ "${build_platforms[$platform]}" != "${level}" ]]; then
          builder_echo "Build-bot: Updating build level for $platform to $level"
          build_platforms[$platform]=$level
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
function build_bot_verify_platforms() {
  local -n input_platforms=$1
  local output_platforms=()
  local platform
  for platform in "${input_platforms[@]}"; do
    # We'll emit a warning with invalid platforms, then remove them from the array
    if [[ ! $platform =~ ^(all|$available_platforms_regex)$ ]]; then
      builder_echo warning "WARNING[Build-bot]: ignoring invalid platform '$platform'"
    elif [[ $platform == all ]]; then
      input_platforms=(${available_platforms[@]})
      return
    else
      if [[ ! "${output_platforms[@]}" =~ [[:\<:]]$platform[[:\>:]] ]]; then
        output_platforms+=($platform)
      fi
    fi
  done
  input_platforms=("${output_platforms[@]}")
}
