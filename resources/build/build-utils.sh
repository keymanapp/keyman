#!/usr/bin/env bash
#
# This script sets build environment variables. VERSION vars are also exported:
#   VERSION:          Full current build version, e.g. "14.0.1"
#   VERSION_WIN:      Full current build version for Windows, e.g. "14.0.1.0"
#   VERSION_RELEASE:  Current release version, e.g. "14.0"
#   VERSION_MAJOR:    Major version, e.g. "14"
#   VERSION_MINOR:    Minor version, e.g. "0"
#   VERSION_PATCH:    Patch version, e.g. "1"
#   TIER:             Current tier, one of "alpha", "beta" or "stable"
#   VERSION_TAG:      Tier + Pull Request + Location of build [-alpha|-beta][-test[-1234]][-local]
#   VERSION_WITH_TAG: e.g. "14.0.1-alpha-test-1234" or "14.0.5-beta-local" or "14.0.1-alpha-test"
#   KEYMAN_ROOT:      fully resolved root path of Keyman repository
#   VERSION_ENVIRONMENT: One of: local, test, alpha, beta, stable
#   UPLOAD_SENTRY:    true - if VERSION_ENVIRONMENT is one of alpha, beta, stable
#                     false - if local, test.  Indicates if debug artifacts should be uploaded to Sentry
#
# On macOS, this script requires coreutils (`brew install coreutils`)
#
# Here is how to include this script reliably, cross-platform:
#    ## START STANDARD BUILD SCRIPT INCLUDE
#    # adjust relative paths as necessary
#    THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
#    . "$(dirname "$THIS_SCRIPT")/../resources/build/build-utils.sh"
#    # END STANDARD BUILD SCRIPT INCLUDE
#
# Note: keep changes to version, tier and tag determination in sync with mkver (windows/src/buildutils/mkver)
#

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

# Setup variable for calling script's path and name
if [ ! -z ${THIS_SCRIPT+x} ]; then
  THIS_SCRIPT_PATH="$(dirname "$THIS_SCRIPT")"
  readonly THIS_SCRIPT_PATH
  THIS_SCRIPT_NAME="$(basename "$THIS_SCRIPT")"
  readonly THIS_SCRIPT_NAME
fi

function die () {
    # TODO: consolidate this with fail() from shellHelperFunctions.sh
    echo
    echo "$*"
    echo
    exit 1
}

function findRepositoryRoot() {
    # See https://stackoverflow.com/questions/59895/how-to-get-the-source-directory-of-a-bash-script-from-within-the-script-itself
    # None of the answers are 100% correct for cross-platform
    # On macOS, requires coreutils (`brew install coreutils`)
    local SCRIPT=$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")
    KEYMAN_ROOT=$(dirname $(dirname $(dirname "$SCRIPT")))
    readonly KEYMAN_ROOT
}

function findVersion() {
    local VERSION_MD="$KEYMAN_ROOT/VERSION.md"
    VERSION=`cat $VERSION_MD | tr -d "[:space:]"`
    [[ "$VERSION" =~ ^([[:digit:]]+)\.([[:digit:]]+)\.([[:digit:]]+)$ ]] && {
        VERSION_MAJOR="${BASH_REMATCH[1]}"
        VERSION_MINOR="${BASH_REMATCH[2]}"
        VERSION_PATCH="${BASH_REMATCH[3]}"
        VERSION_RELEASE="$VERSION_MAJOR.$VERSION_MINOR"
    } || {
        echo "Invalid VERSION.md file: expected major.minor.patch";
        exit 1;
    }

    # Used for Windows, which requires a four part version string
    VERSION_WIN="$VERSION.0"

    #
    # Build a tag to append to the version string. This is not assigned
    # to the version number used in the projects but may be used as a
    # display string and in TeamCity configuration
    #

    if [ "$TIER" == "alpha" ] || [ "$TIER" == "beta" ]; then
        VERSION_TAG="-$TIER"
    else
        VERSION_TAG=
    fi

    if [ -z "${TEAMCITY_VERSION-}" -a -z "${JENKINS_HOME-}" ]; then
        # Local dev machine, not TeamCity
        VERSION_TAG="$VERSION_TAG-local"
        VERSION_ENVIRONMENT=local
    else
        # On TeamCity: are we running a pull request build or a master/beta/stable build?
        if [ ! -z "${TEAMCITY_PR_NUMBER-}" ]; then
            VERSION_ENVIRONMENT=test
            # Note TEAMCITY_PR_NUMBER can also be 'master', 'beta', or 'stable-x.y'
            # This indicates we are running a Test build.
            if [[ $TEAMCITY_PR_NUMBER =~ ^(master|beta|stable(-[0-9]+\.[0-9]+)?)$ ]]; then
                VERSION_TAG="$VERSION_TAG-test"
            else
                VERSION_TAG="$VERSION_TAG-test-$TEAMCITY_PR_NUMBER"
            fi
        else
            VERSION_ENVIRONMENT="$TIER"
        fi
    fi

    VERSION_WITH_TAG="$VERSION$VERSION_TAG"

    readonly VERSION
    readonly VERSION_MAJOR
    readonly VERSION_MINOR
    readonly VERSION_PATCH
    readonly VERSION_RELEASE
    readonly VERSION_WIN
    readonly VERSION_TAG
    readonly VERSION_WITH_TAG
    readonly VERSION_ENVIRONMENT

    # Export version strings so places like version.gradle can access them
    export VERSION
    export VERSION_MAJOR
    export VERSION_MINOR
    export VERSION_PATCH
    export VERSION_RELEASE
    export VERSION_WIN
    export VERSION_TAG
    export VERSION_WITH_TAG
    export VERSION_ENVIRONMENT
}

function findTier() {
    local TIER_MD="$KEYMAN_ROOT/TIER.md"
    TIER=`cat $TIER_MD | tr -d "[:space:]"`
    [[ "$TIER" =~ ^(alpha|beta|stable)$ ]] || {
        echo "Invalid TIER.md file: expected alpha, beta or stable."
        exit 1;
    }
}

function printBuildNumberForTeamCity() {
    if [ ! -z "${TEAMCITY_VERSION-}" ]; then
        if [ ! -z "${TEAMCITY_PR_NUMBER-}" ]; then
            echo "##teamcity[buildNumber '$VERSION_WITH_TAG']"
        else
            # For alpha/beta builds, for now we don't append the
            # version tag as buildNumber is used in the delivery
            # of the build version. We may improve this in the
            # future.
            echo "##teamcity[buildNumber '$VERSION']"
        fi
    fi
}

function printVersionUtilsDebug() {
    echo "KEYMAN_ROOT:         $KEYMAN_ROOT"
    echo "VERSION:             $VERSION"
    echo "VERSION_WIN:         $VERSION_WIN"
    echo "VERSION_RELEASE:     $VERSION_RELEASE"
    echo "VERSION_MAJOR:       $VERSION_MAJOR"
    echo "VERSION_MINOR:       $VERSION_MINOR"
    echo "VERSION_PATCH:       $VERSION_PATCH"
    echo "TIER:                $TIER"
    echo "VERSION_TAG:         $VERSION_TAG"
    echo "VERSION_WITH_TAG:    $VERSION_WITH_TAG"
    echo "VERSION_ENVIRONMENT: $VERSION_ENVIRONMENT"
}

function findShouldSentryRelease() {
    # Default, for 'test' or 'local' environment, or in case $VERSION_ENVIRONMENT is improperly specified.
    # (May be overridden by -upload-sentry in supporting build scripts.)
    UPLOAD_SENTRY=false

    # Default: override to `true` for release builds.
    case $VERSION_ENVIRONMENT in
    # Actual release tiers
    alpha | beta | stable)
        UPLOAD_SENTRY=true
        ;;
    esac
}

findRepositoryRoot
findTier
findVersion
# printVersionUtilsDebug
printBuildNumberForTeamCity

findShouldSentryRelease

# Intended for use with macOS-based builds, as Xcode build phase "run script"s do not have access to important
# environment variables.  Doesn't hurt to run it at other times as well.  The output file is .gitignore'd.
function exportEnvironmentDefinitionScript() {
    ENVIRONMENT_SH="$KEYMAN_ROOT/resources/environment.sh"

    # Remove old copy if it exists
    [ -f "$ENVIRONMENT_SH" ] && rm "$ENVIRONMENT_SH"

    # Documentation about the script, within the script.
    echo "# Do not edit - this is an autogenerated script.  See build/build-utils.sh for more details." >> "$ENVIRONMENT_SH"
    echo "# This file redefines critical environment variables for import to Xcode build phases." >> "$ENVIRONMENT_SH"
    echo "" >> "$ENVIRONMENT_SH"

    # Defining variables for VERSION here will leave static definitions that don't automatically update when a user
    # changes branches; some branches are 'similar enough' to not require full command-line based rebuilds.
    # We want that VERSION number to properly mirror the state of its branch during development so that it matches
    # any error reports that get logged to Sentry.
    #
    # As a result, we explicitly do NOT define VERSION or VERSION_TAG as part of ENVIRONMENT_SH.

    echo "# Required for successful dSYM upload for Sentry error reporting" >> "$ENVIRONMENT_SH"
    echo "export SENTRY_AUTH_TOKEN=${SENTRY_AUTH_TOKEN:-}" >> "$ENVIRONMENT_SH"
    echo "export SENTRY_URL=${SENTRY_URL:-}" >> "$ENVIRONMENT_SH"
    echo "export SENTRY_ORG=${SENTRY_ORG:-}" >> "$ENVIRONMENT_SH"

    # Ensure the autogenerated file may be successfully run/included by xcode-utils.sh.
    chmod +x "$ENVIRONMENT_SH"
}

# Detect if this script is running from within Xcode.  Obviously, this assumes we don't have other definitions
# for these variables... but they're set within Xcode during its runs.  As a result, they're not the wisest thing for
# someone else to intentionally use, so this check seems reasonable.
#
# https://gist.github.com/gdavis/6670468 has a representative copy of a standard Xcode environment variable setup.
if [[ -z "${XCODE_VERSION_ACTUAL:-}" ]] && [[ -z "${XCODE_PRODUCT_BUILD_VERSION:-}" ]]; then
    exportEnvironmentDefinitionScript
fi


replaceVersionStrings() {
  local infile=$1
  local outfile=$2

  sed "
    s/\$VERSION_WIN/$VERSION_WIN/g;
    s/\$VERSION_RELEASE/$VERSION_RELEASE/g;
    s/\$VERSION_MAJOR/$VERSION_MAJOR/g;
    s/\$VERSION_MINOR/$VERSION_MINOR/g;
    s/\$VERSION_PATCH/$VERSION_PATCH/g;
    s/\$TIER/$TIER/g;
    s/\$VERSION_TAG/$VERSION_TAG/g;
    s/\$VERSION_WITH_TAG/$VERSION_WITH_TAG/g;
    s/\$VERSION_ENVIRONMENT/$VERSION_ENVIRONMENT/g;
    s/\$VERSION/$VERSION/g;
    " "$infile" > "$outfile"
}

replaceVersionStrings_Mkver() {
  # This is similar to replaceVersionStrings but supports all
  # the old mkver strings as used by windows/src
  local infile=$1
  local outfile=$2

  sed "
    s/\$VersionWin/$VERSION_WIN/g;
    s/\$VersionRelease/$VERSION_RELEASE/g;
    s/\$VersionMajor/$VERSION_MAJOR/g;
    s/\$VersionMinor/$VERSION_MINOR/g;
    s/\$VersionPatch/$VERSION_PATCH/g;
    s/\$Tier/$TIER/g;
    s/\$Tag/$VERSION_TAG/g;
    s/\$VersionWithTag/$VERSION_WITH_TAG/g;
    s/\$VersionRc/$VERSION_MAJOR,$VERSION_MINOR,$VERSION_PATCH,0/g;
    s/\$Environment/$VERSION_ENVIRONMENT/g;
    s/\$Version/$VERSION/g;
    s/\$VERSIONNUM/$VERSION_MAJOR,$VERSION_MINOR,$VERSION_PATCH,0/g;
    s/\$VERSION/$VERSION_WIN/g;
    s/\$RELEASE_MAJOR/$VERSION_MAJOR/g;
    s/\$RELEASE_MINOR/$VERSION_MINOR/g;
    s/\$RELEASE/$VERSION_RELEASE/g;
    " "$infile" > "$outfile"
}

################################################################################
# Standard build script functions for managing command line, actions and targets
################################################################################

# The following allows coloring of warning and error lines, but only works if there's a
# terminal attached, so not on the build machine.

# Overrides default colorization of logging; can be used in command-line with
# --color or --no-color, or overridden as necessary on a per-script basis.
#
# Parameters
#  1: use_color       true or false
builder_use_color() {
  if $1; then
    COLOR_RED=$(tput setaf 1)
    COLOR_GREEN=$(tput setaf 2)
    COLOR_YELLOW=$(tput setaf 3)
    COLOR_BLUE=$(tput setaf 4)
    COLOR_PURPLE=$(tput setaf 5)
    COLOR_TEAL=$(tput setaf 6)
    COLOR_WHITE=$(tput setaf 7)
    COLOR_GREY=$(tput setaf 8)
    COLOR_RESET=$(tput sgr0)
    # e.g. VSCode https://code.visualstudio.com/updates/v1_69#_setmark-sequence-support
    HEADING_SETMARK='\x1b]1337;SetMark\x07'
  else
    COLOR_RED=
    COLOR_GREEN=
    COLOR_YELLOW=
    COLOR_BLUE=
    COLOR_PURPLE=
    COLOR_TEAL=
    COLOR_WHITE=
    COLOR_GREY=
    COLOR_RESET=
    HEADING_SETMARK=
  fi
}

if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]] && [[ "$TERM" != "unknown" ]]; then
  builder_use_color true
else
  builder_use_color false
fi

####################################################################################
#
# builder_ functions for standard build script parameter and process management
#
####################################################################################

#
# builder_ names are reserved.
# _builder_ names are internal use and subject to change
#
_builder_debug=false

#
# builder_extra_params: string containing all parameters after '--'
#
builder_extra_params=()

# returns 0 if first parameter is in the array passed as second parameter
#
# Usage:
#   if _builder_item_in_array "item" "${array[@]}"; then ...; fi
# Parameters:
#   1: item       item to search for in array
#   2: array      bash array, e.g. array=(one two three)
_builder_item_in_array() {
  local e match="$1"
  shift
  [[ -z "$match" ]] && return 1
  for e; do [[ "$e" == "$match" ]] && return 0; done
  return 1
}

_builder_item_is_target() {
  local item="$1"
  [[ $item =~ ^: ]] && return 1
  return 0
}

#
# Returns 0 if the user has asked to perform action on target on the command line
#
# Usage:
#   if build_has_action action[:target]; then ...; fi
# Parameters:
#   1: action    name of action
#   2: :target    name of target, :-prefixed, as part of first param or space separated ok
# Example:
#   if build_has_action build :app; then
#   if build_has_action build:app; then
#
builder_has_action() {
  local action="$1" target

  if [[ $action =~ : ]]; then
    IFS=: read -r action target <<< $action
    target=:$target
  elif [[ -z ${2+x} ]]; then
    target=:project
  else
    target="$2"
  fi

  if _builder_item_in_array "$action$target" "${_builder_chosen_action_targets[@]}"; then
    echo "${COLOR_BLUE}## $action$target starting...${COLOR_RESET}"
    return 0
  fi
  return 1
}

#
# Returns 0 if the user has --option on the command line
#
# Usage:
#   if build_has_option option; then ...; fi
# Parameters:
#   1: option    name of option, i.e. --option
# Example:
#   if build_has_option --debug; then
#
builder_has_option() {
  local option="$1"

  if _builder_item_in_array "$option" "${_builder_chosen_options[@]}"; then
    return 0
  fi
  return 1
}

_builder_trim() {
  local var="$*"
  # remove leading whitespace characters
  var="${var#"${var%%[![:space:]]*}"}"
  # remove trailing whitespace characters
  var="${var%"${var##*[![:space:]]}"}"
  printf '%s' "$var"
}

#
# Describes a build script, defines available parameters
# and their meanings. Use together with builder_parse
# to process input parameters
#
# Usage:
#    builder_describe description param_desc...
# Parameters:
#    description   A short description of what the script does
#    param_desc    Space separated name and description of parameter, e.g.
#                  "build   Builds the target"
#                  May be repeated to describe all parameters
#
# There are three types of parameters that may be specified:
#
# * Option, param_desc format: "--option[,-o][=var]   [One line description]"
#   All options must have a longhand form with two prefix hyphens,
#   e.g. --option. The ",-o" shorthand form is optional. When testing if
#   the option is set with `builder_has_option``, always use the longhand
#   form.
#
#   if =var is specified, then the next parameter will be a variable stored
#   in $var for that option. e.g. --option=opt means $opt will have the value
#   'foo' when the script is called for --option foo.
#
# * Action, param_desc format: "action   [One line description]"
#   Actions must be a single word, lower case. To specify an action
#   as the default, append a '+' to the action name, e.g.
#   "test+   Test the project". If there is no default specified, then
#   it will be 'build'
#
# * Target, param_desc format: ":target   [One line description]"
#   A target always starts with colon, e.g. :project.
#
builder_describe() {
  _builder_description="$1"
  _builder_actions=()
  _builder_targets=()
  _builder_options=()
  _builder_default_action=build
  declare -A -g _builder_params
  declare -A -g _builder_options_short
  declare -A -g _builder_options_var
  shift
  # describe each target, action, and option possibility
  while [[ $# -gt 0 ]]; do
    local key="$1"
    local value="$(echo "$key" | cut -d" " -f 1 -)"
    local description=
    if [[ $key =~ [[:space:]] ]]; then
      description=$(_builder_trim "$(echo "$key" | cut -d" " -f 2- -)")
    fi

    if [[ $value =~ ^: ]]; then
      # Parameter is a target
      _builder_targets+=($value)
    elif [[ $value =~ ^-- ]]; then
      # Parameter is an option
      # Look for a shorthand version of the option
      local option_var=
      if [[ $value =~ = ]]; then
        option_var="$(echo "$value" | cut -d= -f 2 -)"
        value="$(echo "$value" | cut -d= -f 1 -)"
      fi

      if [[ $value =~ , ]]; then
        local option_long="$(echo "$value" | cut -d, -f 1 -)"
        local option_short="$(echo "$value" | cut -d, -f 2 -)"
        _builder_options+=($option_long)
        _builder_options_short[$option_short]="$option_long"
        if [[ ! -z "$option_var" ]]; then
          _builder_options_var[$option_long]="$option_var"
        fi
        value="$option_long, $option_short"
      else
        _builder_options+=($value)
        if [[ ! -z "$option_var" ]]; then
          _builder_options_var[$value]="$option_var"
        fi
      fi

      if [[ ! -z $option_var ]]; then
        value="$value $option_var"
      fi
    else
      # Parameter is an action
      if [[ $value =~ \+$ ]]; then
        # If the action name has a '+' suffix then it is the default action
        value=${value//+}
        _builder_default_action=$value
      fi
      _builder_actions+=($value)
    fi

    if [[ -z "${description}" ]]; then
      description=$(_builder_get_default_description "$value")
    fi
    _builder_params[${value}]="$description"

    shift
  done

  # We'll always add a :project if no target is specified
  if (( ! ${#_builder_targets[@]} )); then
    _builder_targets+=(:project)
    _builder_params[\:project]=$(_builder_get_default_description ":project")
  fi
}

_builder_get_default_description() {
  local description=
  local value="$1"
  # default descriptions for common build actions, targets, and options
  case "$value" in
    clean)     description="remove build/ folder and build artifacts" ;;
    configure) description="install dependencies, e.g. npm" ;;
    build)     description="build target(s)" ;;
    test)      description="run automated tests" ;;
    :project)  description="this project" ;;
    :app)      description="main app" ;;
    :engine)   description="engine module" ;;
    :module)   description="this module" ;;
    :tools)    description="build tools for this project" ;;
    --debug)   description="debug build" ;;
  esac
  echo "$description"
}

_builder_parameter_error() {
  local program="$1"
  local type="$2"
  local param="$3"
  echo "$COLOR_RED$program: invalid $type: $param$COLOR_RESET"
  echo
  builder_display_usage
  exit 64

}

# Initializes a build.sh script, parses command line. Will abort the script if
# invalid parameters are passed in. Use together with builder_describe which
# sets up the possible command line parameters
#
# Usage:
#   builder_parse "$@"
# Parameters
#   1: $@         command-line arguments
builder_parse() {
  builder_verbose=
  builder_extra_params=()
  _builder_chosen_action_targets=()
  _builder_chosen_options=()

  # Process command-line arguments
  while [[ $# -gt 0 ]] ; do
    local key="$1"
    local action=
    local target=
    local e has_action has_target has_option longhand_option

    if [[ $key == "--" ]]; then
      shift
      builder_extra_params=("$@")
      break
    fi

    if [[ $key =~ : ]]; then
      IFS=: read -r action target <<< $key
      target=:$target
    else
      action="$key"
      target=
    fi

    _builder_item_in_array "$action" "${_builder_actions[@]}" && has_action=1 || has_action=0
    _builder_item_in_array "$target" "${_builder_targets[@]}" && has_target=1 || has_target=0

    # Expand short -o to --option in options lookup
    if [[ ! -z ${_builder_options_short[$key]+x} ]]; then
      key=${_builder_options_short[$key]}
    fi
    _builder_item_in_array "$key" "${_builder_options[@]}" && has_option=1 || has_option=0

    if (( has_action )) && (( has_target )); then
      # apply the selected action and selected target
      _builder_chosen_action_targets+=("$key")
    elif (( has_action )); then
      # apply the selected action to all targets
      if [[ ! -z $target ]]; then
        # A target was specified but is not valid
        _builder_parameter_error "$0" target "$target"
      fi

      for e in "${_builder_targets[@]}"; do
        _builder_chosen_action_targets+=("$action$e")
      done
    elif (( has_target )); then
      # apply the default action to the selected target

      if [[ ! -z $action ]]; then
        # An action was specified but is not valid
        _builder_parameter_error "$0" action "$action"
      fi

      _builder_chosen_action_targets+=("$_builder_default_action$target")
    elif (( has_option )); then
      _builder_chosen_options+=("$key")
      if [[ ! -z ${_builder_options_var[$key]+x} ]]; then
        shift
        if [[ $# -eq 0 ]]; then
          _builder_parameter_error "$0" parameter "$key"
        fi
        # Set the variable associated with this option to the next parameter value
        # A little bit of hoop jumping here to avoid issues with cygwin paths being
        # corrupted too early in the game
        local varname=${_builder_options_var[$key]}
        declare -g $varname="$1"
      fi

    else
      case "$key" in
        --help|-h)
          builder_display_usage
          exit 0
          ;;
        --color)
          builder_use_color true
          ;;
        --no-color)
          builder_use_color false
          ;;
        --verbose|-v)
          _builder_chosen_options+=(--verbose)
          builder_verbose=--verbose
          ;;
        *)
          _builder_parameter_error "$0" parameter "$key"
      esac
    fi
    shift # past the processed argument
  done

  if (( ! ${#_builder_chosen_action_targets[@]} )); then
    for e in "${_builder_targets[@]}"; do
      _builder_chosen_action_targets+=("$_builder_default_action$e")
    done
  fi

  if $_builder_debug; then
    echo "[DEBUG] Selected actions and targets:"
    for e in "${_builder_chosen_action_targets[@]}"; do
      echo "* $e"
    done
    echo
    echo "[DEBUG] Selected options:"
    for e in "${_builder_chosen_options[@]}"; do
      echo "* $e"
    done
  fi
}

_builder_pad() {
  local count=$1
  local text1=$2
  local text2=$3
  local fmt="%-${count}s%s\n"
  printf $fmt "$text1" "$text2"
}

builder_display_usage() {
  local e program description

  # Minimum padding is 12 characters, increase this if necessary
  # if you add other, longer, global options (like --verbose)
  local width=12

  for e in "${!_builder_params[@]}"; do
    if (( ${#e} > $width )); then
      width=${#e}
    fi
  done

  width=$((width + 6))

  program="$(basename "$0")"
  if [[ ! -z ${_builder_description+x} ]]; then
    echo "$program: $_builder_description"
    echo
  fi

  echo "Usage: $program [options...] [action][:target]..."
  echo
  echo "Actions: "

  for e in "${_builder_actions[@]}"; do
    if [[ -v _builder_params[$e] ]]; then
      description="${_builder_params[$e]}"
    else
      description=$(_builder_get_default_description "$e")
    fi
    _builder_pad $width "  $e" "$description"
  done

  echo
  echo "Targets: "

  for e in "${_builder_targets[@]}"; do
    if [[ -v _builder_params[$e] ]]; then
      description="${_builder_params[$e]}"
    else
      description=$(_builder_get_default_description "$e")
    fi
    _builder_pad $width "  $e" "$description"
  done

  echo
  echo "Options: "

  for e in "${!_builder_params[@]}"; do
    if [[ $e =~ ^-- ]]; then
      _builder_pad $width "  $e" "${_builder_params[$e]}"
    fi
  done

  _builder_pad $width "  --verbose, -v"  "Verbose logging"
  _builder_pad $width "  --color"        "Force colorized output"
  _builder_pad $width "  --no-color"     "Never use colorized output"
  _builder_pad $width "  --help, -h"     "Show this help"
  local c1="${COLOR_BLUE:=<}"
  local c0="${COLOR_RESET:=>}"
  echo
  echo "* Specify ${c1}action:target${c0} to run a specific ${c1}action${c0} against a specific ${c1}:target${c0}."
  echo "* If ${c1}action${c0} is specified without a ${c1}target${c0} suffix, it will be applied to all ${c1}:target${c0}s."
  echo "* If ${c1}:target${c0} is specified without an ${c1}action${c0} prefix, ${c1}$_builder_default_action:target${c0} will be inferred."
  echo "* If no ${c1}action${c0}, ${c1}:target${c0}, or ${c1}action:target${c0} entries are specified, ${c1}$_builder_default_action${c0} will run on all ${c1}:target${c0}s."
  echo
}

builder_report() {
  local result="$1"
  local action="$2" target

  if [[ $action =~ : ]]; then
    IFS=: read -r action target <<< $action
    target=:$target
  elif [[ -z ${3+x} ]]; then
    target=:project
  else
    target="$3"
  fi

  if [[ $result == success ]]; then
    echo "${COLOR_GREEN}## $action$target completed successfully${COLOR_RESET}"
  else
    echo "${COLOR_RED}## $action$target failed. Result: $result${COLOR_RESET}"
  fi
}

set_keyman_standard_build_path() {
  PATH="$KEYMAN_ROOT/node_modules/.bin:$PATH"
}

#
# printXCodeBuildScriptLogs: xcodebuild does not emit stdout from scripts in
# PBXShellScriptBuildPhase phases. This is a real problem for us because if
# there is an issue, we just can't see it. So we capture the output in a
# separate logfile, and then call printXCodeBuildScriptLogs after any xcodebuild
# call to get the output.
#
# This file is captured in xcode-utils.sh, logScriptsToFile function, and each
# script phase will append to the log file, until funprintXCodeBuildScriptLogs
# is called, at which point the logfile will be deleted.
#
# The logfile is placed in $KEYMAN_ROOT/xcodebuild-scripts.log. It is used for
# both iOS and macOS builds.
#
# If there is no logfile, then this function will not emit anything.
#
printXCodeBuildScriptLogs() {
  local SCRIPT_LOG="$KEYMAN_ROOT/xcodebuild-scripts.log"
  if [ -f "$SCRIPT_LOG" ]; then
    echo "printXCodeBuildScriptLogs: reporting script results from previous xcode build"
    cat "$SCRIPT_LOG"
    rm "$SCRIPT_LOG"
    echo "printXCodeBuildScriptLogs: done"
    echo
  fi
}

#
# Wraps xcodebuild with error handling and log printing
#
run_xcodebuild() {
  typeset cmnd="$*"
  typeset ret_code
  local hasSetErrExit=false
  if [ -o errexit ]; then
    hasSetErrExit=true
    set +e
  fi
  eval xcodebuild $cmnd
  ret_code=$?
  if $hasSetErrExit; then
    set -e
  fi

  printXCodeBuildScriptLogs
  if [ $ret_code != 0 ]; then
    fail "Build failed! Error: [$ret_code] when executing command: 'xcodebuild $cmnd'"
  fi
}