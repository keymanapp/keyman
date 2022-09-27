#!/usr/bin/env bash
#
# This script contails utilities for builder_script calls
#

# Used to build script-related build variables useful for referencing the calling script
# and for prefixing builder_finish_action outputs in order to more clearly identify the calling
# script.
#
# Assumes that `_builder_findRepoRoot` has already been called, a condition met later on
# within this script.
function _builder_setBuildScriptIdentifiers() {
  if [ ! -z ${THIS_SCRIPT+x} ]; then
    THIS_SCRIPT_PATH="$(dirname "$THIS_SCRIPT")"
    readonly THIS_SCRIPT_PATH
    THIS_SCRIPT_NAME="$(basename "$THIS_SCRIPT")"
    readonly THIS_SCRIPT_NAME
    # Leaves only the part of the path based upon KEYMAN_ROOT.
    THIS_SCRIPT_IDENTIFIER=${THIS_SCRIPT_PATH#"$REPO_ROOT/"}
    readonly THIS_SCRIPT_IDENTIFIER
  fi
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

    # Used by `builder_display_usage` when marking special terms (actions, targets, options)
    # in the plain-text description area.
    BUILDER_TERM_START="$COLOR_BLUE"
    BUILDER_TERM_END="$COLOR_RESET"
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
    BUILDER_TERM_START="<"
    BUILDER_TERM_END=">"
  fi
}

if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]] && [[ "$TERM" != "unknown" ]]; then
  builder_use_color true
else
  builder_use_color false
fi

function die () {
    # TODO: consolidate this with fail() from shellHelperFunctions.sh
    echo
    echo "${COLOR_RED}$* ${COLOR_RESET}"
    echo
    exit 1
}

function builder_die() {
    die "$*"
}

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

function _builder_warn_if_incomplete() {
  if [ -n "${_builder_current_action}" ]; then
    local scope="[$THIS_SCRIPT_IDENTIFIER] "
    echo "${COLOR_YELLOW}## ${scope}Warning - $_builder_current_action never reported success or failure${COLOR_RESET}"
    # exit 1  # If we wanted this scenario to result in a forced build-script fail.
  fi

  # Since we've already warned about this once, we'll clear the variable to prevent repetitions.
  _builder_current_action=
}

# Used by a `trap` statement later to facilitate auto-reporting failures on error detection
# without obscuring failure exit/error codes.
_builder_failure_trap() {
  local trappedExitCode=$?
  local action target

  # Since 'exit' is also trapped, we can also handle end-of-script incomplete actions.
  if [[ $trappedExitCode == 0 ]]; then
    # While there weren't errors, were there any actions that never reported success or failure?
    _builder_warn_if_incomplete
    return
  fi

  # If we've reached this point, we're here because an error occurred.

  # Iterate across currently-active actions and report their failures.
  if [ -n "${_builder_current_action}" ]; then
    action="${_builder_current_action}"
    if [[ $action =~ : ]]; then
      IFS=: read -r action target <<< $action
      target=:$target
    else
      target=:project
    fi

    builder_finish_action failure $action $target
  fi
}

#
# Builds the standardized `action:target` string for the specified action-target
# pairing and also returns 0 if the user has asked to perform it on the command
# line.  Otherwise, returns 0 and sets an empty string in place of the matched
# pair.
#
# The string will be set as `_builder_matched_action`, which is for
# build-utils.sh internal use, used by `builder_start_action`.
#
# Usage:
#   if build_has_action action[:target]; then ...; fi
# Parameters:
#   1: action    name of action
#   2: :target    name of target, :-prefixed, as part of first param or space separated ok
# Example:
#   if builder_has_action build :app; then  # or build:app, that's fine too.
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
    # To avoid WET re-processing of the $action$target string set
    _builder_matched_action="$action$target"
    return 0
  else
    _builder_matched_action=
    return 1
  fi
}

#
# Returns 0 if the user has asked to perform action on target on the command line, and
# then starts the action. Should be paired with builder_finish_action
#
# Usage:
#   if builder_start_action action[:target]; then ...; fi
# Parameters:
#   1: action    name of action
#   2: :target    name of target, :-prefixed, as part of first param or space separated ok
# Example:
#   if builder_start_action build :app; then
#   if builder_start_action build:app; then
#
builder_start_action() {
  local scope="[$THIS_SCRIPT_IDENTIFIER] "

  if builder_has_action $@; then
    echo "${COLOR_BLUE}## $scope$_builder_matched_action starting...${COLOR_RESET}"
    if [ -n "${_builder_current_action}" ]; then
      _builder_warn_if_incomplete
    fi
    _builder_current_action="$_builder_matched_action"
    return 0
  else
    return 1
  fi
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
# Describes a build script, defines available parameters and their meanings. Use
# together with `builder_parse` to process input parameters.
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

# Pre-initializes the color setting based on the options specified to a
# a build.sh script, parsing the command line to do so.  This is only
# needed if said script wishes to use this script's defined colors while
# respecting the options provided by the script's caller.
#
# Usage:
#   builder_check_color "$@"
# Parameters
#   1: $@         all command-line arguments (as with builder_parse)
builder_check_color() {
  # Process command-line arguments
  while [[ $# -gt 0 ]] ; do
    local key="$1"

    case "$key" in
      --color)
        builder_use_color true
        ;;
      --no-color)
        builder_use_color false
        ;;
    esac
    shift # past the processed argument
  done
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
  _builder_current_action=

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

  # Now that we've successfully parsed options adhering to the _builder spec, we may activate our
  # action_failure and action_hanging traps.  (We don't want them active on scripts not yet using
  # said script.)
  #
  # Note:  if an error occurs within a script's function in a `set -e` script, it becomes an exit
  # instead for the function's caller.  So, we need both `err` and `exit` here.
  # See https://medium.com/@dirk.avery/the-bash-trap-trap-ce6083f36700.
  trap _builder_failure_trap err exit
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

  # Defined in `builder_use_color`; this assumes that said func has been called.
  local c1=$BUILDER_TERM_START
  local c0=$BUILDER_TERM_END
  echo
  echo "* Specify ${c1}action:target${c0} to run a specific ${c1}action${c0} against a specific ${c1}:target${c0}."
  echo "* If ${c1}action${c0} is specified without a ${c1}target${c0} suffix, it will be applied to all ${c1}:target${c0}s."
  echo "* If ${c1}:target${c0} is specified without an ${c1}action${c0} prefix, ${c1}$_builder_default_action:target${c0} will be inferred."
  echo "* If no ${c1}action${c0}, ${c1}:target${c0}, or ${c1}action:target${c0} entries are specified, ${c1}$_builder_default_action${c0} will run on all ${c1}:target${c0}s."
  echo
}

builder_finish_action() {
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

  local scope="[$THIS_SCRIPT_IDENTIFIER] "

  if [[ "$action$target" == "${_builder_current_action}" ]]; then
    if [[ $result == success ]]; then
      echo "${COLOR_GREEN}## $scope$action$target completed successfully${COLOR_RESET}"
    elif [[ $result == failure ]]; then
      echo "${COLOR_RED}## $scope$action$target failed${COLOR_RESET}"
    else
      echo "${COLOR_RED}## $scope$action$target failed with message: $result${COLOR_RESET}"
    fi

    # Remove $action$target from the array; it is no longer a current action
    _builder_current_action=
  else
    echo "${COLOR_YELLOW}## Warning: reporting result of $action$target but the action was never started!${COLOR_RESET}"
  fi
}

set_keyman_standard_build_path() {
  PATH="$REPO_ROOT/node_modules/.bin:$PATH"
}

