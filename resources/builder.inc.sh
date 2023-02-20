#!/usr/bin/env bash
#
# This script contains utilities for builder_script calls
#
# * builder_ functions and variables are defined here.
# * REPO_ROOT defines the top level of this repository
# * THIS_SCRIPT_PATH defines the full path of the running script
# * THIS_SCRIPT_NAME defines the basename of the running script
# * THIS_SCRIPT_IDENTIFIER defines the repo-relative path of the running script
# * _builder_ functions and variables are internal use only for builder.inc.sh, and
#   subject to change at any time. Do not use them in other scripts.
# * Note: the running script is the top-level script that includes either
#   builder.inc.sh directly, or, just in the Keyman repo, via build-utils.sh.
#

# _builder_init is called internally at the bottom of this file after we have
# all function declarations in place.
function _builder_init() {
  _builder_findRepoRoot
  _builder_setBuildScriptIdentifiers

  if [[ -n "$TERM" ]] && [[ "$TERM" != "dumb" ]] && [[ "$TERM" != "unknown" ]] && [ -t 1 ]; then
    builder_use_color true
  else
    builder_use_color false
  fi
}

function _builder_findRepoRoot() {
    # See https://stackoverflow.com/questions/59895/how-to-get-the-source-directory-of-a-bash-script-from-within-the-script-itself
    # None of the answers are 100% correct for cross-platform
    # On macOS, requires coreutils (`brew install coreutils`)
    local SCRIPT=$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")
    REPO_ROOT=$(dirname $(dirname "$SCRIPT"))
    readonly REPO_ROOT
}

# Used to build script-related build variables useful for referencing the calling script
# and for prefixing `builder_finish_action` outputs in order to more clearly identify the calling
# script.
#
# Assumes that `THIS_SCRIPT` has been set, typically like this:
#
# ```bash
#   ## START STANDARD BUILD SCRIPT INCLUDE
#   # adjust relative paths as necessary
#   THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
#   . "$(dirname "$THIS_SCRIPT")/resources/builder.inc.sh"
#   ## END STANDARD BUILD SCRIPT INCLUDE
# ```
#
function _builder_setBuildScriptIdentifiers() {
  if [ ! -z ${THIS_SCRIPT+x} ]; then
    THIS_SCRIPT_PATH="$(dirname "$THIS_SCRIPT")"
    readonly THIS_SCRIPT_PATH
    THIS_SCRIPT_NAME="$(basename "$THIS_SCRIPT")"
    readonly THIS_SCRIPT_NAME
    # Leaves only the part of the path based upon REPO_ROOT.
    THIS_SCRIPT_IDENTIFIER=${THIS_SCRIPT_PATH#"$REPO_ROOT/"}
    readonly THIS_SCRIPT_IDENTIFIER
  else
    echo "Warning: THIS_SCRIPT not defined; builder.inc.sh has not been sourced with standard script include."
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

#
# Wraps the input string in `builder_display_usage` with $BUILDER_TERM_START and
# $BUILDER_TERM_END
#
function builder_term() {
  echo "${BUILDER_TERM_START}$*${BUILDER_TERM_END}"
}

function builder_die() {
  echo
  if [[ $# -eq 0 ]]; then
    echo "${COLOR_RED}Unspecified error, aborting script${COLOR_RESET}"
  else
    echo "${COLOR_RED}$*${COLOR_RESET}"
  fi
  echo
  exit 1
}

function builder_warn() {
  echo "${COLOR_YELLOW}$*${COLOR_RESET}"
}

function builder_heading() {
  echo -e "${HEADING_SETMARK}${COLOR_BLUE}$*${COLOR_RESET}"
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
if [ -z ${_builder_debug+x} ]; then
  _builder_debug=false
fi

if $_builder_debug; then
  echo "[DEBUG] Command line: $0 $@"
fi

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
  for e; do [[ "$e" == $match ]] && return 0; done
  return 1
}

#
# Returns `0` if first parameter is in the array passed as second parameter,
# where the array may contain globs.
#
# ### Parameters
#
# * 1: `item`       item to search for in array
# * 2: `array`      bash array, e.g. `array=(one two three)`
#
# ### Example
#
# ```bash
#   array=(foo bar it*)
#   if _builder_item_in_glob_array "item" "${array[@]}"; then ...; fi
# ```
#
_builder_item_in_glob_array() {
  local e match="$1"
  shift
  [[ -z "$match" ]] && return 1
  for e; do [[ "$match" == $e ]] && return 0; done
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

  _builder_cleanup_deps

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

    builder_finish_action failure $action$target

    # Make 100% sure that the exit code chains fully.
    # Without this, nested scripts have failed to chain errors from npm calls past the script
    # that directly executed the failed npm command.
    exit $trappedExitCode
  fi
}

#
# Removes temporary `_builder_deps_built` file when top-level build script
# finishes.
#
_builder_cleanup_deps() {
  if ! builder_is_dep_build && [[ ! -z ${_builder_deps_built+x} ]]; then
    if $_builder_debug; then
      echo "[DEBUG] Dependencies that were built:"
      cat "$_builder_deps_built"
    fi
    rm -f "$_builder_deps_built"
    _builder_deps_built=
  fi
}

_builder_execute_child() {
  local action=$1
  local target=$2

  local scope="[$THIS_SCRIPT_IDENTIFIER] "
  local script="$THIS_SCRIPT_PATH/${_builder_target_paths[$target]}/build.sh"

  if $_builder_debug; then
    echo "${COLOR_BLUE}## $scope$action$target starting...${COLOR_RESET}"
  fi

  "$script" $action \
    $builder_verbose \
    $builder_debug \
  && (
    if $_builder_debug; then
      echo "${COLOR_GREEN}## $scope$action$target completed successfully${COLOR_RESET}"
    fi
  ) || (
    result=$?
    echo "${COLOR_RED}## $scope$action$target failed with exit code $result${COLOR_RESET}"
    exit $result
  )
}

_builder_run_child_action() {
  local action="$1" target

  if [[ $action =~ : ]]; then
    IFS=: read -r action target <<< $action
    target=:$target
  else
    target=':*'
  fi

  if builder_has_action $action$target; then
    if [[ $target == ':*' ]]; then
      # run all children in order specified in builder_describe
      for target in "${_builder_targets[@]}"; do
        # We have to re-test the action because the user may not
        # have specified all targets in their invocation
        if builder_has_action $action$target; then
          if [ -f "$THIS_SCRIPT_PATH/${_builder_target_paths[$target]}/build.sh" ]; then
            _builder_execute_child $action $target
          fi
        fi
      done
    else
      # If specified explicitly, we assume existence of a child build script.
      _builder_execute_child $action $target
    fi
  fi
}

#
# Executes the specified actions on all child targets, or on the specified
# targets. A child target is any target which has a sub-folder of the same name
# as the target. However, the actions will only be run if they have been
# specified by the user on the command-line.
#
# ### Usage
#
# ```bash
# builder_run_child_actions action1 [...]
# ```
#
# ### Parameters
#
#   1...: action[:target]   name of action:target to run
#
# ### Example
#
# ```bash
# builder_run_child_actions configure build test install
# ```
#
builder_run_child_actions() {
  while [[ $# -gt 0 ]]; do
    local action="$1"
    _builder_run_child_action "$action"
    shift
  done
}

#
# Builds the standardized `action:target` string for the specified action-target
# pairing and also returns 0 if the user has asked to perform it on the command
# line.  Otherwise, returns 0 and sets an empty string in place of the matched
# pair.
#
# The string will be set as `_builder_matched_action`, which is for
# builder.inc.sh internal use, used by `builder_start_action`.
#
# ### Usage
#
# ```bash
#   if build_has_action action[:target]; then ...; fi
# ````
#
# ### Parameters
#
#   1: action[:target]    name of action:target
#
# ### Example
#
# ```bash
#   if builder_has_action build:app; then ...
# ```
#
builder_has_action() {
  local action="$1" target

  if [[ $action =~ : ]]; then
    IFS=: read -r action target <<< $action
    target=:$target
  else
    target=':*'
  fi

  if _builder_item_in_array "$action$target" "${_builder_chosen_action_targets[@]}"; then
    # To avoid WET re-processing of the $action$target string set
    _builder_matched_action="$action$target"
    if [[ $target == ':*' ]]; then
      _builder_matched_action_name="$action"
    else
      _builder_matched_action_name="$action$target"
    fi
    return 0
  else
    _builder_matched_action=
    return 1
  fi
}

#
# Returns `0` if the user has asked to perform action on target on the command
# line, and then starts the action. Should be paired with
# `builder_finish_action`.
#
# ### Usage
#
# ```bash
#   if builder_start_action action[:target]; then ...; fi
# ```
#
# ### Parameters
#
# * 1: `action[:target]`   name of action, and optionally also target, if
#                          target excluded starts for all defined targets
#
# ### Example
#
# ```bash
#   if builder_start_action build:app; then ...
# ```
#
builder_start_action() {
  local scope="[$THIS_SCRIPT_IDENTIFIER] "

  if builder_has_action $1; then
    # In a dependency quick build (the default), determine whether we actually
    # need to run this step. Uses data passed to builder_describe_outputs to
    # verify whether a target output is present.
    if builder_is_dep_build &&
        ! builder_is_full_dep_build &&
        [[ ! -z ${_builder_dep_path[$_builder_matched_action]+x} ]] &&
        [[ -e "$KEYMAN_ROOT/${_builder_dep_path[$_builder_matched_action]}" ]]; then
      echo "$scope skipping $_builder_matched_action_name, up-to-date"
      return 1
    fi

    echo "${COLOR_BLUE}## $scope$_builder_matched_action_name starting...${COLOR_RESET}"
    if [ -n "${_builder_current_action}" ]; then
      _builder_warn_if_incomplete
    fi
    _builder_current_action="$_builder_matched_action"

    # Build dependencies as required
    _builder_do_build_deps "$_builder_matched_action"
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
# Expands an in-repo-relative path to a repo-relative path. A path starting with
# `/` is expected to be relative to repo root, not filesystem root. Otherwise,
# it's relative to current script path, not current working directory. The
# returned path will not have a prefix `/`, and will be relative to
# `$KEYMAN_ROOT`. Assumes realpath is installed (brew coreutils on macOS).
#
_builder_expand_relative_path() {
  local path="$1"
  if [[ "$path" =~ ^/ ]]; then
    echo "${path:1}"
  else
    realpath --canonicalize-missing --relative-to="$KEYMAN_ROOT" "$THIS_SCRIPT_PATH/$path"
  fi
}

#
# Expands an `[action][:target]` string, replacing missing values with `*`,
# for example:
#
# * `build` --> `build:*`
# * `build:app` --> `build:app`
# * `:app` --> `*:app`
#
# Supports multiple action:targets in the string
#
_builder_expand_action_target() {
  local input="$1" target= action=
  if [[ "$input" =~ : ]]; then
    action=$(echo "$input" | cut -d: -f 1 -)
    target=$(echo "$input" | cut -d: -f 2 -)
  else
    action=$input
  fi

  if [[ -z "$action" ]]; then
    action='*'
  fi
  if [[ -z "$target" ]]; then
    target='*'
  fi

  echo "$action:$target"
}

_builder_expand_action_targets() {
  local input=($1) e output=()
  for e in "${input[@]}"; do
    e=`_builder_expand_action_target "$e"`
    output+=($e)
  done
  if [[ ${#output[@]} == 0 ]]; then
    echo "*:*"
  else
    echo "${output[@]}"
  fi
}

#
# Describes a build script, defines available parameters and their meanings. Use
# together with `builder_parse` to process input parameters.
#
# ### Usage
#
# ```bash
#    builder_describe description param_desc...
# ```
#
# ### Parameters
#
#  * `description`  A short description of what the script does.
#  * `param_desc`   Space separated name and description of parameter, e.g.
#                   `"build   Builds the target"`
#                   This parameter may be repeated to describe all parameters.
#
# There are four types of parameters that may be specified:
#
# * **Option:** `"--option[,-o][=var]   [One line description]"`
#
#   All options must have a longhand form with two prefix hyphens,
#   e.g. `--option`. The `,-o` shorthand form is optional. When testing if
#   the option is set with `builder_has_option`, always use the longhand
#   form.
#
#   if `=var` is specified, then the next parameter will be a variable stored in
#   `$var` for that option. e.g. `--option=opt` means `$opt` will have the value
#   `"foo"` when the script is called for `--option foo`.
#
# * **Action**: `"action   [One line description]"`
#
#   Actions must be a single word, lower case. To specify an action as the
#   default, append a `+` to the action name, e.g. `"test+   Test the project"`.
#   If there is no default specified, then it will be `build`.
#
# * **Target:** `":target[=path]   [One line description]"`
#
#   A target always starts with colon, e.g. `:project`. If a folder exists with
#   the same name as a target, then that automatically denotes the target as a
#   "child project". This can simplify parent-child style scripts, using the
#   [`builder_run_child_actions`] function.
#
#   A child project with an alternate folder can also be specified by appending
#   `=path` to the target definition, for example `:app=src/app`. Where
#   possible, avoid differences in names of child projects and folders.
#
# * **Dependency:** "@/path/to/dependency [action][:target] ..."
#
#   A dependency always starts with `@`. The path to the dependency will be
#   relative to the build script folder, or to the root of the repository, if
#   the path starts with `/`, not to the root of the file system. It is an error
#   to specify a dependency outside the repo root.
#
#   Relative paths will be expanded to full paths, again, relative to the root
#   of the repository.
#
#   Dependencies may be limited to specific `action:target`. If not specified,
#   dependencies will be built for all actions on all targets. Either `action`
#   or `:target` may be omitted, and multiple actions and targets may be
#   specified, space separated.
#
builder_describe() {
  _builder_record_function_call builder_describe

  _builder_description="$1"
  _builder_actions=()
  _builder_targets=()
  _builder_options=()
  _builder_deps=()                    # array of all dependencies for this script
  _builder_default_action=build
  declare -A -g _builder_params
  declare -A -g _builder_options_short
  declare -A -g _builder_options_var
  declare -A -g _builder_dep_path             # array of output files for action:target pairs
  declare -A -g _builder_dep_related_actions  # array of action:targets associated with a given dependency
  declare -A -g _builder_internal_dep         # array of internal action:targets dependency relationships
  declare -A -g _builder_target_paths         # array of target child project paths
  shift
  # describe each target, action, and option possibility
  while [[ $# -gt 0 ]]; do
    local key="$1"
    local value="$(echo "$key" | cut -d" " -f 1 -)"
    local description=
    if [[ $key =~ [[:space:]] ]]; then
      description="$(_builder_trim "$(echo "$key" | cut -d" " -f 2- -)")"
    fi

    if [[ $value =~ ^: ]]; then
      # Parameter is a target
      local target_path=
      if [[ $value =~ = ]]; then
        # The target has a custom child project path
        target_path="$(echo "$value" | cut -d= -f 2 -)"
        value="$(echo "$value" | cut -d= -f 1 -)"
        if [[ ! -d "$THIS_SCRIPT_PATH/$target_path" ]]; then
          builder_die "Target path '$target_path' for $value does not exist."
        fi
      else
        # If the target name matches a folder name, implicitly
        # make it available as a child project
        if [[ -d "$THIS_SCRIPT_PATH/${value:1}" ]]; then
          target_path="${value:1}"
        fi
      fi
      _builder_targets+=($value)
      if [[ ! -z "$target_path" ]]; then
        _builder_target_paths[$value]="$target_path"
      fi
    elif [[ $value =~ ^@ ]]; then
      # Parameter is a dependency
      local dependency="${value:1}"
      dependency="`_builder_expand_relative_path "$dependency"`"
      _builder_deps+=($dependency)
      _builder_dep_related_actions[$dependency]="`_builder_expand_action_targets "$description"`"

      # We don't want to add deps to params, so shift+continue
      shift
      continue
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

#
# Defines an output file or folder expected to be present after successful
# completion of an action for a target. Used to skip actions for dependency
# builds. If `:target` is not provided, assumes `:project`.
#
# Relative paths are relative to script folder; absolute paths are relative
# to repository root, not filesystem root.
#
# ### Usage
#
# ```bash
#   builder_describe_outputs action:target filename [...]
# ```
#
# ### Parameters
#
# * 1: `action[:target]`   action and/or target associated with file
# * 2: `filename`          name of file or folder to check
# * 3+: ... repeat previous arguments for additional outputs
#
# ### Example
#
# ```bash
#   builder_describe_outputs \
#     configure /node_modules \
#     build     build/index.js
# ```
#
function builder_describe_outputs() {
  _builder_record_function_call builder_describe_outputs

  while [[ $# -gt 0 ]]; do
    local key="$1" path="$2" action target
    path="`_builder_expand_relative_path "$path"`"

    if [[ $key =~ : ]]; then
      action="$(echo "$key" | cut -d: -f 1 -)"
      target=":$(echo "$key" | cut -d: -f 2 -)"
    else
      # Add dependency expected output file for all targets, as well as a
      # wildcard target match
      action="$key"
      for target in "${_builder_targets[@]}"; do
        _builder_dep_path[$action$target]="$path"
      done
      target=':*'
    fi
    _builder_dep_path[$action$target]="$path"
    shift 2
  done

  _builder_define_default_internal_dependencies

  # We only want to define internal dependencies after both builder_parse and builder_describe_outputs have been called
  if _builder_has_function_been_called builder_parse; then
    _builder_add_chosen_action_target_dependencies
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

#
# Pre-initializes the color setting based on the options specified to a
# a build.sh script. This is called automatically during init.
#
# Parameters
#   1: "$@"         all command-line arguments
#
_builder_check_color() {
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

#
# For every build action:target in _builder_chosen_action_targets, add
# its full internal dependency tree
#
_builder_add_chosen_action_target_dependencies() {
  local action_target e i=0 new_actions=()

  # Iterate through every action specified on command line; we use this loop
  # style so that any new actions added here will also be iteratively checked
  while (( $i < ${#_builder_chosen_action_targets[@]} )); do
    action_target=${_builder_chosen_action_targets[$i]}

    # If we have an internal dependency for the chosen action:target pair
    if [[ ! -z ${_builder_internal_dep[$action_target]+x} ]]; then
      local dep_output=${_builder_internal_dep[$action_target]}
      # If there is a defined output for this dependency
      if [[ ! -z ${_builder_dep_path[$dep_output]+x} ]]; then
        # If the output for the dependency is missing, or we have --force-deps
        if [[ ! -e "$KEYMAN_ROOT/${_builder_dep_path[$dep_output]}" ]] || builder_is_full_dep_build; then
          # Add the dependency to the chosen action:target list
          if ! _builder_item_in_array "$dep_output" "${_builder_chosen_action_targets[@]}"; then
            _builder_chosen_action_targets+=($dep_output)
            new_actions+=($dep_output)
          fi
        fi
      fi
    fi
    i=$((i + 1))
  done

  if [[ ${#new_actions[@]} -gt 0 ]]; then
    if builder_is_full_dep_build; then
      echo "Automatically running all dependency actions due to --force-deps:"
    else
      echo "Automatically running following required actions with missing outputs:"
    fi
    for e in "${new_actions[@]}"; do
      echo "* $e"
    done
  fi
}

#
# If we have described outputs, then we will setup our
# default internal dependency chain:
#
#  configure <- build <- (test,install,publish)
#
_builder_define_default_internal_dependencies() {
  for target in "${_builder_targets[@]}"; do
    _builder_define_default_internal_deps_for_target "$target"
  done

  _builder_define_default_internal_deps_for_target ':*'
}

_builder_define_default_internal_deps_for_target() {
  local target=$1
  _builder_define_default_internal_dep "$target" configure build
  _builder_define_default_internal_dep "$target" build test
  _builder_define_default_internal_dep "$target" build install
}

_builder_define_default_internal_dep() {
  local target=$1 dep=$2 action=$3
  if _builder_item_in_array $dep "${_builder_actions[@]}" &&
        _builder_item_in_array $action "${_builder_actions[@]}"; then
    _builder_internal_dep[$action$target]=$dep$target
  fi
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

  _builder_record_function_call builder_parse

  _builder_build_deps=--deps
  builder_verbose=
  builder_debug=
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
        --debug|-d)
          _builder_chosen_options+=(--debug)
          builder_debug=--debug
          ;;
        --deps|--no-deps|--force-deps)
          _builder_build_deps=$key
          ;;
        --builder-dep-parent)
          # internal use parameter for dependency builds - identifier of parent script
          shift
          builder_dep_parent="$1"
          ;;
        --builder-deps-built)
          # internal use parameter for dependency builds - path to dependency tracking file
          shift
          _builder_deps_built="$1"
          ;;
        --builder-report-dependencies)
          # internal reporting function, ignores all other parameters
          _builder_report_dependencies
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

  # We only want to define internal dependencies after both builder_parse and builder_describe_outputs have been called
  if _builder_has_function_been_called builder_describe_outputs; then
    _builder_add_chosen_action_target_dependencies
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

  if builder_is_dep_build; then
    echo "[$THIS_SCRIPT_IDENTIFIER] dependency build, started by $builder_dep_parent"
    if [[ -z ${_builder_deps_built+x} ]]; then
      echo "FATAL ERROR: Expected --builder-deps-built parameter"
      exit 1
    fi
  else
    # This is a top-level invocation, not a dependency build, so we want to
    # track which dependencies have been built, so they don't get built multiple
    # times.
    _builder_deps_built=`mktemp`
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
  # if you add other, longer, global options (like --verbose, --debug)
  local width=12

  for e in "${!_builder_params[@]}"; do
    if (( ${#e} > $width )); then
      width=${#e}
    fi
  done

  width=$((width + 6))

  program="$(basename "$0")"
  if [[ ! -z ${_builder_description+x} ]]; then
    echo "Summary:"
    echo "  $_builder_description"
    echo
  fi
  echo "Script Identifier:"
  echo "  $THIS_SCRIPT_IDENTIFIER"
  echo

  echo "Usage:"
  echo "  $program [options...] [action][:target]..."
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
  _builder_pad $width "  --debug, -d"    "Debug build"
  _builder_pad $width "  --color"        "Force colorized output"
  _builder_pad $width "  --no-color"     "Never use colorized output"
  if builder_has_dependencies; then
    _builder_pad $width "  --deps"         "Build dependencies if required (default)"
    _builder_pad $width "  --no-deps"      "Skip build of dependencies"
    _builder_pad $width "  --force-deps"   "Reconfigure and rebuild all dependencies"
  fi
  _builder_pad $width "  --help, -h"     "Show this help"

  echo
  echo "Dependencies: "

  if builder_has_dependencies; then
    for d in "${_builder_deps[@]}"; do
      echo "  $d"
    done
  else
    echo "  This module has no dependencies"
  fi

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
  local action="$2" target action_name

  if [[ $action =~ : ]]; then
    IFS=: read -r action target <<< $action
    target=":$target"
  else
    target=':*'
  fi

  if [[ "$target" == ":*" ]]; then
    action_name="$action"
  else
    action_name="$action$target"
  fi

  local scope="[$THIS_SCRIPT_IDENTIFIER] "

  if [[ "$action$target" == "${_builder_current_action}" ]]; then
    if [[ $result == success ]]; then
      echo "${COLOR_GREEN}## $scope$action_name completed successfully${COLOR_RESET}"
    elif [[ $result == failure ]]; then
      echo "${COLOR_RED}## $scope$action_name failed${COLOR_RESET}"
    else
      echo "${COLOR_RED}## $scope$action_name failed with message: $result${COLOR_RESET}"
    fi

    # Remove $action$target from the array; it is no longer a current action
    _builder_current_action=
  else
    echo "${COLOR_YELLOW}## Warning: reporting result of $action_name but the action was never started!${COLOR_RESET}"
  fi
}

#
# Returns `0` if the dependency should be built for the given action:target
#
_builder_should_build_dep() {
  local action_target="$1"
  local dep="$2"
  local related_actions=(${_builder_dep_related_actions[$dep]})
  # echo "bdra: ${_builder_dep_related_actions[@]}"
  # echo "target: $action_target"
  # echo "dep: $2"
  # echo "ra: ${related_actions[@]}"
  if ! _builder_item_in_glob_array "$action_target" "${related_actions[@]}"; then
    return 1
  fi
  return 0
}

#
# Configure and build all dependencies
# Later, may restrict by either action or target
#
_builder_do_build_deps() {
  local action_target="$1"

  if [[ $_builder_build_deps == --no-deps ]]; then
    # we've been asked to skip dependencies
    return 0
  fi

  for dep in "${_builder_deps[@]}"; do
    # Don't attempt to build dependencies that don't match the current
    # action:target (wildcards supported for matches here)
    if ! _builder_should_build_dep "$action_target" "$dep"; then
      echo "[$THIS_SCRIPT_IDENTIFIER] Skipping dependency build $dep for $_builder_matched_action_name"
      continue
    fi

    # Only configure and build the dependency once per invocation
    if builder_has_module_been_built "$dep"; then
      continue
    fi

    # TODO: add --debug as a standard builder parameter
    builder_set_module_has_been_built "$dep"
    "$KEYMAN_ROOT/$dep/build.sh" configure build \
      $builder_verbose \
      $builder_debug \
      $_builder_build_deps \
      --builder-deps-built "$_builder_deps_built" \
      --builder-dep-parent "$THIS_SCRIPT_IDENTIFIER"
  done
}

#
# returns `0` if we are in a dependency doing a build.
#
builder_is_dep_build() {
  if [[ ! -z ${builder_dep_parent+x} ]]; then
    return 0
  fi
  return 1
}

#
# returns `0` if we should attempt to do quick builds in a dependency build, for
# example skipping `tsc -b` where a parent may also do it; corresponds to the
# `--deps` parameter (which is the default).
#
builder_is_quick_dep_build() {
  if [[ $_builder_build_deps == --deps ]]; then
    return 0
  fi
  return 1
}

#
# returns `0` if we should do a full configure and build in a dependency build;
# corresponds to the `--force-deps`` parameter.
#
builder_is_full_dep_build() {
  if [[ $_builder_build_deps == --force-deps ]]; then
    return 0
  fi
  return 1
}

#
# returns `0` if the current build script has at least one dependency.
#
builder_has_dependencies() {
  if [[ ${#_builder_deps[@]} -eq 0 ]]; then
    return 1
  fi
  return 0
}

#
# Tests if a dependency module has been built already in the current script
# invocation; if not running in a builder context, always returns `1` (i.e.
# "false").
#
# ### Usage
#
# ```bash
# builder_has_module_been_built dependency-name
# ```
#
# ### Parameters
#
# * 1: `dependency-name`   the `$SCRIPT_IDENTIFIER` of the dependency
#                          (repo-relative path without leading `/`); or for
#                          external dependencies, a path-like starting with
#                          `/external/`.
#
# ### Examples
#
# ```bash
#   if builder_has_module_been_built common/web/keyman-version; then ...
#   if builder_has_module_been_built /external/npm-ci; then ...
# ```
#
builder_has_module_been_built() {
  local module="$1"

  if [[ -z ${_builder_deps_built+x} ]]; then
    # not in a builder context, so we assume a build is needed
    return 1
  fi

  if [[ -f $_builder_deps_built ]] && grep -qx "$module" $_builder_deps_built; then
    # dependency history file contains the dependency module
    return 0
  fi
  return 1
}

#
# Updates the dependency module build state for the current script invocation;
# if not running in a builder context, a no-op.
#
# ### Usage
#
# ```bash
#   builder_set_module_has_been_built dependency-name
# ```
#
# ### Parameters
#
# * 1: `dependency-name`   the `$SCRIPT_IDENTIFIER` of the dependency
#                          (repo-relative path without leading `/`); or for
#                          external dependencies, a path-like starting with
#                          `/external/`.
#
# ### Examples
#
# ```bash
#   builder_set_module_has_been_built common/web/keyman-version
#   builder_set_module_has_been_built /external/npm-ci
# ```
#
builder_set_module_has_been_built() {
  local module="$1"

  if [[ ! -z ${_builder_deps_built+x} ]]; then
    echo "$module" >> $_builder_deps_built
  fi
}

#
# returns `0` if we should be verbose in output
#
builder_verbose() {
  if [[ $builder_verbose == --verbose ]]; then
    return 0
  fi
  return 1
}

#
# returns `0` if we are doing a debug build
#
builder_debug() {
  if [[ $builder_debug == --debug ]]; then
    return 0
  fi
  return 1
}

#
# Reports on all described dependencies, then exits
# used by builder-controls.sh
#
_builder_report_dependencies() {
  echo "${_builder_deps[@]}"
  exit 0
}

#
# Track whether functions have already been called;
# later we may use this to prevent multiple calls to, e.g.
# builder_describe
#

_builder_function_calls=()

_builder_record_function_call() {
  local func=$1
  if _builder_has_function_been_called $1; then
    # builder_die "ERROR: $func cannot be called more than once."
    return 0
  fi
  _builder_function_calls+=($1)
}

_builder_has_function_been_called() {
  local func=$1
  if _builder_item_in_array $1 "${_builder_function_calls[@]}"; then
    return 0
  fi
  return 1
}

#
# Initialize builder once all functions are declared
#
_builder_init
_builder_check_color "$@"
