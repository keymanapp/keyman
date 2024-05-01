#!/usr/bin/env bash

# Note: these two lines can be uncommented for debugging and profiling build
# scripts:
#
#   set -x
#   PS4='+ $EPOCHREALTIME $0 $LINENO   '
#

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

# Exit on command failure and when using unset variables:
set -eu

#
# Prevents 'clear' on exit of mingw64 bash shell
#
SHLVL=0

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

  # All build scripts start in their own folder
  cd "$THIS_SCRIPT_PATH"
}

function _builder_findRepoRoot() {
    # We don't need readlink here because our standard script prolog does a
    # readlink -f already so we will have already escaped from any symlinks
    REPO_ROOT="${BASH_SOURCE[0]%/*/*}"
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
#   THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
#   . "${THIS_SCRIPT%/*}/resources/builder.inc.sh"
#   ## END STANDARD BUILD SCRIPT INCLUDE
# ```
#
function _builder_setBuildScriptIdentifiers() {
  if [ ! -z ${THIS_SCRIPT+x} ]; then
    THIS_SCRIPT_PATH="${THIS_SCRIPT%/*}"
    readonly THIS_SCRIPT_PATH
    THIS_SCRIPT_NAME="${THIS_SCRIPT##*/}"
    readonly THIS_SCRIPT_NAME
    # Leaves only the part of the path based upon REPO_ROOT.
    THIS_SCRIPT_IDENTIFIER=${THIS_SCRIPT_PATH#"$REPO_ROOT/"}
    readonly THIS_SCRIPT_IDENTIFIER
  else
    builder_die "THIS_SCRIPT not defined; builder.inc.sh has not been sourced with standard script include."
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
    # Using esc codes instead of tput for performance
    COLOR_RED='\x1b[31m'                # $(tput setaf 1)
    COLOR_GREEN='\x1b[32m'              # $(tput setaf 2)
    COLOR_YELLOW='\x1b[33m'             # $(tput setaf 3)
    COLOR_BLUE='\x1b[34m'               # $(tput setaf 4)
    COLOR_PURPLE='\x1b[35m'             # $(tput setaf 5)
    COLOR_TEAL='\x1b[36m'               # $(tput setaf 6)
    COLOR_WHITE='\x1b[38;5;252m'        # $(tput setaf 252)
    COLOR_BRIGHT_WHITE='\x1b[38;5;255m' # $(tput setaf 255)
    COLOR_GREY='\x1b[90m'               # $(tput setaf 8)
    COLOR_RESET='\x1b(B\x1b[m'          # $(tput sgr0)
    # e.g. VSCode https://code.visualstudio.com/updates/v1_69#_setmark-sequence-support
    BUILDER_BOLD='\x1b[1m'              # $(tput bold)
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
    COLOR_BRIGHT_WHITE=
    COLOR_GREY=
    COLOR_RESET=
    BUILDER_BOLD=
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
    builder_echo error "Unspecified error, aborting script"
  else
    builder_echo error "$*"
  fi
  echo
  exit 1
}

function builder_warn() {
  builder_echo warning "$*"
}

function builder_heading() {
  builder_echo heading "$*"
}


####################################################################################
#
# builder_ functions for standard build script parameter and process management
#
####################################################################################


builder_echo() {
  local color=white message= mark=
  if [[ $# -gt 1 && $1 =~ ^(white|grey|green|success|blue|heading|yellow|warning|red|error|purple|brightwhite|teal|debug|setmark)$ ]]; then
    color="$1"
    shift
  fi
  message="$*"

  if [[ ! -z ${COLOR_RED+x} ]]; then
    case $color in
      white) color="$COLOR_WHITE" ;;
      grey) color="$COLOR_GREY" ;;
      green|success) color="$COLOR_GREEN" ;;
      blue|heading) color="$COLOR_BLUE" ;;
      yellow|warning) color="$COLOR_YELLOW" ;;
      red|error) color="$COLOR_RED" ;;
      purple) color="$COLOR_PURPLE" ;;
      brightwhite) color="$COLOR_BRIGHTWHITE" ;;
      teal|debug) color="$COLOR_TEAL" ;;
      setmark) mark="$HEADING_SETMARK" color="$COLOR_PURPLE" ;;
    esac

    if builder_is_dep_build; then
      echo -e "$mark$COLOR_GREY[$THIS_SCRIPT_IDENTIFIER]$COLOR_RESET $color$message$COLOR_RESET"
    else
      echo -e "$mark$BUILDER_BOLD$COLOR_BRIGHT_WHITE[$THIS_SCRIPT_IDENTIFIER]$COLOR_RESET $color$message$COLOR_RESET"
    fi
  else
    # Cope with the case of pre-init message and just emit plain text
    echo -e "$message"
  fi
}

builder_echo_debug() {
  builder_echo debug "[DEBUG] $*"
}

#
# builder_ names are reserved.
# _builder_ names are internal use and subject to change
#

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

#
# Expands a shorthand item into a full match from an array of possibilities;
# reports an error if there are ambiguous options. Note that this function
# returns the number of matches, so 0 = no match, 1 = a precise match.
#
# ### Parameters
#
# * 1: `item`       item to search for in array, e.g. "t"
# * 2: `array`      bash array, e.g. `array=(one two three)`
#
# ### Description
#
# Does a substring search by regex for
#
# ### Example
#
# ```bash
#   actions=(clean configure build test)
#
#   action=`_builder_expand_shorthand $1 "${actions[@]}"` &&
#     builder_die "Unrecognized parameter $1" ||
#     case $? in
#       1) echo "Parameter $1 matches {$action}"
#       ;;
#       *) builder_die "Parameter $1 has $? matches, could mean any of {$action}"
#     esac
# ```
#
_builder_expand_shorthand() {
  local item=$1
  shift
  local count=0
  local result=
  local string=
  for e; do
    if [[ $e == $item ]]; then
      # Exact match trumps substring matches
      echo $item
      return 1
    fi
    if [[ $e == "$item"* ]]; then
      count=$((count+1))
      if [[ $count == 2 ]]; then
        string="$result, $e"
        result=$item
      elif [[ $count -gt 2 ]]; then
        string="$string, $e"
      else
        result=$e
      fi
    fi
  done

  if [[ $count -lt 2 ]]; then
    echo $result
  else
    echo $string
  fi
  return $count
}


_builder_item_is_target() {
  local item="$1"
  [[ $item =~ ^: ]] && return 1
  return 0
}

function _builder_warn_if_incomplete() {
  if [ -n "${_builder_current_action}" ]; then
    builder_echo warning "$_builder_current_action never reported success or failure"
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
      IFS=: read -r action target <<< "$action"
      target=:$target
    else
      target=:project
    fi

    builder_finish_action failure $action$target
  fi

  # Make 100% sure that the exit code chains fully.
  # Without this, nested scripts have failed to chain errors from npm calls past the script
  # that directly executed the failed npm command.
  exit $trappedExitCode
}

#
# Removes temporary `_builder_deps_built` file when top-level build script
# finishes.
#
_builder_cleanup_deps() {
  if ! builder_is_dep_build && ! builder_is_child_build && [[ ! -z ${_builder_deps_built+x} ]]; then
    if $_builder_debug_internal; then
      builder_echo_debug "Dependencies that were built:"
      cat "$_builder_deps_built"
    fi
    rm -f "$_builder_deps_built"
    _builder_deps_built=
  fi
}

#------------------------------------------------------------------------------------------
# Child scripts
#------------------------------------------------------------------------------------------

_builder_execute_child() {
  local action=$1
  local target=$2

  local script="$THIS_SCRIPT_PATH/${_builder_target_paths[$target]}/build.sh"

  if $_builder_debug_internal; then
    builder_echo heading "## $action$target starting..."
  fi

  # Build array of specified inheritable options
  local child_options=()
  local opt
  for opt in "${_builder_options_inheritable[@]}"; do
    if builder_has_option $opt; then
      child_options+=($opt)
    fi
  done

  "$script" $action \
    --builder-child \
    $_builder_build_deps \
    ${child_options[@]} \
    $builder_verbose \
    $builder_debug \
  && (
    if $_builder_debug_internal; then
      builder_echo success "## $action$target completed successfully"
    fi
  ) || (
    result=$?
    builder_echo error "## $action$target failed with exit code $result"
    exit $result
  ) || exit $? # Required due to above subshell masking exit
}

_builder_run_child_action() {
  local action="$1" target

  if [[ $action =~ : ]]; then
    IFS=: read -r action target <<< "$action"
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
          if [[ ! -z ${_builder_target_paths[$target]+x} ]] &&
            [[ -f "$THIS_SCRIPT_PATH/${_builder_target_paths[$target]}/build.sh" ]]; then
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

#------------------------------------------------------------------------------------------
# Various API endpoints
#------------------------------------------------------------------------------------------

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
    IFS=: read -r action target <<< "$action"
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
# Wraps builder_start_action and builder_finish action for single-command
# actions. Can be used together with a local function for multi-command actions.
# Do be aware that this pseudo-closure style cannot be mixed with operators such
# as `<`, `>`, `&&`, `;`, `()` and so on.
#
# ### Usage
#
# ```bash
#   builder_run_action action[:target] command [command-params...]
# ```
#
# ### Parameters
#
# * 1: `action[:target]`   name of action, and optionally also target, if target
#                          excluded starts for all defined targets
# * 2: command             command to run if action is started
# * 3...: command-params   parameters for command
#
# ### Example
#
# ```bash
#   function do_build() {
#     mkdir -p build/cjs-src
#     npm run build
#   }
#
#   builder_run_action clean        rm -rf ./build/ ./tsconfig.tsbuildinfo
#   builder_run_action configure    verify_npm_setup
#   builder_run_action build        do_build
# ```
#
function builder_run_action() {
  local action=$1
  shift
  if builder_start_action $action; then
    ($@)
    builder_finish_action success $action
  fi
  return 0
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
  if builder_has_action $1; then
    # In a dependency quick build (the default), determine whether we actually
    # need to run this step. Uses data passed to builder_describe_outputs to
    # verify whether a target output is present.
    if builder_is_dep_build &&
        ! builder_is_full_dep_build &&
        _builder_dep_output_exists $_builder_matched_action; then
      builder_echo "skipping $_builder_matched_action_name, up-to-date"
      return 1
    fi

    builder_echo blue "## $_builder_matched_action_name starting..."
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

#
# Trims leading and following whitespace from the input parameters
#
# ### Usage
#
# ```bash
#   my_string="$(builder_trim "$my_string")"
# ```
#
# ### Parameters
#
# * `my_string`    An input string
#
builder_trim() {
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
# `$REPO_ROOT`. Assumes realpath is installed (brew coreutils on macOS).
#
_builder_expand_relative_path() {
  local path="$1"
  if [[ "$path" =~ ^/ ]]; then
    echo "${path:1}"
  else
    realpath --canonicalize-missing --relative-to="$REPO_ROOT" "$THIS_SCRIPT_PATH/$path"
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
    IFS=":" read -r action target <<< "$input"
  else
    action="$input"
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
    e="$(_builder_expand_action_target "$e")"
    output+=("$e")
  done
  if [[ ${#output[@]} == 0 ]]; then
    echo "*:*"
  else
    echo "${output[@]}"
  fi
}

_builder_child_base=
#
# Describes the path from the build script's working directory to the common subfolder
# containing child scripts / projects without defined custom paths.
#
# This function must be called to set the child base path before builder_describe is
# called in order to work correctly.  Furthermore, note that this setting will be
# ignored by targets with custom paths.
#
# ### Usage
#
# ```bash
#    builder_set_child_base path
# ```
#
# ### Parameters
#
#  * `path`  The relative path from the directory containing the calling script to
#            the base folder to use for child-project detection and resolution
#
builder_set_child_base() {
  _builder_child_base="$1/"
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
# * **Option:** `"--option[,-o][+][=var]   [One line description]"`
#
#   All options must have a longhand form with two prefix hyphens,
#   e.g. `--option`. The `,-o` shorthand form is optional. When testing if
#   the option is set with `builder_has_option`, always use the longhand
#   form.
#
#   If `=var` is specified, then the next parameter will be a variable stored in
#   `$var` for that option. e.g. `--option=opt` means `$opt` will have the value
#   `"foo"` when the script is called for `--option foo`.
#
#   If `+` is specified, then the option will be passed to child scripts. All
#   child scripts _must_ accept this option, or they will fail. It is acceptable
#   for the child script to declare the option but ignore it. However, the option
#   will _not_ be passed to dependencies.
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
# * **Dependency:** `"@/path/to/dependency[:target] [action][:target] ..."``
#
#   A dependency always starts with `@`. The path to the dependency will be
#   relative to the build script folder, or to the root of the repository, if
#   the path starts with `/`, not to the root of the file system. It is an error
#   to specify a dependency outside the repo root.
#
#   Relative paths will be expanded to full paths, again, relative to the root
#   of the repository.
#
#   A dependency definition can include a target for that dependency, for
#   example, `"@/core:arch"`. This would build only the ':arch' target for the
#   core module.
#
#   Dependencies may be limited to specific `action:target` pairs on the current
#   script. If not specified, dependencies will be built for all actions on all
#   targets. Either `action` or `:target` may be omitted, and multiple actions
#   and targets may be specified, space separated.
#
builder_describe() {
  _builder_record_function_call builder_describe

  _builder_description="$1"
  _builder_actions=()
  _builder_targets=()
  _builder_options=()
  _builder_deps=()                    # array of all dependencies for this script
  _builder_options_inheritable=()     # array of all options that should be passed to child scripts
  _builder_default_action=build
  declare -A -g _builder_params
  declare -A -g _builder_options_short
  declare -A -g _builder_options_var
  declare -A -g _builder_dep_path             # array of output files for action:target pairs
  declare -A -g _builder_dep_related_actions  # array of action:targets associated with a given dependency
  declare -A -g _builder_internal_dep         # array of internal action:targets dependency relationships
  declare -A -g _builder_target_paths         # array of target child project paths
  declare -A -g _builder_dep_targets          # array of :targets given for a specific dependency (comma separated if more than one)
  shift
  local sub=()
  # describe each target, action, and option possibility
  while [[ $# -gt 0 ]]; do
    local key="$1"
    local value="$key"
    local description=
    if [[ $key =~ [[:space:]] ]]; then
      IFS=" " read -r -a sub <<< "$key"
      value="${sub[0]}"
      description="$(builder_trim "${sub[@]:1}")"
    fi

    local original_value="$value"

    if [[ $value =~ ^: ]]; then
      # Parameter is a target
      local target_path=
      if [[ $value =~ = ]]; then
        # The target has a custom child project path
        IFS="=" read -r -a sub <<< "$value"
        target_path="${sub[@]:1}"
        value="${sub[0]}"
        if [[ ! -d "$THIS_SCRIPT_PATH/$target_path" ]]; then
          builder_die "Target path '$target_path' for $value does not exist."
        fi
      else
        # If the target name matches a folder name, implicitly
        # make it available as a child project
        if [[ -d "$THIS_SCRIPT_PATH/$_builder_child_base${value:1}" ]]; then
          target_path="$_builder_child_base${value:1}"
        fi
      fi
      _builder_targets+=($value)
      if [[ ! -z "$target_path" ]]; then
        _builder_target_paths[$value]="$target_path"
      fi
    elif [[ $value =~ ^@ ]]; then
      # Parameter is a dependency
      local dependency="${value:1}"
      local dependency_target= # all targets
      if [[ $dependency =~ : ]]; then
        IFS=":" read -r dependency dependency_target <<< "$dependency"
        dependency_target=":$dependency_target"
      fi

      dependency="`_builder_expand_relative_path "$dependency"`"
      _builder_deps+=($dependency)
      _builder_dep_related_actions[$dependency]="$(_builder_expand_action_targets "$description")"
      _builder_dep_targets[$dependency]="$dependency_target"
      # We don't want to add deps to params, so shift+continue
      shift
      continue
    elif [[ $value =~ ^-- ]]; then
      # Parameter is an option
      # Look for a shorthand version of the option
      local option_var=
      if [[ $value =~ = ]]; then
        IFS="=" read -r -a sub <<< "$value"
        option_var="${sub[@]:1}"
        value="${sub[0]}"
      fi

      local is_inheritable=false

      if [[ $value =~ \+$ ]]; then
        # final + indicates that option is inheritable
        is_inheritable=true
        value="${value:0:-1}"
      fi

      if [[ $value =~ , ]]; then
        IFS="," read -r -a sub <<< "$value"
        local option_long="${sub[0]}"
        local option_short="${sub[@]:1}"
        _builder_options+=($option_long)
        if $is_inheritable; then
          _builder_options_inheritable+=($option_long)
        fi
        _builder_options_short[$option_short]="$option_long"
        if [[ ! -z "$option_var" ]]; then
          _builder_options_var[$option_long]="$option_var"
        fi
        value="$option_long, $option_short"
      else
        _builder_options+=($value)
        if $is_inheritable; then
          _builder_options_inheritable+=($value)
        fi
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
      description=$(_builder_get_default_description "$original_value")
    fi
    _builder_params[${original_value}]="$description"

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
#     configure   /node_modules \
#     build       build/index.js
# ```
#
function builder_describe_outputs() {
  _builder_record_function_call builder_describe_outputs

  while [[ $# -gt 0 ]]; do
    local key="$1" path="$2" action= target=
    path="`_builder_expand_relative_path "$path"`"

    if [[ $key =~ : ]]; then
      IFS=":" read -r action target <<< "$key"
      target=":$target"
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
  builder_echo red "$program: invalid $type: $param"
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
  local action_target i=0 new_actions=()

  # Iterate through every action specified on command line; we use this loop
  # style so that any new actions added here will also be iteratively checked
  while (( $i < ${#_builder_chosen_action_targets[@]} )); do
    action_target=${_builder_chosen_action_targets[$i]}

    # If we have an internal dependency for the chosen action:target pair
    if [[ ! -z ${_builder_internal_dep[$action_target]+x} ]]; then
      local dep_outputs=(${_builder_internal_dep[$action_target]}) dep_output
      for dep_output in "${dep_outputs[@]}"; do
        # If there is a defined output for this dependency
        if [[ ! -z ${_builder_dep_path[$dep_output]+x} ]]; then
          # If the output for the dependency is missing, or we have --force-deps
          if [[ ! -e "$REPO_ROOT/${_builder_dep_path[$dep_output]}" ]] || builder_is_full_dep_build; then
            # Add the dependency to the chosen action:target list
            if ! _builder_item_in_array "$dep_output" "${_builder_chosen_action_targets[@]}"; then
              _builder_chosen_action_targets+=($dep_output)
              new_actions+=($dep_output)
            fi
          fi
        fi
      done
    fi
    i=$((i + 1))
  done

  if [[ ${#new_actions[@]} -gt 0 ]]; then
    if builder_is_full_dep_build; then
      builder_echo "Automatically running all dependency actions due to --force-deps:"
    else
      builder_echo "Automatically running following required actions with missing outputs:"
    fi
    for e in "${new_actions[@]}"; do
      builder_echo "* $e"
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
    [[ -z ${_builder_internal_dep[$action$target]+x} ]] &&
      _builder_internal_dep[$action$target]=$dep$target ||
      _builder_internal_dep[$action$target]="${_builder_internal_dep[$action$target]} $dep$target"
  fi
}

#
# Define a local dependency between one action:target and
# another.
#
# Usage:
#   builder_describe_internal_dependency action:target depaction:deptarget ...
# Parameters:
#   1: action:target         The action and target that has a dependency
#   2: depaction:deptarget   The dependency action and target
# Example:
#   builder_describe_internal_dependency \
#     build:mac build:mac-x86_64 \
#     build:mac build:mac-arm64
#
# Note: actions and targets must be fully specified, and this _must_
# be called before either builder_describe_outputs or builder_parse in
# order for dependencies to be resolved.
builder_describe_internal_dependency() {
  while [[ $# -gt 0 ]]; do
    local action_target=$1 dep_action_target=$2
    [[ -z ${_builder_internal_dep[$action_target]+x} ]] &&
      _builder_internal_dep[$action_target]=$dep_action_target ||
      _builder_internal_dep[$action_target]="${_builder_internal_dep[$action_target]} $dep_action_target"
    shift 2
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
  _builder_record_function_call builder_parse

  local exp=()
  builder_extra_params=()

  while [[ $# -gt 0 ]] ; do
    local action= target=
    local key="$1"
    if [[ $key == "--" ]]; then
      shift
      builder_extra_params=("$@")
      break
    fi

    if [[ $key =~ ^- ]]; then

      # Expand short -o to --option in options lookup
      if [[ ! -z ${_builder_options_short[$key]+x} ]]; then
        key=${_builder_options_short[$key]}
      fi

      exp+=($key)
      if [[ ! -z ${_builder_options_var[$key]+x} ]]; then
        shift
        if [[ $# -eq 0 ]]; then
          _builder_parameter_error "$0" parameter "$key"
        fi

        exp+=("$1")
      fi
    else
      # Expand comma separated values
      if [[ $key =~ : ]]; then
        IFS=: read -r action target <<< "$key"
      else
        action="$key"
        target=
      fi

      local actions targets
      IFS=, read -r -a actions <<< "$action"
      IFS=, read -r -a targets <<< "$target"

      if [[ "${#actions[@]}" -eq 0 ]]; then
        # No actions, so must be at least one :target
        for target in "${targets[@]}"; do
          exp+=(:$target)
        done
      else
        for action in "${actions[@]}"; do
          if [[ "${#targets[@]}" -eq 0 ]]; then
            # No :targets so just expand actions
            exp+=($action)
          else
            # Actions:targets, expand them all
            for target in "${targets[@]}"; do
              exp+=($action:$target)
            done
          fi
        done
      fi
    fi

    shift
  done

  _builder_parse_expanded_parameters "${exp[@]}"
}

_builder_parse_expanded_parameters() {
  _builder_build_deps=--deps
  builder_verbose=
  builder_debug=
  local _params=($@)
  _builder_chosen_action_targets=()
  _builder_chosen_options=()
  _builder_current_action=
  _builder_is_child=1

  local n=0

  # Process command-line arguments
  while [[ $# -gt 0 ]] ; do
    local key="$1"
    local action=
    local target=
    local e has_action has_target has_option longhand_option

    if [[ $key =~ : ]]; then
      IFS=: read -r action target <<< "$key"
      target=:$target
    else
      action="$key"
      target=
    fi

    # Expand shorthand parameters

    new_action=$(_builder_expand_shorthand $action "${_builder_actions[@]}") ||
      case $? in
        1)
          action=$new_action
          ;;
        *)
          builder_warn "Parameter $action has $? matches, could mean any of {$new_action}"
          exit 1
          ;;
      esac

    new_target=$(_builder_expand_shorthand $target "${_builder_targets[@]}") ||
      case $? in
        1)
          target=$new_target
          ;;
        *)
          builder_warn "Parameter $target has $? matches, could mean any of {$new_target}"
          exit 1
          ;;
      esac

    _builder_item_in_array "$action" "${_builder_actions[@]}" && has_action=1 || has_action=0
    _builder_item_in_array "$target" "${_builder_targets[@]}" && has_target=1 || has_target=0

    if (( has_action )) || (( has_target )); then
      # Document parameter expansion for end use
      _params[$n]=$action$target
    fi
    n=$((n + 1))

    _builder_item_in_array "$key" "${_builder_options[@]}" && has_option=1 || has_option=0

    if (( has_action )) && (( has_target )); then
      # apply the selected action and selected target
      _builder_chosen_action_targets+=("$action$target")
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
        --builder-child)
          _builder_is_child=0
          ;;
        --builder-report-dependencies)
          # internal reporting function, ignores all other parameters
          _builder_report_dependencies
          ;;
        *)
          # script does not recognize anything of action or target form at this point.
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

  if $_builder_debug_internal; then
    builder_echo_debug "Selected actions and targets:"
    for e in "${_builder_chosen_action_targets[@]}"; do
      builder_echo_debug "* $e"
    done
    builder_echo_debug
    builder_echo_debug "Selected options:"
    for e in "${_builder_chosen_options[@]}"; do
      builder_echo_debug "* $e"
    done
  fi

  if builder_is_dep_build; then
    builder_echo setmark "dependency build, started by $builder_dep_parent"
    builder_echo grey "build.sh parameters: <${_params[@]}>"
    if [[ -z ${_builder_deps_built+x} ]]; then
      builder_die "FATAL ERROR: Expected '_builder_deps_built' variable to be set"
    fi
  elif builder_is_child_build; then
    builder_echo setmark "child build, parameters: <${_params[@]}>"
    if [[ -z ${_builder_deps_built+x} ]]; then
      builder_die "FATAL ERROR: Expected '_builder_deps_built' variable to be set"
    fi
  else
    # This is a top-level invocation, so we want to track which dependencies
    # have been built, so they don't get built multiple times.
    builder_echo setmark "build.sh parameters: <${_params[@]}>"
    if [[ ${#builder_extra_params[@]} -gt 0 ]]; then
      builder_echo grey "build.sh extra parameters: <${builder_extra_params[@]}>"
    fi
    export _builder_deps_built=`mktemp`
  fi

  if builder_is_debug_build; then
    BUILDER_CONFIGURATION=debug
  else
    BUILDER_CONFIGURATION=release
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
    if [[ ! -z "${_builder_params[$e]+x}" ]]; then
      description="${_builder_params[$e]}"
    else
      description=$(_builder_get_default_description "$e")
    fi
    _builder_pad $width "  $e" "$description"
  done

  echo
  echo "Targets: "

  for e in "${_builder_targets[@]}"; do
    if [[ ! -z "${_builder_params[$e]+x}" ]]; then
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
  echo -e "* Specify ${c1}action:target${c0} to run a specific ${c1}action${c0} against a specific ${c1}:target${c0}."
  echo -e "* If ${c1}action${c0} is specified without a ${c1}target${c0} suffix, it will be applied to all ${c1}:target${c0}s."
  echo -e "* If ${c1}:target${c0} is specified without an ${c1}action${c0} prefix, ${c1}$_builder_default_action:target${c0} will be inferred."
  echo -e "* If no ${c1}action${c0}, ${c1}:target${c0}, or ${c1}action:target${c0} entries are specified, ${c1}$_builder_default_action${c0} will run on all ${c1}:target${c0}s."
  echo
}

builder_finish_action() {
  local result="$1"
  local action="$2" target action_name

  if [[ $action =~ : ]]; then
    IFS=: read -r action target <<< "$action"
    target=":$target"
  else
    target=':*'
  fi

  if [[ "$target" == ":*" ]]; then
    action_name="$action"
  else
    action_name="$action$target"
  fi

  local matched_action="$action$target"

  if [[ "$matched_action" == "${_builder_current_action}" ]]; then
    if [[ $result == success ]]; then
      # Sanity check:  if there is a described output for this action, does the corresponding
      # file or directory exist now?
      if _builder_dep_output_defined $matched_action && ! _builder_dep_output_exists "$matched_action"; then
        builder_echo warning "Expected output: '${_builder_dep_path[$matched_action]}'."
        builder_echo warning "## $action_name completed successfully, but output does not exist"
      else
        builder_echo success "## $action_name completed successfully"
      fi
    elif [[ $result == failure ]]; then
      builder_echo error "## $action_name failed"
    else
      builder_echo error "## $action_name failed with message: $result"
    fi

    # Remove $action$target from the array; it is no longer a current action
    _builder_current_action=
  else
    builder_echo warning "reporting result of $action_name but the action was never started!"
  fi
}

#------------------------------------------------------------------------------------------
# Dependencies
#------------------------------------------------------------------------------------------

_builder_dep_output_defined() {
  if [[ ! -z ${_builder_dep_path[$1]+x} ]]; then
    return 0
  else
    return 1
  fi
}

_builder_dep_output_exists() {
  if _builder_dep_output_defined $1 && [[ -e "$REPO_ROOT/${_builder_dep_path[$1]}" ]]; then
    return 0
  else
    return 1
  fi
}

#
# Returns `0` if the dependency should be built for the given action:target
#
_builder_should_build_dep() {
  local action_target="$1"
  local dep="$2"
  local related_actions=(${_builder_dep_related_actions[$dep]})

  if [[ $action_target =~ ^clean ]]; then
    # don't attempt to build dependencies for a 'clean' action
    return 1
  fi

  if ! _builder_item_in_glob_array "$action_target" "${related_actions[@]}"; then
    return 1
  fi
  return 0
}

#
# Removes a dependency from the list of available dependencies
#
# Parameters:
#   $1    path to dependency
#
builder_remove_dep() {
  local dependency="$1" i
  dependency="`_builder_expand_relative_path "$dependency"`"

  for i in "${!_builder_deps[@]}"; do
    if [[ ${_builder_deps[i]} = $dependency ]]; then
      unset '_builder_deps[i]'
    fi
  done

  # rebuild the array to remove the empty item
  _builder_deps=( "${_builder_deps[@]}" )
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
      builder_echo "Skipping dependency $dep for $_builder_matched_action_name"
      continue
    fi

    # Only configure and build the dependency once per invocation
    if builder_has_module_been_built "$dep"; then
      continue
    fi

    dep_target=
    if [[ ! -z ${_builder_dep_targets[$dep]+x} ]]; then
      # TODO: in the future split _builder_dep_targets into comma-separated
      #       array for multiple targets for a dep?
      dep_target=${_builder_dep_targets[$dep]}
    fi

    builder_set_module_has_been_built "$dep"
    "$REPO_ROOT/$dep/build.sh" "configure$dep_target" "build$dep_target" \
      $builder_verbose \
      $builder_debug \
      $_builder_build_deps \
      --builder-dep-parent "$THIS_SCRIPT_IDENTIFIER" && (
      if $_builder_debug_internal; then
        builder_echo success "## Dependency $dep for $_builder_matched_action_name successfully"
      fi
    ) || (
      result=$?
      builder_echo error "## Dependency failed with exit code $result"
      exit $result
    ) || exit $? # Required due to above subshell masking exit
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
# returns `0` if we are in a child script doing a build
#
builder_is_child_build() {
  return $_builder_is_child
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
# Reports on all described dependencies, then exits
# used by builder-controls.sh
#
_builder_report_dependencies() {
  echo "${_builder_deps[@]}"
  exit 0
}

#------------------------------------------------------------------------------------------
# Utility functions
#------------------------------------------------------------------------------------------

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
builder_is_debug_build() {
  if [[ $builder_debug == --debug ]]; then
    return 0
  fi
  return 1
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

# _builder_debug_internal flag can be used to emit verbose logs for builder itself,
# e.g.:
#   _builder_debug_internal=true ./build.sh
#
if [ -z ${_builder_debug_internal+x} ]; then
  _builder_debug_internal=false
fi

if $_builder_debug_internal; then
  builder_echo_debug "Command line: $0 $@"
fi
