#!/usr/bin/env bash

# Note:  must 'source' this file again in the terminal after any edits!
_comp_builder() {
  # Does the file actually exist?  If not, abort.
  local CMD_PATH=`readlink -f "$1"`
  if [ -z "${CMD_PATH}" ]; then
    exit 0
  fi

  # Is it actually a builder-script?  If not, abort.  If so, it defines
  # a special option used to provide us with completion-target data.
  local builder_params=`${CMD_PATH} --builder_completion_describe` || exit 0

  local BASE_NAME=$(basename "$CMD_PATH")
  local BUILDER_ARG_STR="${COMP_LINE##*${BASE_NAME} }"

  local builder_args
  # Does not actually preserve an empty token at the end.
  read -r -a builder_args <<< "${BUILDER_ARG_STR}"

  # Determine the current token (given the caret position) and
  # all existing, already-completed actions, targets, and options
  # for the script before the current token.
  local current_token=
  # If the caret is adjacent to non-whitespace - thus is editing
  # a builder argument...
  if [ ! -z "${COMP_WORDS[COMP_CWORD]}" ]; then
    # Then note the last token as the token being edited...
    current_token="${builder_args[-1]}"
    # ... and thus not pre-completed.
    unset builder_args[-1]
  fi

  # Parse the builder-description for completion target data.
  local action_str target_str option_str
  IFS=";" read -r action_str target_str option_str <<< "$builder_params"
  local actions targets options
  IFS=" " read -r -a actions <<< "$action_str"
  IFS=" " read -r -a targets <<< "$target_str"
  IFS=" " read -r -a options <<< "$option_str"

  # echo "current_token=$current_token"
  local action target
  if [[ $current_token =~ : ]]; then
    IFS=: read -r action target <<< "$current_token"
    target=:$target
  else
    action="$current_token"
    target=
  fi

  # echo ""
  # echo "\$action=$action"
  # echo "\$target=$target"

  local all=()
  # If there is no $action component, it's a standalone target.

  if [[ -z "$action" ]] && [[ -z "$target" ]]; then
    all+="${actions[@]}"
    all+=" ${targets[@]}"
    all+=" ${options[@]}"

    COMPREPLY=( $(compgen -W "${all[@]}" -- "${current_token}") )
  elif [[ -z "$action" ]]; then
    # It's an unpaired target.
    all="${targets[@]}"
    COMPREPLY=( $(compgen -W "${all[@]}" -- "${current_token}") )

    # bash doesn't handle completion with colons well; we should remove the
    # colon prefix from each entry.
    local i
    for (( i=0; i<${#COMPREPLY[@]}; i++ )); do
      COMPREPLY[$i]="${COMPREPLY[$i]##*:}"
    done
  elif [[ -z "$target" ]]; then
    # It's an untargeted action or an option.
    all+="${actions[@]}"
    all+=" ${options[@]}"

    COMPREPLY=( $(compgen -W "${all[@]}" -- "${current_token}") )
  else
    # Ah, an action-target pair.  We have an existing action, so we only need to
    # complete the target part.

    # First, build up the list of legal paired tokens.
    local actiontargets=()
    for e in "${targets[@]}"; do
      actiontargets+=("${action}${e}")
    done

    # Now, complete from that.
    all+="${actiontargets[@]}"
    # echo
    # echo "$all"
    COMPREPLY=( $(compgen -W "${all[@]}" -- "${current_token}") )

    # bash doesn't handle completion with colons well; we should remove the
    # colon prefix from each entry.
    local i
    for (( i=0; i<${#COMPREPLY[@]}; i++ )); do
      COMPREPLY[$i]="${COMPREPLY[$i]##*:}"
    done
  fi
}

# When this script is sourced by .bashrc or similar, this provides autocompletion for
# builder scripts named build.sh and test.sh.
complete  -o bashdefault -F _comp_builder build.sh
complete  -o bashdefault -F _comp_builder test.sh