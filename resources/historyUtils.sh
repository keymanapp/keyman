#!/bin/sh

_hu_base_dir=$(dirname "$BASH_SOURCE")/..

. $_hu_base_dir/resources/shellHelperFunctions.sh

get_history_file_path() {
  verify_project $1

  history_file_path="$_hu_base_dir/$1/history.md"
}

# Creates and initializes a variable called `history_file` that contains the raw content
# of the specified project's `history.md` file.
get_history_file() {
  get_history_file_path "$1"

  history_file=`cat $history_file_path`
}

### Parsing Utilities ###
enum_line_types=("published-version", "pending-version", "legacy-version", "erroneous-version", "header", "title", "whitespace", "listitem", "other")

# ${BASH_REMATCH[1]} - history.md's "Title" text.
re_title="^#[^#](.+)"

# ${BASH_REMATCH[1]} - publishing date
# ${BASH_REMATCH[2]} - version
# ${BASH_REMATCH[3]} - tier
re_published="^##[^#0-9]*([0-9]{4}-[0-9]{2}-[0-9]{2})[^0-9]*([0-9]+\.[0-9]+\.[0-9]+).*(alpha|beta|stable)"
re_v_err="^##[^#0-9]*([0-9]{4}-[0-9]{2}-[0-9]{2})[^0-9]*([0-9]+\.[0-9]+).*(alpha|beta|stable)"

# ${BASH_REMATCH[1]} - version
# ${BASH_REMATCH[2]} - tier (alpha)
re_pending="^##[^#0-9]*([0-9]+\.[0-9]+)[^\.].*(alpha|beta|stable)"

# ${BASH_REMATCH{1}} - version
# ${BASH_REMATCH{2}} - tier
re_legacy="^##[^#0-9]*([0-9]+\.[0-9]+\.[0-9]+).*(alpha|beta|stable)"

# ${BASH_REMATCH[1]} - the full line of text
# ${BASH_REMATCH[2]} - just the text after the bullet, with optional single leading space removed
re_item="([ \t]*\*[[:blank:]]?(.+))"

# Checks that it's a simple line of whitespace.
re_blank="^[[:space:]]*$"

# Checks that it's a changelog subheader for a specified version.
# ${BASH_REMATCH[1]} - header text
re_header="^###[^#](.*)"

re_v_major="^([0-9]+)\.[0-9]+"
re_v_alpha="^([0-9]+\.[0-9]+)"

###                               Core Parsing                               ###

# $1 - should be the $line being processed.
get_line_type() {
  line_type="other"

  if [[ "$1" =~ $re_title ]]; then
    line_type="title"
  elif [[ "$1" =~ $re_published ]]; then
    line_type="published-version"
  elif [[ "$1" =~ $re_pending ]]; then
    line_type="pending-version" 
  elif [[ "$1" =~ $re_legacy ]]; then
    line_type="legacy-version"
  elif [[ "$1" =~ $re_v_err ]]; then
    line_type="erroneous-version"
  elif [[ "$1" =~ $re_item ]]; then
    line_type="listitem"
  elif [[ "$1" =~ $re_blank ]]; then
   line_type="whitespace"
  elif [[ "$1" =~ $re_header ]]; then
    line_type="header"
  fi

  # This section is quite useful for debugging purposes.
  # if [[ $line_type != "other" ]]; then
  #   echo "$line_type: $1"
  # fi
}

# $1 - the markdown block to be processed.  May be the subset of a file, rather than its full body.
# $2 - a shell script command to be performed on the line.  Shell script functors!
# $3, $4 - external arguments provided to the shell script command.
process_history_loop() {
  # Grabs one line from the specified file at a time.
  #printf "%s\n" "$1" | while IFS= read -r line || [[ -n "$line" ]]; do
  while IFS= read -r line || [[ -n "$line" ]]; do
    # Do things with the $line.
    # Like, what kind of $line is this?
    $2 "$line" "$3" "$4"
  done <<< "$1"  # The triple <<< forces the loop to evaluate within the same script, preserving variable changes.
}

###                                 History Validation                            ###

# $1 - one line of history data.
validate_history_line() {
  get_line_type "$1"

  if [ $line_type = "published-version" ]; then
    if [ ${BASH_REMATCH[3]} = "alpha" ]; then
      warn "Error in entry for version ${BASH_REMATCH[2]} ${BASH_REMATCH[3]} - date and/or build number present for alpha."
      validation_error=true
    fi
  elif [ $line_type = "pending-version" ]; then
    if [[ ${BASH_REMATCH[2]} = "stable" || ${BASH_REMATCH[2]} = "beta" ]]; then
      warn "Error in entry for version ${BASH_REMATCH[1]} ${BASH_REMATCH[2]} - build number missing for ${BASH_REMATCH[3]} tier."
      validation_error=true
    fi
  elif [ $line_type = "legacy-version" ]; then
    version="${BASH_REMATCH[1]}"
    tier="${BASH_REMATCH[2]}"

    # It's 'legacy' because these come from pre-10 versions that we no longer have date-data for.
    # They'll never be releases again, so it's fine for THOSE versions - not new ones.
    [[ "${BASH_REMATCH[1]}" =~ $re_v_major ]]
    if [ $tier = "alpha" ]; then
      warn "Error in entry for version ${version} ${tier} - build number should be absent for the 'alpha' tier."
      validation_error=true
    elif [ "${BASH_REMATCH[1]}" -ge "10" ]; then
      warn "Error in entry for version ${version} ${tier} - no date present for non-legacy history."

      validation_error=true
    fi
  elif [ $line_type = "erroneous-version" ]; then
    if [ ${BASH_REMATCH[3]} = "alpha" ]; then
      warn "Error in entry for version ${BASH_REMATCH[2]} ${BASH_REMATCH[3]} - alphas should not be dated."
    else
      warn "Error in entry for version ${BASH_REMATCH[2]} ${BASH_REMATCH[3]} - build number missing for ${BASH_REMATCH[3]} tier."
    fi

    validation_error=true
  fi
}

# $1 - the product for which history.md will be validated.
validate_history_file() {
  validation_error=false
  get_history_file "$1"
  process_history_loop "${history_file}" validate_history_line

  if [ $validation_error = true ]; then
    fail "Errors detected in $1/history.md - please correct the issues noted above."
  else
    echo "$1/history.md validity check passed."
  fi
}

###                           Version Changelog Extraction                            ###

# $1 - a full version string (likely in major.minor.alpha form)
# Returns $v_alpha, containing only major.minor if it may be extracted.  Returns $1 if not.
get_alpha_version() {
  [[ $1 =~ $re_v_alpha ]]
  if [ ! -z ${BASH_REMATCH[1]} ]; then
    v_alpha=${BASH_REMATCH[1]}
  else
    v_alpha=$1
  fi
}

# Used internally by get_version_notes, passed as a pseudo-functor to process_history_loop.
get_version_helper() {
  get_line_type "$1"

  if [ $in_block = true ]; then
    if [[ $line_type = "published-version" ]]; then
      in_block=false
    elif [[ $line_type = "pending-version" || $line_type = "legacy-version" ]]; then
      in_block=false
    else
      # Might should convert these to build a string instead.  That string could then be formatted a few
      # different ways via shell-scripting, while the base retrieval stays the same.
      if [[ $line_type = "header" ]]; then
        echo "${BASH_REMATCH[1]}"
      elif [[ $line_type = "listitem" ]]; then
        echo "$1"
      elif [[ $line_type = "whitespace" ]]; then
        echo "";
      fi
    fi
  else
    v= # Must be cleared initially for the test to work correctly!
    t=
    d=

    if [[ $line_type = "published-version" ]]; then\
      d="${BASH_REMATCH[1]}"
      v="${BASH_REMATCH[2]}"
      t="${BASH_REMATCH[3]}"
    elif [[ $line_type = "pending-version" || $line_type = "legacy-version" ]]; then
      v="${BASH_REMATCH[1]}"
      t="${BASH_REMATCH[2]}"
    fi

    if [[ "$v" = "$2" && "$t" = "$3" ]]; then
      in_block=true
      version_found=true

      if [[ -n $d ]]; then
        echo "Published on $d."
      fi
    fi
  fi
}

# $1 - the product whose version notes we wish to obtain
# $2 - the version number to be obtained.
# $3 - the tier of the product to be obtained (alpha, beta, stable)
get_version_notes() {
  # State-tracking variable(s).
  in_block=false
  version_found=false

  vn_version=$2

  if [ $3 = "alpha" ]; then
    get_alpha_version "$2"
    vn_version=$v_alpha
  fi

  get_history_file "$1"
  process_history_loop "${history_file}" get_version_helper "$vn_version" "$3"

  if [ $version_found = false ]; then
    fail "Could not find changelog information for $1 version $vn_version $3."
  fi
}

###                      End Version Changelog Extraction                            ###