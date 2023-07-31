#!/usr/bin/env bash

#set -x
#DEBUG=1

_hu_base_dir=$(dirname "$BASH_SOURCE")/../../..

. $_hu_base_dir/resources/shellHelperFunctions.sh

_hu_debug() {
  if [[ ! -z ${DEBUG+x} ]] && [[ $DEBUG == 1 ]]; then echo "$1"; fi
}

get_history_file_path() {
  history_file_path="$_hu_base_dir/HISTORY.md"
}

# Creates and initializes a variable called `history_file` that contains the raw content
# of the specified project's `history.md` file.
get_history_file() {
  get_history_file_path

  history_file=`cat $history_file_path`
}

### Parsing Utilities ###
enum_line_types=("version-header", "title", "whitespace", "listitem", "other")

# Title Match: # Keyman Version History
# ${BASH_REMATCH[1]} - history.md's "Title" text.
re_title="^#[^#](.+)"

# Release Subtitle Match: ## version tier date
# Example: ## 14.0.2 alpha 2020-02-02
# ${BASH_REMATCH[1]} - version
# ${BASH_REMATCH[2]} - tier
# ${BASH_REMATCH[3]} - publishing date
re_header="^##[[:space:]]*([0-9]+\.[0-9]+\.[0-9]+)[[:space:]]*(alpha|beta|stable)[[:space:]]*([0-9]{4}-[0-9]{2}-[0-9]{2})"

# Entry in release log
# ${BASH_REMATCH[1]} - the full line of text
re_item="(.+)"

# Checks that it's a simple line of whitespace.
re_blank="^[[:space:]]*$"

re_v_major="^([0-9]+)\.[0-9]+"
re_v_alpha="^([0-9]+\.[0-9]+)"
re_v_build="^[0-9]+\.[0-9]+\.([0-9]+)"

###                               Core Parsing                               ###

# $1 - should be the $line being processed.
get_line_type() {
  line_type="other"

  if [[ "$1" =~ $re_title ]]; then
    line_type="title"
  elif [[ "$1" =~ $re_header ]]; then
    line_type="version-header"
    line_version="${BASH_REMATCH[1]}"
    line_tier="${BASH_REMATCH[2]}"
    line_date="${BASH_REMATCH[3]}"
  elif [[ "$1" =~ $re_blank ]]; then
    line_type="whitespace"
  elif [[ "$1" =~ $re_item ]]; then
    line_type="listitem"
  fi

  # This section is quite useful for debugging purposes.
  #if [[ $line_type != "other" ]]; then
     _hu_debug "$line_type: $1"
  #fi
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
    $2 "$line" "$3" "$4" "$5"
  done <<< "$1"  # The triple <<< forces the loop to evaluate within the same script, preserving variable changes.
}

###                           Version Changelog Extraction                            ###

# $1 - a full version string (likely in major.minor.build form)
# Returns $v_alpha, containing only major.minor if it may be extracted.  Returns $1 if not.
get_alpha_version() {
  [[ $1 =~ $re_v_alpha ]]
  if [ ! -z ${BASH_REMATCH[1]} ]; then
    v_alpha=${BASH_REMATCH[1]}
  else
    v_alpha=$1
  fi
}

# $1 - a full version string
# Returns $v_build, containing the final .build portion of a major.minor.build version number.
# If missing, returns an empty string.
get_build_number() {
  [[ "$1" =~ $re_v_build ]]
  if [ ! -z "${BASH_REMATCH[1]}" ]; then
    v_build="${BASH_REMATCH[1]}"
  else
    v_build=
  fi
}

# Used internally by get_version_notes, passed as a pseudo-functor to process_history_loop.
get_version_helper() {
  local line="$1"
  local version="$2"
  local tier="$3"
  local product="$4"
  get_line_type "$line"

  if [ $in_block = true ]; then
    if [[ $line_type = "version-header" ]]; then
      in_block=false
    elif [[ $line_type = "listitem" ]]; then
      if [[ "$product" == "ios" ]]; then
        # We have to filter out other platforms to satisfy Apple
        if [[ "$line" =~ \(ios|web|common ]]; then
          echo "$line" | sed -e "s/Android/mobile platform/"
        fi
      else
        if [[ "$line" =~ "\($product|web|common" ]]; then
          echo "$line"
        fi
      fi
    elif [[ $line_type = "whitespace" ]]; then
      echo "";
    fi
  else
    v= # Must be cleared initially for the test to work correctly!
    t=
    d=

    if [[ $line_type = "version-header" ]]; then
      d="$line_date"
      v="$line_version"
      t="$line_tier"
    fi

    # Splits the version numbers into their components to facilitate finding
    # 1) the FIRST version in history.md
    # 2) with equal to or lesser version
    # 3) of the same tier.
    if [[ "$v" != "" && "$t" = "$tier" && $version_found = false ]]; then
      get_alpha_version "$v"
      get_build_number "$v"

      found_v="$v_alpha"
      found_b="$v_build"

      get_alpha_version "$version"
      get_build_number "$version"

      search_v="$v_alpha"
      search_b="$v_build"

      if [[ "$found_v" = "$search_v" && ( $found_b -le $search_b ) ]]; then
        in_block=true
        version_found=true

        if [[ -n $d ]]; then
          echo "Published on $d."
        fi
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

  local vn_product=$1
  local vn_version=$2
  local vn_tier=$3

  get_history_file
  process_history_loop "${history_file}" get_version_helper "$vn_version" "$vn_tier" "$vn_product"

  if [ $version_found = false ]; then
    builder_die "Could not find changelog information for $vn_product version $vn_version $vn_tier."
  fi
}

###                      End Version Changelog Extraction                            ###