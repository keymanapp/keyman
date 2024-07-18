#!/usr/bin/env bash
#
# This script contains utilities for converting markdown help documentation to htm/html.
# The apps can then include the htm/html files in the installation to display
# help within the app.
# Expected platforms: android, ios, mac
# Note: linux app doesn't need to convert from markdown to html
#

# Clean the htm/html files and recreate the output path
# ### Parameters
#
# * `platform`     the platform to build, matching the platform folder name
# * `output_path`  path to emit files to, relative to `$KEYMAN_ROOT/$platform`
#
function _build_help_clean() {
  echo "Cleaning"
  local PLATFORM="$1"
  local DESTHTM="$KEYMAN_ROOT/$PLATFORM/$2"

  echo "rm DESTHTM: $DESTHTM"
  rm -rf "$DESTHTM"
  mkdir -p "$DESTHTM"
}


#
# Compile all .md to .html
# ### Parameters
#
# * `platform`     the platform to build, matching the platform folder name
# * `output_path`  path to emit files to, relative to `$KEYMAN_ROOT/$platform`
#
function _build_help_build() {
  local MDLUA="$KEYMAN_ROOT/resources/build/html-link.lua"
  local CSS="$KEYMAN_ROOT/resources/build/offline-help-style-spec.txt"
  local PLATFORM="$1"

  cd "$KEYMAN_ROOT/$PLATFORM/help"
  local MD=`find . -name "*.md"`
  local DESTHTM="$KEYMAN_ROOT/$PLATFORM/$2"

  #
  # Generate HTML files from Markdown
  #
  for INFILE in $MD; do
    OUTFILE="$DESTHTM/${INFILE%.md}.html"
    echo "Processing $INFILE to $(basename "$OUTFILE")"
    mkdir -p "$(dirname "$OUTFILE")"
    pandoc -s -H "$CSS" --lua-filter="$MDLUA" -t html -o "$OUTFILE" $INFILE
  done

  #
  # Copy Images
  #
  cd "$KEYMAN_ROOT/$PLATFORM/help/"
  mkdir -p "$DESTHTM/${PLATFORM}_images"
  cp $KEYMAN_ROOT/$PLATFORM/help/${PLATFORM}_images/* "$DESTHTM/${PLATFORM}_images/"
}

#
# Builds .html files from .md source
#
# ### Usage
#
# ```bash
#   build_help_html platform output_path
# ```
#
# ### Parameters
#
# * `platform`     the platform to build, matching the platform folder name
# * `output_path`  path to emit files to, relative to `$KEYMAN_ROOT/$platform`
#
# ### Description
#
# Expects to find .md source under $KEYMAN_ROOT/$platform/help/, and
# will write the output .html to $KEYMAN_ROOT/$platform/$output_path.
#
function build_help_html() {
  pushd $(pwd)
  _build_help_clean $1 $2
  _build_help_build $1 $2
  popd
}
