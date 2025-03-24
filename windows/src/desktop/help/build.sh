#!/usr/bin/env bash
## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/builder.inc.sh"
## END STANDARD BUILD SCRIPT INCLUDE

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

builder_describe "Help documentation for Keyman for Windows" \
  clean configure build test install \
  ":web    copy documentation to bin/help/md folder. This can then be deployed with help-keyman-com.sh" \
  ":chm    convert documentation to html using pandoc and then build .chm"

builder_parse "$@"

#-------------------------------------------------------------------------------------------------------------------

source "$KEYMAN_ROOT/resources/build/win/environment.inc.sh"
source "$KEYMAN_ROOT/resources/build/win/hhc.inc.sh"

builder_describe_outputs \
  build:web          /windows/bin/help/md/desktop/index.md \
  build:chm          /windows/bin/desktop/keymandesktop.chm

#-------------------------------------------------------------------------------------------------------------------

# All content is stored in windows/docs/help

HELP_SOURCE_PATH="$KEYMAN_ROOT/windows/docs/help"
cd "$HELP_SOURCE_PATH"

MD=`find -name "*.md"`
DESTCHM="$KEYMAN_ROOT/windows/bin/help/desktop"
DESTWEB="$KEYMAN_ROOT/windows/bin/help/md/desktop"

#
# Build toc.hhc
#

build_hhc_header() {
  echo '
<HTML>
  <HEAD>
    <meta http-equiv="Content-Type" content="text/html; charset=windows-1252" />
  </HEAD>
  <BODY>
    <OBJECT type="text/site properties">
      <param name="ImageType" value="Folder" />
    </OBJECT>
' > "$DESTCHM/toc.hhc"
}

build_hhc_footer() {
  echo '
</BODY>

</HTML>
' >> "$DESTCHM/toc.hhc"
}

build_hhc_entry() {
  local FILE="$1"
  local TITLE=$(grep '<title>' < "$FILE" | sed -r 's/.*>(.+)<\/.*/\1/' | iconv -f utf-8 -t windows-1252)
  if [ -z "$TITLE" ]; then
    TITLE="$1"
  fi

  echo '
    <LI><OBJECT type="text/sitemap">
        <param name="Name" value="'"$TITLE"'" />
        <param name="Local" value="'"$FILE"'" /></OBJECT></LI>
' >> "$DESTCHM/toc.hhc"
}

build_hhc() {
  local TARGET_PATH="$1"

  if [ -f "$TARGET_PATH/index.htm" ]; then
    build_hhc_entry "$TARGET_PATH/index.htm"
  fi

  echo '
    <UL>
' >> "$DESTCHM/toc.hhc"

  for file in "$TARGET_PATH"/*; do
    if [ -d "$file" ]; then
      build_hhc "$file"
    elif [[ "$file" == */*.htm && "$file" != */index.htm ]]; then
      build_hhc_entry "$file"
    fi
  done

  echo '
    </UL>
' >> "$DESTCHM/toc.hhc"
}

#-------------------------------------------------------------------------------------------------------------------

function do_clean_chm() {
  rm -rf "$DESTCHM"
}

#
# Compile all .md to .htm
#
function do_build_chm() {
  do_clean_chm
  mkdir -p "$DESTCHM"

  #
  # Generate HTML files from Markdown
  #

  for INFILE in $MD; do
    OUTFILE="$DESTCHM/${INFILE%.md}.htm"
    echo "Processing $INFILE to $(basename "$OUTFILE")"
    mkdir -p "$(dirname "$OUTFILE")"
    pandoc -s -H "$HHC_CSS" --lua-filter="$HHC_MDLUA" -t html -o "$OUTFILE" $INFILE
  done

  #
  # Copy Images
  #

  mkdir -p "$DESTCHM/desktop_images"
  cp "$HELP_SOURCE_PATH/desktop_images"/* "$DESTCHM/desktop_images/"

  #
  # Prepare TOC and HHP files
  #

  pushd "$DESTCHM" > /dev/null

  cp "$HELP_SOURCE_PATH/keymandesktop.hhp" "$DESTCHM/keymandesktop.hhp"
  find -name '*.htm' >> "$DESTCHM/keymandesktop.hhp"

  build_hhc_header
  build_hhc .
  build_hhc_footer

  # hhc.exe returns 1 on success!
  "$HHC" keymandesktop.hhp && false || true
  cp keymandesktop.chm "$WINDOWS_PROGRAM_APP/keymandesktop.chm"

  popd > /dev/null
}

#-------------------------------------------------------------------------------------------------------------------

function do_clean_web() {
  rm -rf "$DESTWEB"
}

#
# Copy files to help.keyman.com
#
function do_build_web() {
  do_clean_web
  mkdir -p "$DESTWEB"

  for INFILE in $MD; do
    OUTFILE="$DESTWEB/$INFILE"
    mkdir -p "$(dirname "$OUTFILE")"
    cp "$INFILE" "$OUTFILE"
  done

  mkdir -p "$DESTWEB/desktop_images"
  cp "$HELP_SOURCE_PATH/desktop_images"/* "$DESTWEB/desktop_images/"
}

builder_run_action clean:chm   do_clean_chm
builder_run_action clean:web   do_clean_web
builder_run_action build:chm   do_build_chm
builder_run_action build:web   do_build_web

