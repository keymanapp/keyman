#!/usr/bin/env bash

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

QUIET=0

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

THIS_DIR="$(dirname "$THIS_SCRIPT")"

display_usage() {
  echo "build.sh [--no-clean] target [...target]"
  echo "Builds help documentation for Keyman for Windows"
  echo "Targets:"
  echo "  * web: copy documentation to bin/help/php folder. This can then be"
  echo "         deployed with help-keyman-com.sh"
  echo "  * chm: convert documentation to html using pandoc and then build .chm"
  echo
  echo " --no-clean: don't clean target folder before building"
}

DO_CHM=false
DO_WEB=false
DO_CLEAN=true

# Debug flags
DO_CHM_CONVERSION=true

#
# Parse args
#

shopt -s nocasematch

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    chm)
      DO_CHM=true
      ;;
    web)
      DO_WEB=true
      ;;
    --no-clean)
      DO_CLEAN=false
      ;;
    *)
      display_usage
      exit 1
  esac
  shift # past argument
done

if ! $DO_WEB && ! $DO_CHM ; then
  display_usage
  exit 1
fi

displayInfo "" \
  "DO_CHM: $DO_CHM" \
  "DO_WEB: $DO_WEB" \
  "DO_CLEAN: $DO_CLEAN" \
  ""

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

#
# Compile all .md to .htm
#

MDLUA="$KEYMAN_ROOT/resources/build/htm-link.lua"
CSS="../../../../resources/build/offline-help-style-spec.txt"
MD=`find -name "*.md"`
DESTCHM="$THIS_DIR/../../../bin/help/desktop"

if $DO_CHM; then
  #
  # Clean existing folder
  #

  if $DO_CLEAN; then
    rm -rf "$DESTCHM" || true # We don't want to die when we clean an empty folder
  fi
  mkdir -p "$DESTCHM"

  #
  # Generate HTML files from Markdown
  #

  if $DO_CHM_CONVERSION; then
    for INFILE in $MD; do
      OUTFILE="$DESTCHM/${INFILE%.md}.htm"
      echo "Processing $INFILE to $(basename "$OUTFILE")"
      mkdir -p "$(dirname "$OUTFILE")"
      pandoc -s -H "$CSS" --lua-filter="$MDLUA" -t html -o "$OUTFILE" $INFILE
    done
  fi

  #
  # Copy Images
  #

  mkdir -p "$DESTCHM/desktop_images"
  cp "$THIS_DIR"/desktop_images/* "$DESTCHM/desktop_images/"

  #
  # Prepare TOC and HHP files
  #

  pushd "$DESTCHM" > /dev/null

  cp "$THIS_DIR/keymandesktop.hhp" "$DESTCHM/keymandesktop.hhp"
  find -name '*.htm' >> "$DESTCHM/keymandesktop.hhp"

  build_hhc_header
  build_hhc .
  build_hhc_footer

  # hhc.exe returns 1 on success!
  "/c/program files (x86)/html help workshop/hhc.exe" keymandesktop.hhp && false || true
  cp keymandesktop.chm "$THIS_DIR/../../../bin/desktop/keymandesktop.chm"

  popd > /dev/null
fi

#
# Copy files to help.keyman.com
#

if $DO_WEB; then
  DESTWEB="$THIS_DIR/../../../bin/help/md/desktop"

  if $DO_CLEAN; then
    rm -rf "$DESTWEB" || true
  fi

  mkdir -p "$DESTWEB"

  for INFILE in $MD; do
    OUTFILE="$DESTWEB/$INFILE"
    mkdir -p "$(dirname "$OUTFILE")"
    cp "$INFILE" "$OUTFILE"
  done

  mkdir -p "$DESTWEB/desktop_images"
  cp "$THIS_DIR"/desktop_images/* "$DESTWEB/desktop_images/"
fi

