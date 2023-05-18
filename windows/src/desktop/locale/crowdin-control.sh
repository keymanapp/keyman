#!/usr/bin/env bash
#
# Controls uploading and downloading Crowdin l10n files for
# Keyman for Windows.
#

# exit on error
set -e
# fail on undefined variable
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(readlink -f "${BASH_SOURCE[0]}")"
. "${THIS_SCRIPT%/*}/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

LOCALE_DIR="$KEYMAN_ROOT/windows/src/desktop/locale/"
XSLT="$KEYMAN_ROOT/windows/bin/buildtools/xslt.exe"
[ -f "$XSLT" ] || builder_die "ERROR: Unable to find xslt.exe; build Keyman buildtools first."

SOURCE_LOCALE_XML="$KEYMAN_ROOT/windows/src/desktop/kmshell/xml/locale.xml"
TARGET_STRINGS_XML="$LOCALE_DIR/strings.xml"

display_usage() {
    echo <<-END
      usage: crowdin-control.sh upload|download

      * upload: upload source locale.xml to crowdin
      * download: download translations from crowdin

      Files will be automatically converted from locale.xml format to
      Android strings format for import into Crowdin
END
  exit 1
}

##
## Uploads kmshell/xml/locale.xml to Crowdin
##
do_upload() {
  # Convert locale.xml to Android strings format
  "$XSLT" "$SOURCE_LOCALE_XML" "$LOCALE_DIR/locale-to-android-strings.xsl" "$TARGET_STRINGS_XML"
  crowdin.bat upload
}

##
## Downloads latest strings.xml from Crowdin for each
## target locale and converts them to locale.xml format
##
do_download() {
  # crowdin.bat download
  builder_die "Not yet implemented"
}

# Parse args
shopt -s nocasematch

ACTION=

while [[ $# -gt 0 ]] ; do
  key="$1"
  case $key in
    upload)
      ACTION=upload
      ;;
    download)
      ACTION=download
      ;;
    -h|-\?|-help|--help)
      display_usage
      ;;
    *)
      builder_die "Invalid parameters specified. $0 --help for help."
      ;;
  esac
  shift # past argument
done

if [ -z $ACTION ]; then
  builder_die "No parameters specified. $0 --help for help.";
fi

if [ $ACTION = upload ]; then
  do_upload
else
  do_download
fi
