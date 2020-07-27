#!/bin/bash
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
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

XSLT="$KEYMAN_ROOT/windows/bin/buildtools/xslt.exe"
[ -f "$XSLT" ] || die "ERROR: Unable to find xslt.exe; build Keyman buildtools first."

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
    -h|-?|-help|--help)
      display_usage
      ;;
    *)
      fail "Invalid parameters specified. $0 --help for help."
      ;;
  esac
  shift # past argument
done

if [ ACTION = upload ]; then
  do_upload
else
  do_download
fi

##
## Uploads kmshell/xml/locale.xml to Crowdin
##
do_upload() {
  # Convert locale.xml to Android strings format
  "$XSLT" "$SOURCE_LOCALE_XML" "$(dirname "$THIS_SCRIPT")/locale-android-strings.xsl" "$(dirname "$THIS_SCRIPT")/strings.xml"
  crowdin.bat upload
}

##
## Downloads latest strings.xml from Crowdin for each
## target locale and converts them to locale.xml format
##
do_download() {
  # crowdin.bat download
  fail "Not yet implemented"
}