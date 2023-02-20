# Set sensible script defaults:
# set -e: Terminate script if a command returns an error
set -e
# set -u: Terminate script if an unset variable is used
set -u

## START STANDARD BUILD SCRIPT INCLUDE
# adjust relative paths as necessary
THIS_SCRIPT="$(greadlink -f "${BASH_SOURCE[0]}" 2>/dev/null || readlink -f "${BASH_SOURCE[0]}")"
. "$(dirname "$THIS_SCRIPT")/../../resources/build/build-utils.sh"
## END STANDARD BUILD SCRIPT INCLUDE

QUIET=0

. "$KEYMAN_ROOT/resources/shellHelperFunctions.sh"

THIS_DIR="$(dirname "$THIS_SCRIPT")"

#
# Copy release notes for Gradle Play Publisher to upload
# Reference: https://github.com/Triple-T/gradle-play-publisher#uploading-release-notes
#
PLAY_RELEASE_NOTES="$KEYMAN_ROOT/android/KMAPro/kMAPro/src/main/play/release-notes/en-US/$TIER.txt"
if [ $TIER = "stable" ]; then
  PLAY_RELEASE_NOTES="$KEYMAN_ROOT/android/KMAPro/kMAPro/src/main/play/release-notes/en-US/default.txt"
fi
echo "Generating Play Store release notes to $PLAY_RELEASE_NOTES"
echo "" > "$PLAY_RELEASE_NOTES"

# Copy whatsnew.md to release notes 1 line at a time,
# filtering for lines that start with "*".
# Pad release notes if whatsnew.md doesn't have any line items
# Play Store release notes have a limit of 500 characters
DEFAULT_RELEASE_NOTE="* Additional bug fixes and improvements"
FILTERED_LINES=$( grep '^\s*\*.*$' "$KEYMAN_ROOT/android/help/about/whatsnew.md" || [[ $? == 1 ]] ) # Continue if grep has no matches
if [ -z "$FILTERED_LINES" ]; then
  FILTERED_LINES="$DEFAULT_RELEASE_NOTE"
  builder_warn "Warning: whatsnew.md empty so using default release note: '$FILTERED_LINES'"
fi

IFS=$'\n'      # Change IFS to new line
for line in $FILTERED_LINES
do
  CHARS_IN_RELEASE_NOTES=$( wc -m < $PLAY_RELEASE_NOTES )
  CHARS_IN_CURRENT_LINE=$( wc -m <<< $line )
  if (( CHARS_IN_RELEASE_NOTES + CHARS_IN_CURRENT_LINE + 1 < 450 )); then
    # Copy line to Play Store release notes
    echo "$line" >> $PLAY_RELEASE_NOTES
  else
    # 450 chars reached
    builder_warn "Warning: Play Store release notes approaching 500 character limit"
    echo "$DEFAULT_RELEASE_NOTE" >> $PLAY_RELEASE_NOTES
    break
  fi
done
