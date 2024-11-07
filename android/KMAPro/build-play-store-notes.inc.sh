#!/usr/bin/env bash

# Assumption: parent script that sources this already has START STANDARD BUILD SCRIPT INCLUDE

################################ Main script ################################

function generateReleaseNotes() {
  pushd "$KEYMAN_ROOT/android/KMAPro/"

  #
  # Copy release notes for Gradle Play Publisher to upload
  # Reference: https://github.com/Triple-T/gradle-play-publisher#uploading-release-notes
  #
  local PLAY_RELEASE_NOTES="$KEYMAN_ROOT/android/KMAPro/kMAPro/src/main/play/release-notes/en-US/$TIER.txt"
  if [ $TIER = "stable" ]; then
    PLAY_RELEASE_NOTES="$KEYMAN_ROOT/android/KMAPro/kMAPro/src/main/play/release-notes/en-US/default.txt"
  fi
  builder_heading "Generating Play Store release notes to $PLAY_RELEASE_NOTES"
  # write to file
  echo "" > "$PLAY_RELEASE_NOTES"

  # Copy whatsnew.md to release notes 1 line at a time,
  # filtering for lines that start with "*".
  # Pad release notes if whatsnew.md doesn't have any line items
  # Play Store release notes have a limit of 500 characters
  local DEFAULT_RELEASE_NOTE="* Additional bug fixes and improvements"
  local FILTERED_LINES=$( grep '^\s*\*.*$' "$KEYMAN_ROOT/android/docs/help/about/whatsnew.md" || [[ $? == 1 ]] ) # Continue if grep has no matches
  if [ -z "$FILTERED_LINES" ]; then
    FILTERED_LINES="$DEFAULT_RELEASE_NOTE"
    builder_warn "Warning: whatsnew.md empty so using default release note: '$FILTERED_LINES'"
  fi

  # Change IFS to new line
  local old_IFS="${IFS}"
  IFS=$'\n'
  for line in $FILTERED_LINES
  do
    local CHARS_IN_RELEASE_NOTES=$( wc -m < "$PLAY_RELEASE_NOTES" )
    local CHARS_IN_CURRENT_LINE=$( wc -m <<< $line )
    if (( CHARS_IN_RELEASE_NOTES + CHARS_IN_CURRENT_LINE + 1 < 450 )); then
      # Copy line to Play Store release notes
      echo "$line" >> "$PLAY_RELEASE_NOTES"
    else
      # 450 chars reached
      builder_warn "Warning: Play Store release notes approaching 500 character limit"
      echo "$DEFAULT_RELEASE_NOTE" >> "$PLAY_RELEASE_NOTES"
      break
    fi
  done

  # Restore IFS
  IFS="${old_IFS}"
  popd
}
