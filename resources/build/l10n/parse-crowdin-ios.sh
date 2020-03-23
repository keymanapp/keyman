#!/bin/bash

# Copy extracted crowdin files to KMEA and KMAPro projects
function processiOS() {
  cd "$CROWDIN_ROOT"
  echo "Processing iOS"
  for crowd_locale in *; do
    if [ -d ${crowd_locale} ]; then
      echo "Found locale $crowd_locale"

      # If crowdin locale contains region, we need to prefix with "r"
      # See: https://developer.android.com/guide/topics/resources/providing-resources#AlternativeResources
      #local locale=${crowd_locale/-/-r}

      #if [ -f "$CROWDIN_ROOT/$crowd_locale/android/KMEA/strings.xml" ]; then
      #  copy_file "$CROWDIN_ROOT/$crowd_locale/android/KMEA/strings.xml" "$KMA_ROOT/KMEA/app/src/main/res/values-$locale"
      #fi

      #if [ -f "$CROWDIN_ROOT/$crowd_locale/android/KMAPro/strings.xml" ]; then
      #  copy_file "$CROWDIN_ROOT/$crowd_locale/android/KMAPro/strings.xml" "$KMA_ROOT/KMAPro/kMAPro/src/main/res/values-$locale"
      #fi
    fi

    # For now, only handle the first locale. Remove this when we're ready for everything
    #exit
  done
}