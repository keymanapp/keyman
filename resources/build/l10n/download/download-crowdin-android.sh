#!/bin/bash

# Copy extracted crowdin files to KMEA and KMAPro projects
function processAndroid() {
  cd "$CROWDIN_ROOT"
  echo "Processing Android"
  for crowd_locale in *; do
    if [ -d ${crowd_locale} ]; then

      # For now, only handle km locale
      if [ ${crowd_locale} != "km" ]; then
        continue
      fi
      echo "Found locale $crowd_locale"

      # If crowdin locale contains region, we need to prefix with "r"
      # See: https://developer.android.com/guide/topics/resources/providing-resources#AlternativeResources
      local locale=${crowd_locale/-/-r}

      # KMEA strings
      copy_file "$CROWDIN_ROOT/$crowd_locale/android/KMEA/strings.xml" "$KMA_ROOT/KMEA/app/src/main/res/values-$locale"

      # KMAPro strings
      copy_file "$CROWDIN_ROOT/$crowd_locale/android/KMAPro/strings.xml" "$KMA_ROOT/KMAPro/kMAPro/src/main/res/values-$locale"
    fi

  done
}
