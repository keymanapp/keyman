#!/bin/bash

# Copy extracted crowdin files to KMEI and Keyman app projects
function processiOS() {
  cd "$CROWDIN_ROOT"
  echo "Processing iOS"
  for crowd_locale in *; do
    if [ -d ${crowd_locale} ]; then

      # For now, only handle km locale
      if [ ${crowd_locale} != "km" ]; then
        continue
      fi  

      echo "Found locale $crowd_locale"

      # KMEI strings
      if [ -f "$CROWDIN_ROOT/$crowd_locale/ios/engine/KMEI/KeymanEngine/Classes/LanguagePicker/en.lproj/ResourceInfoView.strings" ]; then
        copy_file "$CROWDIN_ROOT/$crowd_locale/ios/engine/KMEI/KeymanEngine/Classes/LanguagePicker/en.lproj/ResourceInfoView.strings" \
            "$KMI_ROOT/engine/KMEI/KeymanEngine/Classes/LanguagePicker/${crowd_locale}.lproj/"
      fi

      if [ -f "$CROWDIN_ROOT/$crowd_locale/ios/engine/KMEI/KeymanEngine/en.lproj/Localizable.strings" ]; then
        copy_file "$CROWDIN_ROOT/$crowd_locale/ios/engine/KMEI/KeymanEngine/en.lproj/Localizable.strings" \
            "$KMI_ROOT/engine/KMEI/KeymanEngine/${crowd_locale}.lproj/"
      fi

      # Keyman app strings
      if [ -f "$CROWDIN_ROOT/$crowd_locale/ios/keyman/Keyman/Keyman/en.lproj/Localizable.strings" ]; then
        copy_file "$CROWDIN_ROOT/$crowd_locale/ios/keyman/Keyman/Keyman/en.lproj/Localizable.strings" \
            "$KMI_ROOT/keyman/Keyman/Keyman/${crowd_locale}.lproj/"
      fi
    fi

  done
}