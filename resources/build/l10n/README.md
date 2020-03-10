# Localization Maintenance

Localization for Keyman is handled at https://crowdin.com/project/keyman

In order to incorporate those files into the Keyman platforms:
1. Download the translations to the root of this repo as Keyman.zip
2. From git bash, run `./parse-crowdin.sh` with the specified platforms
Platform options: `-all`, `-android`, `-common`, `-developer`, `-ios`, `-linux`, `-mac`, `-web`, `-windows`

This will extract `/Keyman.zip` into `/crowdin/` and copy files to the respective platforms.

TODO: Add parameter to select locales
