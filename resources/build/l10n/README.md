# Localization Maintenance

Localization for Keyman is handled at https://crowdin.com/project/keyman

In order to incorporate those files into the Keyman platforms:
1. Download the translations to `/.crowdin-tmp/Keyman.zip`
2. From git bash, run `./parse-crowdin.sh [platform options] [-no-extract]` with the specified platforms

    Platform options: `-all`, `-android`, `-common`, `-developer`, `-ios`, `-linux`, `-mac`, `-web`, `-windows`
    This will extract `/.crowdin-tmp/Keyman.zip` into `/crowdin/` and copy files to the respective platforms.

    `-no-extract`: Don't clean or unzip Keyman.zip


TODO: Add parameter to select locales
