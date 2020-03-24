# Localization Maintenance

Localization for Keyman is handled at https://crowdin.com/project/keyman

## Setup
In order automatically download translation files with the crowdin API, you need
to have this [API token](https://crowdin.com/project/keyman/settings#api) 
set as an environment variable.  You might want to add this to a shell script.

```
export KEYMAN_CROWDIN_TOKEN=[project API key here]
```

Otherwise, manually download the translations to `/.crowdin-tmp/Keyman.zip`

## download-crowdin
In order to incorporate the crowdin translation files into the Keyman platforms:
1. Do the setup from above
2. From git bash in this directory, run `./download-crowdin.sh [platform options]
 [-no-download] [-no-extract]` with the specified platforms

    Platform options: `-all`, `-android`, `-common`, `-developer`, `-ios`, `-linux`, `-mac`, `-web`, `-windows`
    This will extract `/.crowdin-tmp/Keyman.zip` into `/crowdin/` and copy files to the respective platforms.

    `-no-download`: Skip downloading Keyman.zip from crowdin.com

    `-no-extract` : Skip clean and unzipping Keyman.zip


TODO: Add parameter to select locales
