# Localization Maintenance
Localization for Keyman is maintained at https://crowdin.com/project/keyman
The Crowdin CLI (v3) tool is used to automate downloading and updating files 
between Keyman and Crowdin. The configuration file for each platform is a YAML file named `crowdin.yml`.

On Windows, use `crowdin.bat` instead of `crowdin` for all the syntax below.

When adding new files to https://crowdin.com/project/keyman/settings#files
also go to the file "Settings" and edit **Resulting file after translations export:**. The downloaded translations from Crowdin (Keyman.zip) will have this folder structure:
```
{locale} / {Resulting file after translation export}
```

For example, a KMEA string uses
```
/KMEA/app/src/main/res/values-%android_code%/%original_file_name%
```

## Setup
Install the [Crowdin CLI (v3)](https://support.crowdin.com/cli-tool-v3/) for your OS. 
Note, it has a prerequisite on Java 8.

### Environment Variables
In Crowdin, create a [personal access token](https://crowdin.com/settings#api-key) 
and set it as an environment variable `CROWDIN_PERSONAL_TOKEN`.

Also copy the [project id](https://crowdin.com/project/keyman/settings#api) 
from API v2 and set it as an environment variable `CROWDIN_PROJECT_ID`.

### Testing Crowdin CLI is Set Up Correctly 
To check your CLI setup, 
```
crowdin list project
```

You should see the CLI fetching project info and generating a list of files associated with the project. If you get Java ConnectionExceptions, try connecting VPN.

## Downloading from Crowdin

To download a platform's latest translations from Crowdin, open a command line at a platform's folder and run:
```
crowdin download
```

To download latest translations for the specific language:
```
crowdin download -l {language_code}
```

To display a list of latest translations from Crowdin:
```
crowdin download --dryrun
```

## Uploading to crowdin

To upload source files to Crowdin:
```
crowdin upload sources
```
