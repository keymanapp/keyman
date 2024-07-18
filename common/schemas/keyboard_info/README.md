# keyboard_info

* **keyboard_info.schema.json**

Documentation at https://help.keyman.com/developer/cloud/keyboard_info

* Primary version:
  * https://github.com/keymanapp/api.keyman.com/tree/master/schemas/keyboard_info
* Synchronized copies at:
  * https://github.com/keymanapp/keyman/tree/master/common/schemas/keyboard_info

# .keyboard_info version history

## 2023-08-11 2.0 stable
* Removed:
  - `.documentationFilename`
  - `.documentationFileSize`
  - `.legacyId`
    `.links`
    `.related[].note`
    `.languages[].example`
  Added:
  - `.languages[].examples[]`
  Modified:
  - `.languages[].font`, `.languages[].oskFont`: `.source` is `[string]`
  - Source .keyboard_info files are no longer needed, so source vs distribution
    keyboard_info distinction is removed

## 2019-09-06 1.0.6 stable
* No changes (see api.keyman.com#36 and api.keyman.com#59. Reverted in 2020-06-10.).

## 2018-12-18 1.0.5 stable
* Deprecate languages[] array. Update KeyboardLanguageInfo object to include subtag names.

## 2018-11-26 1.0.4 stable
* Add helpLink field - a link to a keyboard's help page on help.keyman.com if it exists.

## 2018-02-12 1.0.3 stable
* Renamed minKeymanDesktopVersion to minKeymanVersion to clarify that this version information applies to all platforms.

## 2018-02-10 1.0.2 stable
* Add dictionary to platform support choices. Fixed default for platform to 'none'.

## 2018-01-31 1.0.1 stable
* Add file sizes, isRTL, sourcePath fields so we can supply these to the legacy KeymanWeb Cloud API endpoints.
* Remove references to .kmx being a valid package format.

## 2017-09-14 1.0 stable
* Initial version
