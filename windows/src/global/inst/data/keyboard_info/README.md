# keyboard_info

* **keyboard_info.source.json**
* **keyboard_info.distribution.json**

Documentation at https://help.keyman.com/developer/cloud/keyboard_info

New versions should be deployed to **keymanapp/keyman/windows/src/global/inst/keyboard_info** and **keymanapp/keyboards/tools**

# .keyboard_info version history

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
