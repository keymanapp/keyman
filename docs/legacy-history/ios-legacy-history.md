# Keyman for iPhone and iPad Version History

## 2020-04-15 13.0.106 stable
* Adds workaround for iOS 13.4 bug that breaks longpresses (#2960, #2970)
* Mitigates bug where in-app keyboard would reset inappropriately (#2952)
* Fixes issue where base key could output at same time a popup key was selected (#2882)

## 2020-03-17 13.0.105 stable
* Fixes issues with keyboard banner display (#2841)

## 2020-03-09 13.0.102 stable
* Pre-emptively fixes issue with keyboard scaling for as-of-yet unreleased devices (#2704)
* Fixes in-app Info help link (#2774)
* Fixes issue with select keyboard rules causing loss of context when removing context up to a newline (#2776)

## 2020-02-19 13.0.100 stable
* Release of Keyman 13 for iPhone and iPad
* New Feature: QR Code support for sharing keyboards
* New Feature: Dark mode support
* New Feature: File browser for installing keyboard packages saved on device

## 2020-02-19 13.0.72 beta
* Fixes issue with KeymanEngine demo app and missing color definition in KeymanEngine library (#2662)
* Updates offline help content for 13.0 release (#2671)

## 2020-02-14 13.0.70 beta
* Change: restyles suggestion banner to more closely match the iOS system default (#2629)

## 2020-02-12 13.0.68 beta
* Bug fix: fixed keyboard display logic when sharing with other apps (#2631)

## 2020-02-11 13.0.67 beta
* Bug fix: fixed issue with sample apps and engine keyboard switcher (#2626)
* Bug fix: fixed issue with dictionary updates (#2627)

## 2020-02-10 13.0.66 beta
* Bug fix: Fixed migration of old resources to new app versions, updated default MTNT dictionary (#2512)
* Bug fix: Fixed issues when iOS system transitions between light and dark modes (#2593, #2594)

## 2020-02-03 13.0.63 beta
* Bug fix: Fixes keyboard widths/margins in iOS 9 and 10 (#2560)

## 2020-01-30 13.0.62 beta
* Feature: Initial keyboard setup process simplified with direct Settings link [iOS 11.0+ only] (#2548)
* Bug fix: System keyboard now respects "Sounds > Keyboard Clicks" setting (#2550)

## 2020-01-29 13.0.61 beta
* Bug fix: System keyboard would sometimes appear blank (#2545)

## 2020-01-28 13.0.60 beta
* Feature: Adds file browsing for installable KMPs and makes KMPs for resources installed this way available to the Files app (#2457, #2489, #2510)
* Feature: Adds support for iOS 13.0's dark mode feature (#2277, #2312, #2468, #2521)
* Feature: Adds QR code to keyboard info pages for easier sharing with internet availability (#2509)
* Feature: Show clear message when a language does not have any lexical models (#2475)
* Bug fix: "Always show banner" option now works more consistently (#2479)
* Bug fix: Keyboards now scale to match the default system keyboard's size for known devices (#2487)
* Bug fix: Rotation of keyboard into landscape scales wrongly (#2301, 12.0:#2318)
* Bug fix: Multi-row longpress keys not contained within keyboard bounds (#2306, 12.0:#2317)
* Bug fix: Fixes bug where the keyboard might display within the settings menu (#2470)
* Bug fix: Fix situation where lexical model fails to install with missing version number (#2191, 12.0:#2193)
* Bug fix: Mismatch for BCP 47 codes caused keyboard downloads to fail (#2403, 12.0:#2420)
* Bug fix: If malformed data was returned from an online API, Keyman could crash (#2368)
* Bug fix: Unable to replace previously installed KMP from ad-hoc file (#2459, 12.0:#2461)
* Bug fix: Default model (MTNT) installation was not happening (#2532)

## 2019-12-09 12.0.62 stable
* Bug Fix: Mismatch for BCP 47 codes caused keyboard downloads to fail (#2420)

## 2019-11-25 12.0.60 stable
* Bug Fix: Unable to replace previously installed KMP from ad-hoc file (12.0:#2461)

## 2019-11-13 12.0.55 stable
* Bug Fix: Multi-row longpress keys not contained within keyboard bounds (#2317)

## 2019-11-11 12.0.54 stable
* Bug fix: Rotation of keyboard into landscape scales wrongly (#2318)

## 2019-10-19 12.0.52 stable
* Bug fix: Fix situation where lexical model fails to install with missing version number (#2193)

## 2019-09-20 12.0.39 beta
* App info / help now points to the equivalent pages on help.keyman.com in a multi-page format (#2088)
* Offline help now uses a mirrored copy of help.keyman.com and is also multi-page (#2102)

## 2019-09-17 12.0.37 beta
* Multiple bug fixes for the system keyboard (#2094)

## 2019-09-11 12.0.35 beta
* Fixes predictive text bug where the last input was always interpreted as the first available key (#2076)
* Adds support for scrolling long suggestions in the predictive banner (#2071)

## 2019-09-09 12.0.32 beta
* Fixes OSK scaling bug on app re-entry when using keyboards without dictionaries (#2067)

## 2019-08-29 12.0.20 beta
* Automatically updates old default resources to the new set (#2007)
* Includes the English (MTNT) dictionary by default for English (#2010)

## 2019-08-26 12.0.14 beta
* Fixes Get Started display logic (#1988)
* Enables swipe-deletion of installed keyboards in the language settings menu. (#1969)
* Fixes toggle alignment issues in the Settings UI (#1947)
* Polishes loading of specific-language keyboard downloading menu.  (#1944)
* Moves update functionality to the Installed Language menu and added dictionary updates. (#1979)

## 2019-07-29 12.0.11 beta
* Initial beta release of Keyman for iPhone and iPad 12
* [Pull Requests](https://github.com/keymanapp/keyman/pulls?utf8=%E2%9C%93&q=is%3Apr+merged%3A2019-02-25..2019-08-04+label%3Aios+base%3Amaster)

* New Features:
  * Add new "Settings" menu (#1824)
  * Add predictive text support (#1824)

* Changes:
  * Add automated tests (#1865)

* Bug Fixes:
  * Install a multilingual keyboard only to first language by default (#1610)
  * Fix issue with LTR and RTL marks breaking Keyman context checks (#1756)

## 2019-05-14 11.0.323 stable
* Fixes bug with keyboards using 'nul' and 'context' statements (#1212)

## 2019-02-25 11.0.320 stable
* 11.0 Stable release

## 2019-02-25 11.0.302 beta
* Keyman now works again on iOS 9.3.5 (#1588)

## 2019-01-04 11.0.301 beta
* Improves loading efficiency of system keyboard, helping prevent related crashes (#1475)

## 2019-01-02 11.0.300 beta
* Initial beta release of Keyman for iPhone and iPad 11
* [Pull Requests](https://github.com/keymanapp/keyman/pulls?utf8=%E2%9C%93&q=is%3Apr+merged%3A2018-07-01..2019-01-01+label%3Aios+-label%3Acherry-pick+-label%3Astable)

* New Features:
  * Added support for keypress error feedback with vibration (#1314)

* Changes:
  * Replaced deprecated calls to UIAlertView and cleaned up extraneous blank buttons (#1002)
  * Removed deprecated code to to support keyman:// scheme for ad-hoc distribution (#1160)
  * Updated the default keyboard to `sil_euro_latin` (#1417, #1288)
  * Added SIL logo to info page (#1164)

* Bug Fixes:
  * Bookmark add button is enabled only when title/url fields have text (#1073)
  * Fixed bug behind some crashes of system keyboard (#1166)
  * Fixed ongoing issues with keyboard rotation and sizing, including the iPhone X notch. (#1347, #1318, #1089, #1045)
  * Fixed keyboard display/overlap of "Getting Started" info panel, added keyboard hide/display API functions (#1084)
  * Fixed issues with keyboard keycap scaling and diacritic display (#1445, #1407, #1070)
  * Fixed issue with incorrect font on key caps in some situations (#1450)
  * Various crashes (#1057, #1301)

## 2018-08-02 10.0.208 stable
* Fixed OSK layout problems (and possible crash) on iOS 11 on certain hardware (#1089, #1159)

## 2018-07-06 10.0.203 stable
* Fixes an issue where a keyboard with varying row counts in different layers could crash (#1056, #1057)

## 2018-06-28 10.0.200 stable
* 10.0 stable release

## 2018-06-27 10.0.163 beta
* Fixes issue where next layer was not correctly selected when the first longpress key pressed (#1027)

## 2018-06-26 10.0.162 beta
* Fixes miskey when pressing between two keys (#1020)

## 2018-06-14 10.0.161 beta
* Fixes keyboard not responding to touch from #981 (#988)

## 2018-06-13 10.0.160 beta
* No changes affecting iOS

## 2018-06-13 10.0.159 beta
* No changes affecting iOS

## 2018-06-13 10.0.158 beta
* Bug fix installing a keyboard with missing metadata (#982)

## 2018-06-13 10.0.157 beta
* Improvements to device rotation (#981)

## 2018-06-12 10.0.156 beta
* No changes affecting iOS

## 2018-06-11 10.0.155 beta
* Platform tests (in keyboards) are now consistent across all platforms (#969)

## 2018-06-07 10.0.154 beta
* Fixed API call to fetch BCP-47 language ids and make keyboard updates more robust (#961)

## 2018-06-06 10.0.147-10.0.153 beta
* No actual code changes affecting iOS (#873)

## 2018-05-18 10.0.146 beta
* Code cleanup to remove warnings, etc. (#805)

## 2018-05-17 10.0.145 beta
* Fixes occasional repeated characters when typing rapidly (#865)

## 2018-05-04 10.0.144 beta
* Refactor navigation bar branding. (#802)

## 2018-05-04 10.0.143 beta
* Search google when an invalid URL is entered in the address bar. (#801)

## 2018-05-03 10.0.142 beta
* Supports loading of keyboard .json files which include milliseconds in date formats (#804)

## 2018-05-02 10.0.141 beta
* Fix alphabetical list to be case insensitive on new keyboard list. (#803)

## 2018-04-25 10.0.140 beta
* Fixed lack of output for certain punctuation longpress keys. (#702)

## 2018-03-22 10.0.139 beta
* Initial beta release of Keyman for iPhone and iPad 10

## 10.0 alpha
* Updated versioning scheme for uniformity across all Keyman products.
* Keyman app migrated to Swift 4.0 (#305)
* KeymanEngine is now built as a Swift 4.0 framework (#378)
* Refactored internal app data structures to be more strictly typed (#384, #388)
* Add types to notifications posted by KeymanEngine (#389)
* Refactor KeymanEngine's internal usage of KeymanWeb (#406)
* Fixed bugs introduced by refactoring (#408, #413, #414, #416, #422, #443, #453, #463, #464, #465)
* Removed notifications for subkey displayed, subkey dismissed and debug log messages (#389, #415)
* Removed migration from old app data directory structure (#418)
* Implemented Carthage as a dependency manager and include 3rd party libraries for unzipping and logging (#475)
* Support installing Keyman Packages (KMP) for ad-hoc distribution

## 2017-08-26 2.6.4 stable
* Fixed bug with blank keyboard on some devices (#218)
* Fixed bug with keyboard width being incorrect on iPhone 7, iPhone 7+ (#224)

## 2017-08-18 2.6.0 beta
* Numerous keyboarding bugfixes
* Replaced an outdated internal library

## 2017-02-21 2.5.2 stable
* Fixed bug with long-press keys not working on some newer iPhones

## 2017-02-09 2.5.1 stable
* Keyman is now distributed by SIL International

## 2016-10-14 2.4.2 stable
* Keyman Pro is renamed to Keyman and is now free!
* Separate free edition discontinued

## 2015-11-03 2.4.1 stable
* Now rotates correctly on iOS 9
* Optimized for iOS 9
* Fixed performance issues on iOS 8

## 2015-06-29 2.2.0 stable
* Faster load, keyboard switching and more responsive touches
* More stable, reduced memory requirements and addressed crashes
* Improved look and feel including smaller banner and improved long-press menus
* Smoother touch interactions and rapid touch interactions
* Handles touches just outside a key more intelligently
* Minor bug fixes and improvements

## 2015-01-27 2.1.0 stable
* Built-in browser enables language display where standard browsers do not (Keyman Pro)
* Other bug fixes & improvements

## 2.0.2 stable
* Fixes OSK display issues on iOS 6 & 7 (only Keyman Free supports iOS 6 & 7)
* Fixes Navigation bar background image for iPhone 6 & 6 Plus
* Fixes display of Get started & activity indicator for iPhone 6 Plus on landscape
* Now displays version and build number in info page
* Other bug fixes & improvements

## 2014-11-10 2.0.0 stable
### New Features
* Use any Keyman keyboard throughout your entire iOS 8 device
* Custom installable fonts with iOS 8
* Updated keyboard styling

### Bug Fixes
* Custom font issues experienced in iOS 7.1+ are resolved when you update to iOS 8

## 1.4.0 stable
### New Features

* iPhone & iPod Touch devices now show a key preview when a key is touched
* Installed keyboards now have keyboard version information and help link available in keyboard picker
* A-Z index in language/keyboard list is now enabled on iOS 7.1 and later versions
* Hebrew, Arabic and other right-to-left languages are now correctly displayed flowing right-to-left on iOS 7 and later
* European Latin keyboard no longer has desktop-based shortcuts enabled (e.g. .c no longer outputs ċ)
* Default European Latin keyboard upgraded to latest version (1.2)
* Keyboards can now be re-downloaded if the font fails to download
* Keyboard is now reloaded after updating/re-installing a custom keyboard to allow any changes to appear immediately
* Keyman help page link can now link to keyboard help
* Other minor bug fixes, a potential memory leak, and performance enhancements

### Known Issues

* iOS 7.1 has a problem with installed font profiles: once you restart your device, and in certain other situations, the installed font profiles become unavailable and language fonts will fail to display. While this is not a bug in Keyman as such, it can impact usage of your language in some apps.
* We have received some reports of a bug with fonts in version 1.2: for some users, their language font fails to display after upgrading to version 1.2 from an earlier version. This is happening due to a change in the cached data for your language. To fix the problem on your iPhone or iPad:
	* Open the Keyboards list by touching the globe button on the keyboard.
	* Swipe left on the keyboard with the issue to delete it.
	* Click the Add (+) button to reinstall the keyboard. You should not need to reinstall the font profile.

## 1.3.0 stable
### New Features

* Enhancement: Keyboard is slightly taller on 3.5" and 4" devices to make it easier to use
* Bug fix: A slightly longer press on a key would sometimes fail to input the keystroke
* Default English keyboard is now enhanced for European language diacritics

### Bug Fixes

* Font display in iOS 7.1 and later is improved
* Egyptian Hieroglyphic keyboard works correctly
* Click sounds when typing now correspond precisely to each touch
* Some keyboards which displayed UTF-8 encoding bugs now load correctly.

### Known Issues

* iOS 7.1 has a problem with installed font profiles: once you restart your device, and in certain other situations, the installed font profiles become unavailable and language fonts will fail to display. While this is not a bug in Keyman as such, it can impact usage of your language in some apps.
* We have received some reports of a bug with fonts in version 1.2: for some users, their language font fails to display after upgrading to version 1.2 from an earlier version. This is happening due to a change in the cached data for your language. To fix the problem on your iPhone or iPad:
	* Open the Keyboards list by touching the globe button on the keyboard.
	* Swipe left on the keyboard with the issue to delete it.
	* Click the Add (+) button to reinstall the keyboard. You should not need to reinstall the font profile.

## 1.2.0 stable
### New Features

* Install custom keyboards created with Keyman Developer 9 (free download for Windows)
* User interface updated for iOS 7
* Performance improvements

### Bug Fixes

* Font display in iOS 7.1 and later is improved
* Egyptian Hieroglyphic keyboard works correctly
* Click sounds when typing now correspond precisely to each touch
* Some keyboards which displayed UTF-8 encoding bugs now load correctly.

### Known Issues

* iOS 7.1 has a problem with installed font profiles: once you restart your device, and in certain other situations, the installed font profiles become unavailable and language fonts will fail to display. While this is not a bug in Keyman as such, it can impact usage of your language in some apps.
* We have received some reports of a bug with fonts in version 1.2: for some users, their language font fails to display after upgrading to version 1.2 from an earlier version. This is happening due to a change in the cached data for your language. To fix the problem on your iPhone or iPad:
	* Open the Keyboards list by touching the globe button on the keyboard.
	* Swipe left on the keyboard with the issue to delete it.
	* Click the Add (+) button to reinstall the keyboard. You should not need to reinstall the font profile.

## 1.1.0 stable
### New Features

* Font installation for your language for iOS 7+
* Keyman now opens in response to keyman://localhost/open links on websites
* Text and text size are now saved and loaded back automatically after restarting the app.

### Bug Fixes

* Copy/Paste no longer fails if the text contains apostrophe character
* Fix for Javascript code injection
* Popup keys no longer appear unexpectedly after key touch
* (>) button in keyboard list now operates
* Action button popups no longer overlap the button on iPad
* Keyboard height now always cleanly matches available height in landscape mode
* Help bubble for keyboard change button now shows on first use
* Small message appended to mail and Facebook posts to help friends read text that may not display without fonts

## 1.0.0 stable
* First release!
