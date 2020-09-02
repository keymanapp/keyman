# Keyman for Android Version History

## 2020-09-02 13.0.6218 stable
* Bug fix:
  * Dismiss notifications when touched (#3548)

## 2020-08-31 13.0.6217 stable
* Bug fix:
  * Fix overflow menu for hdpi devices (#3536)

## 2020-07-17 13.0.6216 stable
* Bug fix:
  * Make sure switch to system keyboard works on lock screen for Android P (#3358)

## 2020-07-16 13.0.6215 stable
* Improve performance when typing long documents with predictive text (#3302)

## 2020-05-29 13.0.6214 stable
* Bug fix:
  * Fix format string of QR code for sharing keyboards (#3188)

## 2020-05-27 13.0.6213 stable
* Changes:
  * Add Tamil lexical model to KMSample2 (#3165)

## 2020-05-25 13.0.6212 stable
* Bug fix:
  * Fix system keyboard globe button override (#3161)

## 2020-05-08 13.0.6211 stable
* no change to Keyman for Android (rebuild FV app)

## 2020-05-07 13.0.6210 stable
* Bug fix:
  * Back button dismisses system keyboard (#3094)
  * Fix system keyboard alignment (#3097)

## 2020-04-20 13.0.6209 stable
* Bug fix:
  * Fix crash involving system keyboard update notifications (#3007)

## 2020-04-15 13.0.6208 stable
* no change to Keyman for Android (updated Keyman Web Engine #2957)

## 2020-04-01 13.0.6207 stable
* Bug fix:
  * Fix crash involving globe button with 3rd party apps (#2932)

## 2020-03-18 13.0.6206 stable
* No change to Keyman for Android (updated Keyman Web Engine, #2841, 2839)

## 2020-03-13 13.0.6205 stable
* No change to Keyman for Android (updated Keyman Web Engine, #2823, 2821, 2820, 2823)

## 2020-03-11 13.0.6204 stable
* No change to Keyman for Android (updated Keyman Web Engine, #2807, 2797)

## 2020-03-09 13.0.6203 stable
* No change to Keyman for Android (updated Keyman Web Engine, #2694)

## 2020-03-06 13.0.6202 stable
* No change to Keyman for Android (updated Keyman Web Engine, #2781)

## 2020-02-25 13.0.6201 stable
* Bug fix:
  * Fix crash involving empty keyboard list (#2723)
  * Sanitize app version for api query (#2724)

## 2020-02-19 13.0.6200 stable
* Release 13.0

## 2020-02-14 13.0.6059 beta
* Bug fix:
  * Re-initialize CloudDownloadManager when downloading resources after Keyman app is closed (#2635)
* Change:
  * Restyles the suggestion banner for predictions (#2629)

## 2020-02-13 13.0.6058 beta
* Change:
  * Update rest of Android apps' dependency to `androidx.appcompat:appcompat:1.2.0-alpha02` to
    fix WebView crash on Android 5.0 devices (#2644)

## 2020-02-12 13.0.6057 beta
* Change:
  * Update in-app help for 13.0 (#2641)
  * Update oem dependency to `androidx.appcompat:appcompat:1.2.0-alpha02` to
    fix WebView crash on Android 5.0 devices (#2640)

## 2020-02-11 13.0.6056 beta
* No change to Keyman for Android (updated Keyman Web Engine, #2623)

## 2020-02-10 13.0.6055 beta
* Change:
  * Update default nrc.en.mtnt model to version 0.1.4 (#2608)

## 2020-02-06 13.0.6054 beta
* No change to Keyman for Android (updated Keyman Web Engine, #2561)

## 2020-02-04 13.0.6053 beta
* No change to Keyman for Android (updated Keyman Web Engine, #2559)

## 2020-01-31 13.0.6051 beta
* Bug fix:
  * Fix cancelling dictionary update notifications (#2547)
  * Fix external keyboard key "tab" (updated Keyman Web Engine #2546)

## 2020-01-28 13.0.6050 beta
* New Features:
  * Adding a download manager to execute downloads in background and cleanup the existing implementation (#2247, #2275, #2308, #2365)
  * Show spinner (without blocking UI), if user wants to add a language/keyboard and catalog download is in progress (#2313)
  * Improve custom package installation: Show readme.htm before starting installation process (#2286)
  * Check for keyboard updates during keyman startup (#2335)
  * Show available keyboard updates as android system notifications (#2335)
  * Add update indicator icon to inform user about updates and install updates in keyman app (#2335)
  * Add preference so update notifications can be ignored 3 months (#2412)
  * Indicate when keyboard or model updates are available on the "Keyboard Settings" and "Model Info" pages (#2511)
  * Add QR Codes to Keyboard Info pages so users can share keyboard downloads (#2458)
* Changes:
  * Update target Android SDK version to 29 (#2279)
  * Add simple UI tests for keyboard picker and keyboard info screens (#2326)
  * Add example dictionary to KMSample1 project (#2369)
  * Prevent lower-cased API returns from causing mismatches (#2404)
* Bug fix:
  * Sanitize the app version to `#.#.#` for the API cloud query (#2319)
  * Add linting to Debug builds and resolve lint errors (#2305)
  * Fix memory issues during build process (#2361)
  * Fix crashes when parsing JSON data from Cloud (#2393)
  * Improve compatibility with applications such as Gmail, Chrome that do not conform to the Android input APIs (#2382, #2376)
  * Propagate custom help links (#2448)
  * Fix file permissions for viewing welcome.htm assets (#2465)
  * Fix UI flicker during keyboard switching (#2296)
  * Fix compatibility with older Android devices (4.4 KitKat and later) (#2401, 12.0:#2358, #2454, 12.0:#2453)

## 2019-12-18 12.0.4215 stable
* Bug fix:
  * Fix compatibility with older Android devices (4.4 KitKat and later) (#2453)

## 2019-12-12 12.0.4214 stable
* Bug fix:
  * Fix crash involving 0-length context (#2444)

## 2019-12-09 12.0.4213 stable
* Bug fix:
  * Always use lower-case langauge ID's when processing API returns (#2406)
  * Add checks when accessing the Cloud to avoid exceptions (#2393)
  * Improve Keymanweb and KMEA compatability with devices Android API 19-23 (4.4 KitKat and later) (#2358)

* Change
  * Update default nrc.en.mtnt model to version 0.1.3 (#2389)

## 2019-11-27 12.0.4211 stable
* Bug fix:
  * Fix crashes involving context manipulation of invalid context (#2377)
  * Fix Package ID so other languages from a keyboard .kmp package can be installed (#2378)
  * Fix crashes when accessing the Cloud for downloading keyboards (#2378)

## 2019-11-26 12.0.4210 stable
* No change to Keyman for Android (updated Keyman Web Engine, #2322)

## 2019-11-25 12.0.4209 stable
* No change to Keyman for Android (updated Keyman Web Engine, #2372)

## 2019-11-22 12.0.4208 stable
* Bug fix:
  * Fix context manipulation to work around Chromium issue (#2281)

## 2019-11-13 12.0.4207 stable
* No change to Keyman for Android (updated Keyman Web Engine, #2288)

## 2019-10-30 12.0.4206 stable
* Bug fix:
  * Disable suggestions when system keyboard entering password field (#2255)

## 2019-10-25 12.0.4205 stable
* Bug fix:
  * Use lexical model package version for dataset (#2242)

## 2019-10-14 12.0.4204 stable
* Bug Fix:
  * FirstVoices app may crash if analytics is not present (#2204)

## 2019-10-10 12.0.4201 stable
* Bug Fix:
  * Use lexical model package version for lexical model version (#2195)

## 2019-10-07 12.0.4200 stable
* Release 12.0

## 2019-10-04 12.0.4096 beta
* Bug Fix:
  * Add wrappers for missing API methods (#2167)
* Add in-app help on uninstalling dictionary (#2171)

## 2019-10-01 12.0.4095 beta
* Splits help into multiple pages for better usability (#2139)

## 2019-09-26 12.0.4094 beta
* Bug Fixes:
  * Fix crash when Language picker doesn't contain keyboard catalog info (#2138)
  * Fix crashes involving dismissing keyboard and selecting the last keyboard (#2135)
  * Improve keyboard swap stability (#2136)

## 2019-09-26 12.0.4093 beta
* No change to Keyman for Android (updated Keyman Web Engine, #2126)

## 2019-09-23 12.0.4092 beta
* Disable corrections toggle when predictions are disabled (#2119)

## 2019-09-20 12.0.4091 beta
* Update offline help content (#2104)

## 2019-09-19 12.0.4090 beta
* Use versioned help on the Info page (#2103)

## 2019-09-09 12.0.4086 beta
* Bug Fix:
  * Fix exception handling while parsing JSON info from cloud (#2065)
  * Fix crash involving undefined key for custom keyboard (#2066)

## 2019-09-06 12.0.4085 beta
* New Feature
  * Include the English (MTNT) dictionary by default for English (#2029)

* Bug Fix:
  * Fix collision of File Provider (#2053)

## 2019-09-04 12.0.4084 beta
* No change to Keyman for Android (updated Keyman Web Engine, #2033, #2037)

## 2019-09-03 12.0.4083 beta
* No change to Keyman for Android (updated Keyman Web Engine, #2027)

## 2019-09-02 12.0.4082 beta
* No change to Keyman for Android (updated Keyman Web Engine, #2013)

## 2019-08-30 12.0.4081 beta
* New Feature:
  * Allow user to "Add keyboard from local device" from Settings menu (#1992)

* Bug Fix:
  * Fix keyboard and dictionary info pages (#2020)

## 2019-08-27 12.0.4080 beta
* Fix menu icon and text alignment (#1999)

## 2019-08-23 12.0.4079 beta
* Disable version and copyright text on splash screen (#1989)

## 2019-08-16 12.0.4078 beta
* No changes to Keyman for Android

## 2019-08-12 12.0.4077 beta
* No changes to Keyman for Android

## 2019-08-06 12.0.4076 beta
* Adjustments to Settings UI (#1931)

## 2019-08-05 12.0.4075 beta
* Fixes issue with suggestion text misalignment (#1932)

## 2019-07-29 12.0.4074 beta
* Initial beta release of Keyman for Android 12
* [Pull Requests](https://github.com/keymanapp/keyman/pulls?utf8=%E2%9C%93&q=is%3Apr+merged%3A2019-02-25..2019-08-04+label%3Aandroid+base%3Amaster)

* New Features:
  * Add new "Settings" menu (#1751)
  * Add predictive text support (#1641, #1653)

* Changes:
  * Minimum supported Android version is now 4.4 (KitKat) (#1905)

* Bug Fixes:
  * Improve stability of keyboard loading and app startup (#1907)

## 2019-06-28 11.0.2108 stable
* Changes:
  * Allow user to change keyboard from lock screen (#1709)

## 2019-02-27 11.0.2102 stable
* Bug fix:
  * Fix crash from language picker trying to show error dialog (#1634)

## 2019-02-26 11.0.2101 stable
* No changes to Keyman for Android (updated Keyman Web Engine, #1629)

## 2019-02-25 11.0.2100 stable
* 11.0 Stable release

## 2019-02-15 11.0.2062 beta
* No changes.

## 2019-02-07 11.0.2061 beta
* Bug fix:
  * Add notifications when keyboard or font fails to download from Keyman cloud (#1570)

## 2019-01-27 11.0.2060 beta
* Bug fixes:
  * Clean up styling of dialogs when downloading keyboards
  * Fix "Get Started" checkbox label to display on older Android versions

## 2019-01-21 11.0.2059 beta
* Bug fix:
  * Change installation of ad-hoc keyboards via .kmp packages to only add the first language for each keyboard.
    Additional languages can be added offline from the .kmp package. (#1550, #1554)

## 2019-01-18 11.0.2058 beta
* No changes to Keyman for Android (updated Keyman Web Engine, #1537)

## 2019-01-17 11.0.2057 beta
* No changes to Keyman for Android (updated Keyman Web Engine, #1539)

## 2019-01-15 11.0.2056 beta
* No changes to Keyman for Android (updated Keyman Web Engine, #1540)

## 2019-01-14 11.0.2055 beta
* Changes:
  * Keyman for Android 11 requires a minimum version of Android 4.1 (Jelly Bean) (#1532)
  * When KMW doesn't process external "tab" or "enter" keys, have the Android app dispatch the keys (#1526)

## 2019-01-10 11.0.2054 beta
* Bug fix:
  * Fix keyboard version comparison that was causing "Unable to contact Keyman server" notifications (#1520)

## 2019-01-09 11.0.2053 beta
* Bug fixes:
  * Fix "Get Started" default keyboard status on engineering builds (#1515)
  * Fix crash involving certain fonts. Prioritize using .ttf font in keyboards (#1507)

## 2019-01-04 11.0.2052 beta
* Bug fixes:
  * Fix default handling of 102nd key found on European hardware keyboards (#1491)
  * Fixed external keyboard keys "tab" and "backspace" for embedded platforms (#1474)

## 2019-01-03 11.0.2051 beta
* New Feature:
  * Add option to cancel when downloading the keyboard catalog (#1470)

## 2019-01-02 11.0.2050 beta
* Initial beta release of Keyman for Android 11
* [Pull Requests](https://github.com/keymanapp/keyman/pulls?utf8=%E2%9C%93&q=is%3Apr+merged%3A2018-07-01..2019-01-01+label%3Aandroid+-label%3Acherry-pick+-label%3Astable)

* New Features:
  * System keyboard changes to "numeric" layer for digit/phone number text fields (#1218)
  * Device vibrates when Keyman Web calls `beep` -- when invalid combinations are pressed (#1227)
  * Added support for 102nd key found on European hardware keyboards (#1291)
  * Keyboard picker can now switch to next system keyboard (#1283)

* Changes:
  * Added round launcher icons (#1077)
  * Added splash screen (#1151)
  * Updated app to use Material Design theme (#681, #1378, #1303)
  * Updated to Cloud API 4.0 for downloading keyboards (#1320)
  * Removed deprecated ad-hoc distribution of keyboards via `keyman://` protocol (#1109)
  * Changed default keyboard from `european` to `sil_euro_latin` (#1112, #1400)

* Bug fixes:
  * Diacritics now display more consistently on key caps (#1407)
  * Fixed globe button when pausing WebBrowser (#1213)
  * Fixed issue where file extensions are upper-case, e.g. ".TTF" (#1333)
  * Fixed various crashes (#1108, #1057)

## 2018-11-14 10.0.508 stable
* Fix crash that can occur when text selection ends before the starting position (#1313)

## 2018-10-04 10.0.507 stable
* Fix crash that can occur when displaying preview key (#1230, #1234)

## 2018-08-23 10.0.505 stable
* Validate keyboard ID when downloading keyboard from Keyman cloud (#1121)

## 2018-08-22 10.0.504 stable
* Fixes crash when installed keyboards list is invalid (#1119)

## 2018-08-16 10.0.503 stable
* Fixes crashes for release configurations when InputConfiguration or package name is null (#1103)

## 2018-07-06 10.0.502 stable
* Fixes issue for embedded Android, iOS apps where a keyboard with varying row counts in different layers could crash (#1055)

## 2018-06-28 10.0.500 stable
* 10.0 stable release

## 2018-06-27 10.0.405 beta
* Fixes issue where next layer was not correctly selected when the first longpress key pressed (#1027)
* Fixes issue where a quote character in some contexts could cause the keyboard to fail (#1028)

## 2018-06-26 10.0.404 beta
* Add documentation for version 10.0 (#1023)

## 2018-06-21 10.0.403 beta
* Fixes an issue where opening a menu could cause the keyboard to unload (#1014)

## 2018-06-20 10.0.402 beta
* Support script subtags for keyboard languages supplied from cloud (#1012)

## 2018-06-15 10.0.401 beta
* No changes to Keyman for Android

## 2018-06-13 10.0.400 beta
* No changes to Keyman for Android

## 2018-06-11 10.0.399 beta
* Consolidate Info view (#972)
* Platform tests (in keyboards) are now consistent across all platforms (#969)

## 2018-06-05 10.0.398 beta
* Fix globe button for system keyboard (#942)

## 2018-05-27 10.0.397 beta
* Improve intent-filter for *.kmp extensions (#902)

## 2018-05-22 10.0.396 beta
* No changes to Keyman for Android.

## 2018-05-22 10.0.395 beta
* No changes to Keyman for Android.

## 2018-05-18 10.0.394 beta
* No changes to Keyman for Android.

## 2018-05-17 10.0.393 beta
* No changes to Keyman for Android.

## 2018-05-11 10.0.392 beta
* Fix globe button when exiting in-app browser (#848)

## 2018-05-11 10.0.391 beta
* Update compile and target Android SDK version to 27 (#750)

## 2018-05-08 10.0.386 beta
* Fix crashes from invalid package name/version (#819)
* Clean up console log (#748)

## 2018-04-30 10.0.385 beta
* No changes to Keyman for Android (updated Keyman Web Engine, #834)

## 2018-04-30 10.0.384 beta
* No changes to Keyman for Android.

## 2018-05-03 10.0.383 beta
* No changes to Keyman for Android.

## 2018-04-30 10.0.382 beta
* No changes to Keyman for Android (updated Keyman Web Engine, #797)

## 2018-04-30 10.0.381 beta
* Fix OSK missing some keys on older Android configurations (#781)

## 2018-04-30 10.0.380 beta
* No changes to Keyman for Android.

## 2018-04-27 10.0.379 beta
* Fixed app crash when cancelling Keyboard download dialog (#786)

## 2018-04-25 10.0.378 beta
* No changes to Keyman for Android (updated Keyman Web Engine, #772).

## 2018-04-25 10.0.377 beta
* No changes to Keyman for Android (updated Keyman Web Engine, #773).

## 2018-04-24 10.0.376 beta
* Hide system OSK when resuming Keyman app. Disable text suggestions (#711)

## 2018-04-12 10.0.375 beta
* No changes to Keyman for Android.

## 2018-04-12 10.0.374 beta
* Fixes for back button in web browser (#737)

## 2018-03-22 10.0.373 beta
* Initial beta release of Keyman for Android 10.0

## 10.0 alpha
* Refactor how longpress keys on touch layout are processed in KMW engine. This prevents key text
  from being processed as key codes, and fixes the app crash when longpress with K_SPACE.
* Added support for L/R Alt and Ctrl and Caps Lock modifiers for keyboards if specified by a keyboard designer
* Add feature to reset keyboard to default layer when new input field focused (#288)
* Removed "Share to Facebook" feature (#156)
* Fix dual keyboards that appear when closing Keyman Browser (#220)
* Fix KMEA http:// to https:// redirects for downloading keyboard resources (#370)
* Change internal keyboard assets from languages/ and fonts/ folders to packages/
* Add feature to install ad-hoc keyboards via .kmp packages
* Add [Firebase Crashlytics](https://firebase.google.com/docs/crashlytics/) for generating crash reports
* Add Material Design [icons](https://material.io/icons/)
* Fix path to special OSK font for longpress of special keys (#239)

## 2017-08-10 2.8.300 stable
* No changes, just published latest beta as stable

## 2017-07-14 2.7.298 beta
* Fixed long-press popups to correctly show lower case and upper case letters
* Fixed several hardware keyboard bugs involving SPACEBAR, TAB, and ENTER keys, and correctly displaying non-English languages
* Removed license checks

## 2016-10-10 2.4 stable
* Keyman is now free!
* Keyman Pro renamed to Keyman
* Keyman Free retired
* Experimental support for hardware keyboards

## 2015-07-06 2.2 stable
* Faster load, keyboard switching and more responsive touches
* More stable, reduced memory requirements and addressed crashes
* Improved look and feel including improved long-press menus
* Smoother touch interactions and rapid touch interactions
* Handles touches just outside a key more intelligently
* Minor bug fixes and improvements

## 2015-01-27 2.1 stable
* New feature: Keyman browser allows use of your language online (Pro edition only)

## 2014-11-14 2.0 stable
* Major release: split into Pro and Free editions, retired Beta edition
* Bug fixes and performance improvements

## 2014-09-26 1.5 stable
* Added a new "Get Started" menu that lists key tasks such as adding a keyboard or implementing system wide keyboards
* Other bug fixes

## 2014-06-30 1.4 stable
* You will now see a key preview on phone devices when you touch a key
* You can now swipe to select popup keys
* Installed keyboards now have keyboard version and help available
* European Latin keyboard no longer uses desktop-based shortcuts (e.g. `.c` no longer outputs `ċ`)
* Improved lock screen compatibility
* System keyboard no longer loses context or fails to respond on switch
* Other minor bug fixes

## 2014-05-27 1.3 stable
* Keyboards will update automatically when bug fixes or new features are added
* Bug fix: A slightly longer press on a key would sometimes fail to input the keystroke
* Default English keyboard is now enhanced for European language diacritics
* Behind the scenes: Now uses Keyman Cloud API 3.0 for access to newest keyboard layouts
* Other minor bug fixes

## 2014-04-22 1.2 stable
* Install custom keyboards created with Keyman Developer 9

## 2014-02-27 1.1 stable
* Keyman is now available as Android system keyboard.
* Touch and hold keys crash issue solved for Android 4.0.3 - 4.0.4 devices
* Keyman can now be launched from custom link *keyman://* in a web page
* Fixed bug with some keyboards loading with incorrect character set (instead of UTF-8)

## 2014-01-29 1.0 stable
* Keyman for Android original release
