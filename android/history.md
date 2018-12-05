# Keyman for Android


## 11.0 alpha
* Move to 11.0
* Add round launcher icons (#1077)
* Change default keyboard from `european` to `sil_euro_latin` (#1112)
* Remove deprecated ad-hoc distribution of keyboards via `keyman://` protocol (#1109)
* Add splash screen (#1151)
* Update app to use Material Design theme (#681)
* Fix globe button when pausing WebBrowser (#1213)
* Change system keyboard to "numeric" layer for digit/phone number text fields (#1218)
* Add feature to vibrate device when Keyman Web calls `beep` (#1227)
* Add support for extra key found on European hardware keyboards (#1291)
* Add feature for keyboard picker to switch to next keyboard (#1283)
* Update to Cloud API 4.0 for downloading keyboards (#1320)
* Fixes issue where file extensions are upper-case, e.g. ".TTF" (#1333)

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