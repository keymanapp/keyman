# Keyman for Android

## 2018-05-11 10.0.392 beta
* Fix globe button when exiting in-app browser (#231)

## 2018-05-11 10.0.391 beta
* Update compile and target Android SDK version to 27 (#750)

## 2018-05-08 10.0.386 beta
* Fix crashes from invalid package name/version (#819)
* Clean up console log (#748)

## 2018-04-30 10.0.380 beta
* Fix OSK missing some keys on older Android configurations (#304)

## 2018-04-27 10.0.379 beta
* Fixed app crash when cancelling Keyboard download dialog (#786)

## 2018-04-24 10.0.376 beta
* Hide system OSK when resuming Keyman app. Disable text suggestions (#711)

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