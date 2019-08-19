# Keyman for macOS Version History

## 13.0 alpha
* Start version 13.0

## 2019-08-12 12.0.7 beta
* Fix compatibility issues with macOS 10.14.5 (#1889)

## 2019-08-09 12.0.6 beta
* Fix crash when working with keyboards that contain if() statements (e.g sil_ipa) (#1956)

## 2019-07-29 12.0.5 beta
* Initial beta release of Keyman for Mac 12
* [Pull Requests](https://github.com/keymanapp/keyman/pulls?utf8=%E2%9C%93&q=is%3Apr+merged%3A2019-02-25..2019-08-04+label%3Amac+base%3Amaster)

* Bug Fixes:
  * Fix keyboard download passed wrong version string (#1814)

## 2019-02-26 11.0.221 stable
* Add support for option stores (if/set/reset/save statements) (#1838)
* Fix bug where keyboards with more than nine deadkeys did not operate correctly (#1839)
* Fix out of date context buffer when using command keys (#1840)

## 2019-02-25 11.0.220 stable
* 11.0 Stable release

## 2019-02-20 11.0.201 beta
* Update version strings to 11.0.0 (#1583)
* Workaround bug in macOS 10.13.1-10.13.3 which caused preferences dialog to fail to appear (#1447)
* Fix broken About dialog (#1594)

## 2019-01-02 11.0.200 beta
* Initial beta release of Keyman for macOS 11
* [Pull Requests](https://github.com/keymanapp/keyman/pulls?utf8=%E2%9C%93&q=is%3Apr+merged%3A2018-07-01..2019-01-01+label%3Amac+-label%3Acherry-pick+-label%3Astable)

* Changes:
  * Code refactoring and cleanup (#1050, #1053)
  * Added SIL logo and copyright information to About box and DMG image (#1163)

* Bug Fixes:
  * In some situations, Keyman for Mac would use the wrong rule in a keyboard (#1091, #1099)
  * Various crashes (#1424, #1066, #1080)

## 2018-08-14 10.0.111 stable
* CORRECLTY fixed bug in engine that caused incorrect rules to be used (#1095, #1099)

## 2018-08-10 10.0.110 stable
* No change (DO NOT USE; use 10.0.111 or later)

## 2018-08-10 10.0.109 stable
* DO NOT USE (use 10.0.111 or later) - Faulty attempt at bug fix in engine (#1091)

## 2018-07-12 10.0.104 stable
* Removed help button from OSK for versions of macOS < 10.10 to prevent crash (#1080, #1081)

## 2018-07-12 10.0.101 stable
* Fixed crashing bug in OSK for older versions of OS (#1066)

## 2018-06-28 10.0.100 stable
* 10.0 stable release

## 2018-06-28 10.0.53 beta
* Made key-press on OSK work instead of crash (#1038)
* Reverted fix for issue #872: resetting font in Pages and Keynote (#1039)

## 2018-06-27 10.0.52 beta
* Prevented permanently hiding OSK when a menu is dropped down (#1031)

## 2018-06-26 10.0.51 beta
* Prevented calling CFRelease() with a NULL argument (#1019)

## 2018-06-20 10.0.50 beta
* Fixed problems with OSK timer that caused crashes, e.g., when Shift was pressed (#1013)

## 2018-06-19 10.0.49 beta
* Enabled displaying keyboard names in native script in information window (#1001)

## 2018-06-18 10.0.48 beta
* Made other version(s) of LibreOffice use "legacy" mode for character replacements (#990)
* Corrected the way Keyman responds evaluates tokens in **platform** statement (#971)

## 2018-06-07 10.0.47 beta
* Change to build script to use correct build location (#950)

## 2018-06-06 10.0.46 beta
* Fixed crash caused by accessing disposed KeyView from timer event (#938, #941)

## 2018-06-05 10.0.45 beta
* Error-reporting improvements (better method to ensure upload of symbols for Engine) (#935)

## 2018-06-01 10.0.44 beta
* Improved double-clicking KMP (#929)

## 2018-05-28 10.0.43 beta
* Error-reporting improvements (settings modified to upload symbols for Engine) (#884)
* Solved problem of losing font/style information in Pages and Keynote (#930)

## 2018-05-25 10.0.42 beta
* Prevented crashes and performance problems with "event tap" (#883)

## 2018-05-22 10.0.41 beta
* No changes affecting Keyman for macOS (#876)

## 2018-05-22 10.0.40 beta
* No changes affecting Keyman for macOS (#873)

## 2018-05-17 10.0.39 beta
* Added automatic error reporting when exceptions occur (#851)

## 2018-05-09 10.0.38 beta
* Hide the on-screen keyboard when Keyman is no longer the active input method (#770)

## 2018-05-08 10.0.37 beta
* Added option to use verbose logging to facilitate debugging by Keyman Support (#768)

## 2018-05-04 10.0.36 beta
* Improved compatibility with Open Office (#175 - incomplete)

## 2018-05-02 10.0.35 beta
* Enabled searching for keyboards by language name (#705)
* Fixed configuration and download windows to enable typing (e.g., in search boxes) (#791)
* Made hyperlinks in web-views in configuration and download windows work correctly (#709)

## 2018-03-22 10.0.34 beta
* No changes to Keyman for macOS.

## 2018-03-22 10.0.33 beta
* No changes to Keyman for macOS.

## 2018-03-22 10.0.32 beta
* No changes to Keyman for macOS.

## 2018-03-22 10.0.31 beta
* Initial beta release of Keyman 10 for macOS

## 10.0 alpha
* New feature: install keyboard packages by double-clicking the kmp file (#511)
* Added support for L/R Alt and Ctrl modifiers for keyboards (#178)
* Display the version number in the download dialog
* Added support for Keyman version 10.0 keyboards
* Detection of context changes due to mouse clicks and command keys in "legacy" apps (#394)

## 2017-11-21 1.2.0 beta
* Works around bug in macOS High Sierra so that Configuration dialog can be opened (#368)
* Fix support for older versions of macOS below 10.9 (#393)

## 2017-08-13 1.1.14 beta
* Fixed blank download dialog

## 1.0.136 beta
* Keyman for Mac OS X 1.0.136 - General release