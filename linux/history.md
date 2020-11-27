# Keyman for Linux Version History

## 2020-02-20 13.0.101 stable
* Bug Fix: Add Keyman gsettings schema to manifest (#2688)

## 2020-02-19 13.0.100 stable
* 13.0 stable release

## 2020-02-18 13.0.30 beta
* Bug Fix:
  * Include Keyman gsettings schema for packaging (#2675)

## 2020-02-17 13.0.29 beta
* Bug Fix:
  * Fix how tier is determined for debian watch files (#2664)
  * Fix keyboard processor compilation for Xenial (#2648)

## 2020-02-11 13.0.28 beta
* Bug Fix:
  * Incorporate packaging fixes from 14.0 development (#2624)

## 2020-02-10 13.0.27 beta
* Bug Fix:
  * Fix setting context when >= 64 characters (common #2607)

## 2020-02-06 13.0.26 beta
* New Feature:
  * Add ability to set keyboard options from options.htm dialog (#2566)

## 2020-02-05 13.0.25 beta
* Bug Fix:
  * Fix "About" keyboard when missing copyright info (#2583)

## 2020-02-04 13.0.23 beta
* Remove cosmic and disco releases. Add eoan (#2574)

## 2020-01-28 13.0.21 beta
* Revert default tier to alpha (#2474)
* Add QR Codes to share keyboards (#2486)
* Work on Linux packaging (#2263, #2290, #2320, #2321, #2325, #2474, #2515, #2551)

## 2019-09-19 12.0.18 beta
* Bug Fix:
  * Extract icon from kmx instead of downloading it (#2075)

## 2019-09-11 12.0.17 beta
* Bug Fix:
  * Remove code that was downloading keyboard source .kmn files (#2068)

## 2019-07-29 12.0.12 beta
* Initial beta release of Keyman for Linux 12
* [Pull Requests](https://github.com/keymanapp/keyman/pulls?utf8=%E2%9C%93&q=is%3Apr+merged%3A2019-02-25..2019-08-04+label%3Alinux+base%3Amaster)

* Bug Fixes:
  * Clean up readme for km-package (#1660)
  * Ensure automated build uses correct tier (#1670)

## 2019-02-25 11.0.124 stable
* 11.0 Stable release

## 2019-02-22 11.0.111 beta
* Use lowercase ID and kmx filenames when installing .kmp packages (#1601)

## 2019-02-19 11.0.110 beta
* Update readme about launchpad (#1574)

## 2019-02-18 11.0.109 beta
* Create appstream appdata for keyman-config (#1543)

## 2019-02-15 11.0.108 beta
* Handle keyboard package install/uninstall when languages aren't defined (#1585)

## 2019-02-08 11.0.107 beta
* Add disco distribution information (#1572)

## 2019-02-05 11.0.106 beta
* Update readme about packaging and fix build.sh script (#1565)
* Bugfix for potential utf overrun in Keyman Core (#1547)

## 2019-01-10 11.0.105 beta
* Restart ibus as user and on gnome-shell (#1510,#1521)
* Build on xenial (#1518,#1477)
* Remove debconf template not needed any more (#1497)

## 2019-01-04 11.0.104 beta
* Add km-config and .kmp icons (#1495)

## 2019-01-04 11.0.103 beta
* No changes.

## 2019-01-04 11.0.102 beta
* Initial build of Keyman Core on xenial (#1477)
* Auto restart IBus when apt and kbp versioning (#1468)
* Fix majorversion which broke download window (#1490)

## 2019-01-03 11.0.101 beta
* No changes.

## 2019-01-02 11.0.100 beta
* Initial beta release of Keyman for Linux
* [Pull Requests](https://github.com/keymanapp/keyman/pulls?utf8=%E2%9C%93&q=is%3Apr+merged%3A2018-07-01..2019-01-01+label%3Alinux+-label%3Acherry-pick+-label%3Astable)

* Keyman for Linux supports .kmp and .kmx file types from Keyman keyboard distribution repository

* Import KMFL into Keyman
* kmflcomp 0.9.10
* libkmfl 0.9.12
* ibus-kmfl 1.0.8
