# Keyman Version History

## 14.0.22 alpha 2020-03-09

* fix(common): parameter order incorrect in git diff (#2787)
* feat(ios): compilations within Xcode now properly set version (#2775)
* fix(ios/engine): Fixes context bug for certain keyboard rules after newlines (#2770)
* feat(android): Update additional main app strings for crowdin (#2793)
* feat(ios): Start of Sentry-based crash reporting (#2771)
* feat(ios): Improved Sentry integration (first pass) (#2782)

## 14.0.21 alpha 2020-03-08

* fix(ci): builds were never triggered (#2790)

## 14.0.20 alpha 2020-03-08

* feat(windows): Chromium replacement for embedded MSHTML in for Keyman Desktop (#1720)
* Refactor/common/rename core (#2735)

## 14.0.19 alpha 2020-03-06

* fix(web): support otf extension for truetype fonts (#2780)

## 14.0.18 alpha 2020-03-04

* feat(windows): etl2log support tool (#2758)
* feat(developer): allow use of ISO9995 in key ids (#2741)
* Feature: Android - Handle keyman:// protocol to download kmp keyboard (#2734)
* change(android): Cleanup UI strings (#2751)
* fix(ios): fixes broken online-help versioned link (#2773)

## 14.0.17 alpha 2020-02-26

* fix(android) Fix crash on kbdMapList (#2719)
* fix(developer): crash when switching layout templates (#2726)
* feat(developer): always save options (#2731)
* feat(common): Support git worktree when configuring local hooks (#2722)
* docs(linux): Add linux packaging documentation (#2720)
* fix(developer): insert from charmap into touch editor 🍒 (#2737)
* fix(developer): debugger breaking smp with bksp (#2739)
* feat(ios): Adds keyboard-scale unit tests, fixes unknown-device bug (#2695)

## 14.0.16 alpha 2020-02-25

* fix(android) Fix crash on kbdMapList (#2719)
* fix(developer): crash when switching layout templates (#2726)

## 14.0.15 alpha 2020-02-24

* fix(android): Sanitize app version string for api query (#2715)
* chore(ci): Improve output when triggering Jenkins jobs (#2706)
* fix(ci): Fix increment-version.sh script (#2714)

## 14.0.14 alpha 2020-02-21

* chore(linux): allow to trigger Jenkins build from script (#2697)
* fix(ios): Resource update issues (#2703)
* fix(web): Web CI target reversion (#2693)
* refactor(common): Simplify and improve getting hook directory (#2701)

## 14.0.13 alpha 2020-02-19

* chore: merge p9s2 beta part 2 to master (#2683)

## 14.0.12 alpha 2020-02-18
* test(android): Fix build for KMSample1 project (#2669)
* fix(developer): upgrade removes preferences (#2668)
* chore(ci): Rename trigger-definitions.sh to *.config (#2665)

## 14.0.11 alpha 2020-02-17

* chore: merge beta changes to master 🍒 (#2659)
* fix(mac): invalid build script params removed (#2660)

## 14.0.10 alpha 2020-02-14

* refactor(mac): initial steps of input pathway (#2643)

## 14.0.9 alpha 2020-02-10

* fix(common/core): buffer overrun in context api (#2614)
* fix(ci): refactor trigger of test builds (#2611)
* fix(linux): Fix packaging of keyman-config on Xenial (#2609)

## 14.0.8 alpha 2020-02-07

* chore(ci): Tweak history management (#2602)
* chore(common): Update README.md (#2598)
* chore: update history (#2605)
* chore: merge beta P9S1 changes to master (#2606)
* chore(ci): is build required? (#2603)
* fix(linux): Install requirements before packaging (#2599)

## 14.0.7 alpha 2020-02-07

* chore(ci): Tweak history management (#2602)
* chore(common): Update README.md (#2598)

## 14.0.6 alpha 2020-02-06

* fix(linux): cherry-pick packaging changes from beta branch (#2589)
* fix(linux): Display the script name in log output (#2591)

## 14.0.5 alpha 2020-02-04

* chore(ci): support test builds on master/beta/stable-x.y (#2576)

## 14.0.4 alpha 2020-02-03

* chore(ci): trigger builds after version increment (#2572)

## 14.0.3 alpha 2020-02-03

* chore(ci): increment version final (#2568)
* chore(ci): add version tags (#2570)

## 14.0.2 alpha 2020-01-29

* chore(ci): version tags (#2562)

## 14.0.1 alpha 2020-01-29

* chore: Starting 14.0 release
