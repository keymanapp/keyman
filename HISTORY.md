# Keyman Version History

## 14.0.142 alpha 2020-09-15

* chore(deps): bump node-fetch from 2.6.0 to 2.6.1 in /resources/build/version (#3578)

## 14.0.141 alpha 2020-09-11

* fix(web): default layout shift not changing layer (#3574)

## 14.0.140 alpha 2020-09-09

* feat(common/models): core edit-distance calculation class (#3526)
* feat(common/models): Edit path derivation (#3547)
* feat(common/models):  low-level correction-algorithm infrastructure (#3527)
* feat(common/models): correction-search algorithm core (#3534)

## 14.0.139 alpha 2020-09-04

* fix(windows): BCP 47 tag canonicalization (#3545)
* fix(windows): upgrade of profiles from 13.0 (#3552)

## 14.0.138 alpha 2020-09-02

* feat(windows): map installed bcp47 (#3542)
* fix(windows): simplify profile uninstall (#3543)
* fix(android/app): Remove unused intent ACTION_GET_USERDATA (#3551)
* fix(android/engine): Remove notification after installing kbd package (#3546)
* fix(android): Re-enable monitoring of Application Not Responding (ANR) (#3550)

## 14.0.137 alpha 2020-08-31

* fix(android/engine): Fix package filename when downloading from cloud (#3541)
* feat(windows): select language at keyboard install (#3524)
* feat(windows): add langtags.json data (#3529)
* fix(windows): add transient profile support to keyman32 (#3539)

## 14.0.136 alpha 2020-08-28

* fix(android/app): Inject meta viewport tag for viewing help (#3523)
* fix(android/app): Fix overflow menu for hdpi devices (#3532)
* feat(windows): Rework of profile installation - Engine (#3509)
* feat(windows): profile installation - Keyman for Windows (#3510)
* feat(windows): profile installation - Support Tool (#3511)
* chore(windows): remove stockeditor (#3516)
* fix(windows): refresh configuration after changes (#3517)
* feat(windows): split language registration on app install (#3520)
* fix(linux): Improve robustness when installing ibus-keyman (#3535)

## 14.0.135 alpha 2020-08-27

* feat(common/models): override script defaults: spaces break words (#3506)
* fix(android/engine): Check asset package version before installing (#3514)
* feat(android): Use Stepper for navigating package installation steps (#3498)

## 14.0.134 alpha 2020-08-26

* [linux] Add packaging of Linux localization files (#3504)

## 14.0.133 alpha 2020-08-25

* feat(common/models): lexicon traversal (#3479)
* feat(common/models):  actual priority queue for Trie models (#3480)
* feat(common/models): efficient batch-enqueue (#3486)

## 14.0.132 alpha 2020-08-24

* feat(linux): Small UI improvements (#3495)

## 14.0.131 alpha 2020-08-21

* feat(android/app): Add language picker for keyboard package installation (#3481)

## 14.0.130 alpha 2020-08-19

* fix(android): Fix util to getting the tier on CI builds (#3491)

## 14.0.129 alpha 2020-08-17

* feat(windows): rework download keyboard dialog style (#3463)
* chore: Add sample vscode settings files (#3249)
* fix(linux): Lookup language tag from keyboard (#3408)

## 14.0.128 alpha 2020-08-13

* feat(linux): Hook up Sentry for km-config (#3378)
* chore(linux): Use new staging site names and use variable for downloads.keyman.com (#3406)

## 14.0.127 alpha 2020-08-12

* refactor(common/models): LMLayer state management tweak, persistent ModelCompositor (#3477)

## 14.0.126 alpha 2020-08-10

* fix(android): Add CI script to publish to Play Store (#3469)
* fix(android): Update sentry plugin to remove obsolete API  use (#3471)
* feat(ios/app): universal links for keyboard installation (#3466)
* fix(ios/engine): Proper associating-installer deinitialization, cancellation tests (#3468)
* refactor(ios): error definitions, i18n (#3470)
* chore: cleanup keyboardprocessor.h.in (#3473)

## 14.0.125 alpha 2020-08-07

* fix(android): Update Gradle wrapper to 5.6.4 (#3467)
* feat(ios/engine): Associating package installer (#3458)
* feat(ios/app):  Associating package installer use within existing install paths (#3465)

## 14.0.124 alpha 2020-08-06

* feat(windows): set default UI language at install (#3438)
* feat(windows): i18n for Setup (#3444)
* feat(windows): add globe icon to Configuration UI (#3446)
* feat(windows): set UI language from Setup preference on first install (#3447)
* feat(windows): docs on editing translations (#3448)
* Modify/android/improve string context (#3457)
* fix(android/app): Fix environment portion of app version string (#3462)

## 14.0.123 alpha 2020-08-05

* feat(android): Use staging help site for pre-release builds (#3453)
* fix(android): Cleanup string formatting with strings.xml (#3452)
* chore(android): Add script to find unused strings. Manually remove them (#3456)
* feat(ios/engine): LanguagePickAssociator progress tracking, base integration with package lang picker (#3455)

## 14.0.122 alpha 2020-08-04

* feat(android/app): Associate app with /keyboards/install links (#3449)
* fix(android): Update KMPBrowser to pass external links to user browser (#3439)
* feat(ios/engine): Language-pick associator (#3451)

## 14.0.121 alpha 2020-08-03

* fix(android): Fix KMTextView to compile on Linux (#3442)

## 14.0.120 alpha 2020-07-31

* feat(android): Use api.keyman-staging.com for pre-release builds (#3423)
* refactor(ios/engine): merges keyboard & lexical model info views, fixes QR code logic (#3432)
* feat(android): Add app association to keyman.com (#3431)
* chore(windows): cleanup comments in strings.xml (#3434)
* feat(ios/engine): language selection during (file-based) package installation (#3416)
* feat(ios/engine) Package-installer layout optimizations (#3437)

## 14.0.119 alpha 2020-07-30

* feat(windows): convert locale.xml to strings.xml format (#3424)
* refactor(windows): add translations to windows install (#3428)
* refactor(windows): remove old locale links (#3429)
* chore(windows): remove old locale tools (#3430)
* feat(ios): Start of engine and app internationalization (#2745)

## 14.0.118 alpha 2020-07-29

* fix(android/app): Use go/android/ links to download cloud keyboards (#3343)
* fix(windows): i18n of strings in Font Helper (#3414)
* refactor(windows): refactor Dialog elements (#3421)
* chore(windows): Remove usage page resources (#3422)

## 14.0.117 alpha 2020-07-28

* feat(windows): i18n groundwork (#3411)
* feat(windows): Removes unused strings from locale.xml (#3412)
* refactor(ios/engine): package installer tweaks (#3410)
* chore(ios/engine): more obsoletions (#3407)

## 14.0.116 alpha 2020-07-24

* refactor(ios/engine): builds package download links (#3383)
* refactor(ios/engine): chained lexical model install callback, fix (#3388)
* feat(ios/engine): download queue concurrency (#3395)
* feat(ios/engine): keyboard search core (#3384)
* feat(ios/engine): support for deferred chaining of lexical model downloads (#3401)
* docs(linux): Update readme (#3397)
* feat(ios/engine): use of Keyman staging sites (#3405)
* feat(linux): keyman:// protocol handler (#3398)
* chore(ios/engine): dead code removal + deprecations (#3402)

## 14.0.115 alpha 2020-07-23

* feat(windows): keyman: protocol handler (#3382)
* feat(developer/compilers): logging errors and warnings in the lexical model compiler (#3385)

## 14.0.114 alpha 2020-07-22

* chore(deps): bump lodash from 4.17.15 to 4.17.19 in /web/testing/regression-tests (#3360)

## 14.0.113 alpha 2020-07-21

* refactor(ios/engine): update detection now based upon packages (#3362)
* refactor(ios/engine): package download notifications (#3363)
* refactor(ios/engine): package-based resource updates, cloud resource migration (#3372)
* fix(ios/engine): engine tier enum, proper detection (#3373)
* refactor(ios/engine): Centralized Keyman domain definitions (#3381)
* fix(linux): Use language from search when installing keyboard (#3290)
* refactor(linux): Small refactorings (#3376)
* refactor(developer/compilers): reading from a wordlist (#3380)

## 14.0.112 alpha 2020-07-20

* refactor(ios/engine): package-version query caching + package state properties (#3335)
* refactor(ios/engine): keyboard downloads now retrieve KMPs (#3341)
* refactor(ios/engine):  download queue & download state detection cleanup (#3342)
* chore(developer): cleanup test constants (#3350)
* change(common/models/wordbreakers): update word boundary props to 13.0 (#3365)
* feat(linux): Use staging URLs for alpha version (#3364)

## 14.0.111 alpha 2020-07-16

* fix(android/engine): Update deprecated call to switch system keyboard for Android P (#3353)

## 14.0.110 alpha 2020-07-15

* refactor(developer): hashmap-based wordlist compilation (#3340)
* chore(linux): Add license details for kmpdetails.* (#3355)

## 14.0.109 alpha 2020-07-14

* refactor(ios/engine): lexical model query & download rework (#3327)
* refactor(ios/engine): hashable package keys (#3333)
* fix(ios/engine): version equality (#3334)
* refactor(linux): make some methods protected (#3291)
* feat(android/oem): Add nrc.str.sencoten model and update SystemKeyboard from KMAPro (#3332)

## 14.0.108 alpha 2020-07-09

* chore: cleanup global ExtPath (#3315)
* feat(android): Use build-download-resources.sh for KMApro and FV apps (#3322)
* refactor(ios/engine): Centralized package download utility function (#3299)
* fix(android/oem): Add a fallback keyboard to FV Android app (#3323)

## 14.0.107 alpha 2020-07-08

* feat(android): Add additional info on installed keyboards exceptions (#3319)

## 14.0.106 alpha 2020-07-04

* fix(android/app) Download associated dictionary when installing cloud keyboard package (#3307)

## 14.0.105 alpha 2020-07-03

* feat(android): Download default resources at build time (#3300)
* fix(android/engine): Fix unbound variable in build script (#3308)

## 14.0.104 alpha 2020-07-01

* fix(android): Resize PackageActivity title text (#3297)

## 14.0.103 alpha 2020-06-30

* fix(common/core/web): fixes transcription buffer cap (#3301)

## 14.0.102 alpha 2020-06-29

* fix(android): Use tier to determine keyboard search host (#3296)
* refactor(ios/engine): Resource download cleanup and mocking prep (#3292)
* refactor(ios/engine): ID-based download tracking, update notification rework (#3295)

## 14.0.101 alpha 2020-06-26

* feat(linux): address code review comments of #3278 (#3281)
* fix(linux): Always restart IBus when installing keyboard (#3284)
* refactor(linux): cleanup code (#3286)
* fix(linux): Fix crash converting kvk into LDML (#3288)
* feat(ios/engine): package-version query implementation (#3280)

## 14.0.100 alpha 2020-06-25

* refactor(ios/engine): Download notifications via completion blocks (#3274)
* refactor(ios/engine): better resource type links (#3276)
* refactor(ios/engine): Resource-download installation closures (#3277)
* feat(linux): Install keyboard on Gnome (#3278)

## 14.0.99 alpha 2020-06-24

* feat(ios/engine): adds getInstalledPackage(for:) (#3270)
* refactor(linux): address code review comments of #3272 (#3275)

## 14.0.98 alpha 2020-06-23

* refactor(ios/engine): reworks download queue objects (#3267)
* feat(linux): Improve Linux package description (#3268)

## 14.0.97 alpha 2020-06-22

* feat(android): Implement menu to add language for installed keyboard pkg (#3255)
* fix(mac/resources): altool is failing but we lose the log in CI (#3264)
* refactor(ios/engine): more notification rework prep (#3262)
* fix(mac): altool second call report ci errors (#3265)
* refactor(ios/engine): more dead code removal (#3266)

## 14.0.96 alpha 2020-06-19

* fix(ios/engine): fixes keyboard swapping (#3259)
* fix(common/resources): shebang for lerna-based Linux builds (#3260)
* refactor(ios/engine): Notification rework prep, simplification (#3261)

## 14.0.95 alpha 2020-06-19

* feat(android/app): Consolidate install menus (#3245)
* change(ios/engine): migrates legacy cloud resources to KMP format (#3237)
* change(ios/engine): Default resource update (#3244)
* fix(ios/engine): migration of preload-sourced resources (#3247)
* refactor(common/resources): web-environment package (#3248)
* chore(linux): Add some unit tests for keyman_config (#3250)
* chore(linux): Update automatically installed dependencies (#3254)
* fix(common/resources): web-environment package-lock.json (#3256)

## 14.0.94 alpha 2020-06-16

* feat(linux): Add onboard as recommended package (#3241)

## 14.0.93 alpha 2020-06-15

* fix(developer/ide): double click to change layer update combo (#3240)
* fix(developer/ide): fix copy and paste in touch editor (#3238)

## 14.0.92 alpha 2020-06-12

* change(ios/engine): Package migration prep (#3229)
* chore: labeler (#3233)
* feat(android/engine): Migrate cloud keyboards when updating keyboard package (#3221)
* refactor(ios/engine): explicitly synchronous Package processing (#3230)
* feat(ios/engine): KeymanPackage now parses its ID (#3234)
* fix(windows): sentry exception reports (#3235)

## 14.0.91 alpha 2020-06-11

* chore(android): Remove google-services and update readme (#3228)

## 14.0.90 alpha 2020-06-10

* refactor(ios/engine): Language resource + keying polymorphism (#3220)
* refactor(ios): Selective resource installation via abstraction (#3207)
* feat(ios/engine): Generation of kmp.json for non-package resources (#3212)

## 14.0.89 alpha 2020-06-09

* fix(android/engine): Revise updateOldKeyboardsList (#3216)
* feat(ios/engine): embedded KeymanWeb engine Sentry use (#3218)
* feat(android): Handle model picker updates (#3209)

## 14.0.88 alpha 2020-06-08

* modify(android/samples): Update addKeyboard syntax for Sample and Test apps (#3213)
* fix(linux): Restart km-config after installing keyboard (#3214)

## 14.0.87 alpha 2020-06-05

* fix(developer/compilers): normalise touch layout on compile (#3203)
* feat(android): Add utility to check BCP47 equivalence (#3210)
* refactor(ios/engine): KMP installation now relies on KeymanPackage class, parses (#3205)
* refactor(ios/engine): Abstraction of KMP installation methods (#3206)
* feat(linux): Make two windows modal dialogs (#3211)

## 14.0.86 alpha 2020-06-02

* fix(ios): fixes lerna concurrency workaround and missing func reference (#3192)
* fix(android/engine): Use available models for ModelPickerActivity() (#3191)
* feat(android): Add system globe action to show system keyboards (#3197)
* feat(linux): Open a .kmp file in km-config (#3183)
* refactor(ios/engine): kmp.json - Keyboard info deserialization (#3193)
* refactor(ios/engine): kmp.json - Lexical model info deserialization  (#3195)
* feat(ios/engine): kmp.json - full deserialization (#3198)
* fix(android): Add wrapper for logging errors & exceptions (#3196)
* fix(oem/fv/ios): new certificate (#3201)

## 14.0.85 alpha 2020-05-30

* modify(android/engine): Clean up naming for formatting QR code URL (#3187)

## 14.0.84 alpha 2020-05-29

* change(web): Selective Sentry uploads (#3163)
* refactor(ios/engine): Polishes HTTPDownloader to prep for unit testing (#3179)
* feat(ios/engine): HTTPDownloader unit testing (#3180)
*  fix(linux): Disable buttons if no keyboard installed (#3184)
* change(ios/engine):  Removes unused, deprecated JS/JSON-based adhoc install code (#3186)
* change(common/lmlayer): move joiner from LMLayer to model compiler (#3071)

## 14.0.83 alpha 2020-05-28

* change(ios): Selective Sentry uploads (#3162)
* Fix/android/package updates (#3175)
* fix(common/models/types): fixes models-types build script, sets std header (#3182)

## 14.0.82 alpha 2020-05-27

* fix(ios): fixes build warning on Error coersion, fixes logged error for intermediate embedded state (#3173)
* fix(linux): Don't fail (un-)install if running multiple ibus (#3169)
* chore(linux): Update maintainer in package metadata (#3168)
* feat(linux): Improve appstream metadata (#3176)

## 14.0.81 alpha 2020-05-27

* refactor(resources): convert gosh into npm package 🙃 (#3159)
* chore(common,web): use consistent TypeScript dep on all packages (#3158)
* chore(common/resources): add `common/models` to build trigger definitions (#3144)
* fix(common): adds package-lock.json for gosh package (#3171)

## 14.0.80 alpha 2020-05-25

* fix(android/engine): Remove LanguageListUtil (#3155)
* refactor(common/lmlayer): create package: @keymanapp/models-templates (#3128)

## 14.0.79 alpha 2020-05-22

* fix(android): Fix system keyboard globe button override (#3140)
* fix(web/utils): use build script to generate environment.inc.ts (#3146)
* change(common/core/web,web): update deps to @keymanapp/models-types (#3147)

## 14.0.78 alpha 2020-05-21

* refactor(common/models): factor out word breakers to their own package (#3125)
* fix(developer): crash when developer starts (#3138)
* modify(android): Refactor KeyboardPickerActivity (#3113)
* chore(common/resources): cleanup scopes (#3139)
* feat(web): starts proper KMW Sentry integration (#3122)
* refactor(common/models/types): rename @keymanapp/lexical-model-types => @keymanapp/models-types (#3143)
* change(web/utils): add and distribute type declaration (#3145)
* fix(common): drops version updates for deprecated common/lexical-model-types (#3148)

## 14.0.77 alpha 2020-05-19

* refactor(web/engine): Moves common utility functions into separate `web-utils` package (#3130)
* refactor(web/engine): renames DeviceSpec, moves to utils (#3132)
* fix(ios): Fixes keyboard metadata decoding, tweaks to project files  (#3137)
* feat(windows): Use http: instead of file: for Configuration UI (#3127)

## 14.0.76 alpha 2020-05-15

* fix(windows): use correct name for Sentry in C++ (#3129)
* chore: Allow to override hook defs (#3112)

## 14.0.75 alpha 2020-05-13

* feat(android): Add Tamil lexical model to KMSample2 (#3123)

## 14.0.74 alpha 2020-05-11

* fix(windows): kmbrowserhost was missing debug info (#3117)

## 14.0.73 alpha 2020-05-11

* fix(windows): force output keystroke failure (#3083)
* fix(windows): kmshell title and kmbrowserhost sentry (#3115)

## 14.0.72 alpha 2020-05-08

* fix(ios/oem): Fixes FV app's system keyboard (#3105)
* fix(ios/oem): FV light mode lock and basic banner fix (#3108)
* fix(windows): sentry cef shutdown interactions (#3107)
* feat(android): Migrate installed keyboards list to keyboards_list.json (#3091)

## 14.0.71 alpha 2020-05-08

* fix(windows): use consistent sentry db location (#3100)

## 14.0.70 alpha 2020-05-07

* fix(android): Fix back button after System Keyboard dismissed (#3093)
* fix(android): Back button to dismiss KMSample2 system keyboard (#3095)
* fix(oem): Back button to dismiss FV Android system keyboard (#3096)

## 14.0.69 alpha 2020-05-06

* fix(common): npm install required for auto inc lerna versions (#3089)
* fix(windows): sentry x64 stacks truncated pointers (#3087)

## 14.0.68 alpha 2020-05-06

* fix(common): enable build for win x64, use global VERSION.md and fix decxstr() bug (#3076)
* modify(android): Add methods to go between LanguageResource and JSON (#3079)
* feat(common): auto-update for package versioning (#3078)

## 14.0.67 alpha 2020-05-05

* fix(ios): Additional libraries for FirstVoices SWKeyboard (#3080)

## 14.0.66 alpha 2020-05-04

* feat(web): test Recorder overhaul, Node-based tests using Recorder for KeyboardProcessor (#3060)
* fix(ios): fv add sentry framework to carthage build step (#3074)

## 14.0.65 alpha 2020-05-03

* fix(ios): try embed of sentry again for fv (#3072)

## 14.0.64 alpha 2020-05-01

* fix(ios): add sentry framework to fv keyboards (#3069)

## 14.0.63 alpha 2020-05-01

* fix(android): Fix FileUtilsTest to be cross-platform (#3061)
* fix(windows): add LARGEADDRESSAWARE flag for all CEF processes (#3064)
* Fix/android/remove more custom (#3051)
* fix(ios): FV settings bundle for SWKeyboard (#3066)
* change(common/lmlayer): single-pass join word breaker decorator (#3059)

## 14.0.62 alpha 2020-05-01

* modify(oem): Update DSN for FV Android app (#3050)
* feat(web/engine): Basic KeyboardProcessor tests (#2994)
* fix(oem,common): Update FirstVoices build_keyboards.sh script (#3045)

## 14.0.61 alpha 2020-04-30

* chore(deps): bump @actions/http-client from 1.0.3 to 1.0.8 in /resources/build/version (#3047)
* fix(ios): firstvoices icon and version info (#3044)
* feat(windows): Sentry integration fixes and polish (#3006)
* feat(common/lmlayer): compile joinWordsAt property (#3032)

## 14.0.60 alpha 2020-04-29

* feat(common): initial use of lerna (in-repo package links only) (#2997)
* change(web/engine): spins core/web/keyboard-processor package off from KeymanWeb (#3001)
* change(web/engine): spins core/web/input-processor package off from KeymanWeb (#3008)
* chore(common/lmlayer): change author from personal to work affiliation (#3046)

## 14.0.59 alpha 2020-04-29

* chore: merge stable history (#3037)
* feat(common/lmlayer): create join word breaker decorator (#3021)
* fix(ios): download fv keyboards (#3040)
* modify(android): Refactor LanguageResource() and remove "Custom" property (#3033)

## 14.0.58 alpha 2020-04-28

* change(common/lmlayer): improvements to default searchTermToKey (#3024)

## 14.0.57 alpha 2020-04-27

* modify(common): Update to Unicode 13.0 (#3029)
* modify(android): Refactor Keyboard class to not use Map (#3020)

## 14.0.56 alpha 2020-04-24

* fix(android): Clarify label that shows "Get Started" on startup (#3025)
* feat(common/lmlayer): allow for verbose word breaker specification (#3023)

## 14.0.55 alpha 2020-04-23

* modify(android): Convert LanguageListActivity to utility (#3018)
* refactor(web/engine): successful web-core compilation (#2992)
* change(common/lmlayer): always bundle searchTermToKey() with model (#2971)
* change(common/lmlayer): remove NFD table (#3014)

## 14.0.54 alpha 2020-04-22

* refactor(common/lmlayer): word breaker compilation (#3016)

## 14.0.53 alpha 2020-04-21

* refactor(common/lmlayer): Abstracted connection between LMLayer and Worker initialization (#2986)
* refactor(common/lmlayer): starts a formal 'headless' mode (#2987)
* fix(android): predictive banner display bugfix (#3010)
* fix(android): Fix system keyboard alignment (#3009)

## 14.0.52 alpha 2020-04-17

* refactor(web/engine): precomputation for OSK key events, headless production thereof (#2969)
* refactor(web/engine): initial ModelManager split (#2974)
* refactor(web/engine): lm-layer enablement state management rework (#2975)
* refactor(web/engine): predictive data routing, LanguageProcessor as EventEmitter (#2976)
* refactor(web/engine): web-core build prep (#2982)

## 14.0.51 alpha 2020-04-16

* modify(android): Update minimum SDK to 21 (#2993)

## 14.0.50 alpha 2020-04-15

* fix(android): Handle default font DejaVuSans.ttf (#2981)
* feat(android): Download cloud keyboards from https://keyman.com/keyboards (#2953)
* fix(ios): iOS 13.4 subkey menu workaround (#2959)
* change(ios): Web-based popup key longpresses (#2968)
* fix(web):  repairs Web regression test suite (#2973)
* feat(android): Dismiss system keyboard on Back press (#2984)

## 14.0.49 alpha 2020-04-11

* chore(common/lmlayer): do not run tests in IE11 in Windows (#2978)

## 14.0.48 alpha 2020-04-07

* feat(windows): use crashpad and better call stacks (#2931)

## 14.0.47 alpha 2020-04-07

* fix(common/lmlayer): use searchTermToKey() on input (#2954)
* refactor(web/engine): proper split-off of DOM-reliant code (#2939)
* refactor(web/engine): InputProcessor/KeyboardProcessor split (#2940)
* fix(ios): prevents in-app keyboard resets (#2951)
* refactor(web/engine): headless KeyboardProcessor (#2941)

## 14.0.46 alpha 2020-04-06

* fix(android): Add check for WRITE_EXTERNAL_STORAGE permission (#2946)

## 14.0.45 alpha 2020-04-05

* chore(deps-dev): bump minimist from 1.2.2 to 1.2.3 in /web/testing/regression-tests (#2947)

## 14.0.44 alpha 2020-04-03

* refactor(web/engine): begins formally removing DOM-aware keyboard API functions from web-core KeyboardInterface (#2915)
* refactor(web/engine): start of system store abstraction (#2919)
* refactor(web/engine): Processor now manages current layer; OSK listens via callback (#2920)
* refactor(web/engine):  RuleBehavior now headless (#2925)
* refactor(web/engine): variable store storage abstraction (#2926)
* fix(web): fixes activeElement typing (#2927)
* refactor(web/engine): relocates DOM-only parts of Processor  (#2938)
* fix(android): Change KeyboardHarness/build.sh to not rebuild KMEA (#2943)
* feat(android): Propagate languageID when downloading kmp (#2944)

## 14.0.43 alpha 2020-04-02

* fix(web/engine): default layout fix for chiral keyboards (#2936)
* fix(android): Fix exception in ResourcesUpdateTool (#2933)
* modify(android): Update sample and test projects to install asset kmp's (#2935)

## 14.0.42 alpha 2020-04-01

* fix(android): Fix globe button crash on 3rd party apps (#2930)
* modify(android) Install default asset kmp's (#2928)

## 14.0.41 alpha 2020-03-31

* refactor(web/engine): KeyboardInterface/Processor cleanup and prep (#2901)
* refactor(web/engine): Preps keyboard layouts definitions for web-core (#2902)
* refactor(web/engine): layouts now full property of Keyboard wrapper (#2903)
* feat(android): Specify optional language ID for installing kmp (#2921)

## 14.0.40 alpha 2020-03-30

* fix(developer): use correct registry types for sentry options (#2912)
* fix(windows): replaces empty eventid with underscore when autoreport errors off (#2913)
* refactor(web): namespacing DOM-focused management (#2891)
* refactor(web/engine): starts DOM pre-processor (#2892)
* refactor(web): starts OSK preprocessor (#2893)
* refactor(web): headless device representation (#2894)
* refactor(web): cleanup for Processor.processKeyEvent (#2899)
* chore(common): Cleanup unused folders and update README (#2916)

## 14.0.39 alpha 2020-03-29

* feat(windows): user control for upload to sentry (#2900)

## 14.0.38 alpha 2020-03-28

* feat(ios): Add Crowdin CLI for iOS strings (#2905)

## 14.0.37 alpha 2020-03-27

* feat(windows): crash reports in CEF (#2887)
* feat(common): Use Crowdin CLI (v3) for handling l10n files (#2895)
* fix(windows): show error if tsysinfo fails to start; kmcomapi reporting (#2897)

## 14.0.36 alpha 2020-03-26

* fix(web/engine): Fixes default key lookup returns (#2890)

## 14.0.35 alpha 2020-03-25

* feat(windows): new error notification dialog (#2875)
* feat(windows): sentry c++ wrappers (#2886)
* refactor(web/engine): activeKeyboard now tracked on Processor (#2864)
* refactor(web/engine): Initial definition of typed Keyboard wrapper (#2868)
* refactor(web/engine): More Keyboard wrapper properties (#2869)
* refactor(web/engine): headless-friendly keymapping (#2870)
* refactor(web/engine): relocates keyboard tag code, adds typing (#2883)
* fix(web): fixes variable stores (#2884)

## 14.0.34 alpha 2020-03-24

* fix(web): fixes embedded kbd initialization (#2879)
* fix(ios): popup keys over base keys no longer emit base char. (#2881)

## 14.0.33 alpha 2020-03-23

* fix(android): Update min SDK versions for sample apps (#2872)
* refactor(web/engine): extended abstraction with OutputTarget (#2849)
* refactor(web/engine): reworks use of embedded's keyman['oninserttext'] (#2850)
* refactor(web/engine): reworks default output handling to return RuleBehaviors (#2854)
* refactor(web/engine): removes shiftState parameter (#2859)
* refactor(web/engine): moves new RuleBehavior type & behaviors to own file (#2861)
* refactor(web/engine): doInputEvent moved to OutputTarget (#2862)
* refactor(web/engine): KeyboardInterface now property of Processor (#2863)
* fix(windows): some sentry symbolication was not working (#2871)
* fix(linux): Use __release_version__ for downloadkeyboard window (#2877)

## 14.0.32 alpha 2020-03-20

* fix(windows): include sources for sentry (#2866)

## 14.0.31 alpha 2020-03-20

* fix(android): Fix min SDK version for Sample and Test apps (#2860)

## 14.0.30 alpha 2020-03-19

* docs(common): minor updates to readme (#2856)

## 14.0.29 alpha 2020-03-19

* feat(windows): Add Sentry reports to Delphi apps (#2848)

## 14.0.28 alpha 2020-03-18

* fix(web): fixes design mode and content editable issues (#2838)
* fix(ios): corrects OSK height adjustment, banner display issues. (#2840)
* feat(windows): add sentry tooling (#2806)
* refactor(web/engine): new RuleBehavior return type from keyboard calls, utilization (#2830)
* refactor(web/engine): KeyEvent object now refers to outputTarget over element (#2846)
* fix(linux): Fix failing Linux package builds (#2843)
* feat(linux): Add focal as platform to build packages for (#2842)

## 14.0.27 alpha 2020-03-17

* chore(deps-dev): bump minimist from 1.2.0 to 1.2.2 in /web/testing/regression-tests (#2829)

## 14.0.26 alpha 2020-03-16

* fix(oem): Disable monitoring of ANR for oem Android app (#2828)

## 14.0.25 alpha 2020-03-13

* feat(common): sentry release control (#2794)
* chore(windows): improve build script tests (#2680)
* fix(web): enhanced sourcemaps + proper sourcemaps for minified KMW (#2809)
* feat/common/parse-crowdin Add script to parse crowdin translation file (#2801)
* fix(linux): Fix how keyboardprocessor version is set in dist.sh (#2814)
* feat(android): Additional Sentry integration (#2810)
* fix(web): applies base key layer property to unassigned subkeys (#2808)
* feat(android): Start adding RTL to layouts (#2816)
* fix(common/lmlayer): fixes word lookup from Tries for SMP script-based languages (#2815)
* feat(common): add release finalization for Sentry (#2819)
* fix(web): further fixes BuildVisualKeyboard.  Fixes #2818 (#2822)
* fix(web): fixes internal reference for validation tool (#2824)

## 14.0.24 alpha 2020-03-11

* fix(linux): Fix CI dist path to common/core/desktop (#2795)
* fix(web): fixes build number reference of API call (#2796)
* fix(web): updating BuildVisualKeyboard (#2802)

## 14.0.23 alpha 2020-03-10

* fix(ios): fixes Carthage framework copy for Sentry (#2800)
* feat(android): Start of Sentry-based crash reporting (#2778)

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
