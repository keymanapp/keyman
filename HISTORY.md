# Keyman Version History

## 18.0.188 alpha 2025-02-10

* fix(windows): check the params status flag equals ucrsUpdateReady before attempting to download the keyman setup file (#13154)
* feat(windows):  background updates go from downloading to waiting for a restart except if the `apply now` flag is set (#13159)
* feat(developer): verify package version number format in kmc-package (#13118)
* fix(developer): support non-US base keyboard layouts in debuggers (#13131)
* feat(developer): improve compiler messages and user interface (#13156)
* feat(developer): verify that packages do not contain themselves in kmc-package (#13157)
* fix(developer): link welcome.htm in package for new projects; use v17 project format for new models (#13161)
* fix(windows): use Automatically check for updates and download for the english xml (#13162)
* change(linux): Implement ordered output without patched ibus (#11535)

## 18.0.187 alpha 2025-02-07

* docs(web): relocate gesture docs to web/docs/internal (#13139)
* chore(mac): remove empty options tab from configuration (#13160)
* chore(linux): Update debian changelog (#13143)

## 18.0.186 alpha 2025-02-06

* feat(windows): check updates and automatic update merged (#13115)

## 18.0.185 alpha 2025-02-05

* fix(developer): make kmc log options consistent across all commands (#13075)
* feat(developer): add user interface for `kmc copy` to TIKE (#13076)
* chore(developer): add verbose logs for project copier (#13080)
* chore(mac, ios): build and run with xcode 16 (#13077)

## 18.0.184 alpha 2025-02-04

* fix(common): handle undefined input in `@keymanapp/langtags` lookups (#13117)
* chore: update docs to reference node 20.0 (#13124)
* feat(developer): include command line in kmc sentry reports (#13113)
* fix(common/web): replace invalid StrsItem identity checks with isEqual() (#13122)

## 18.0.183 alpha 2025-02-03

* change(ios): set host-app bundle identifier in embedded Web debug-report (#13105)
* fix(ios): prevent message-handler collision (#13037)
* change(web): add context tracker bksp handling, alignment-offset calculations (#12911)
* fix(web): correct edit-path ordering at source (#12925)
* fix(web): correctly track context transitions caused by applying suggestions (#12927)
* chore(linux): preserve log files when running tests under Docker (#13107)

## 18.0.182 alpha 2025-01-31

* feat(windows): report on processor type in tsysinfo (#13084)
* fix(common): push branch when creating PR in version history writer (#13082)
* refactor: move langtags to @keymanapp/langtags npm module (#13046)
* chore(developer): use langtags.json for kmc-generate language names (#13047)
* feat(developer): support relocation of external files in kmc-copy (#13061)
* feat(windows): kmshell switch handling for the installing state (#12956)
* feat(windows): add boot switch (#12995)
* docs(web): remove `.md` in help links (#13051)
* chore: add android platform to titles (#13033)
* chore: add the windows platform to titles (#13026)
* fix(mac): properly manage Input Method lifecycle (#13006)
* chore(developer): handle forward slashes in paths in package editor (#13081)
* chore(developer,core,resources): LDML v46 update and remove workaround for import base (#13099)

## 18.0.181 alpha 2025-01-30

* feat(windows): add localized button captions and form labels (#13025)
* fix(web): disabled correction state should prevent predictive corrections (#13066)
* fix(web): disabled autoaccept should not autohighlight suggestions (#13068)
* chore(common): add 17.0.334 to version history (#13065)
* fix(windows): add api GetContext and TIPProcessKeyEx with IsTextSelected bool (#13023)
* fix(web): keyboard query error states should always provide informative Error objects, reports (#13048)

## 18.0.180 alpha 2025-01-29

* chore(common): Update crowdin strings for Portuguese (#13056)
* chore(common): Update crowdin strings for Spanish (Latin America) (#13044)
* chore(linux): Update debian changelog (#13063)
* chore(linux): improve compatibility with Gentoo Linux (#13070)

## 18.0.179 alpha 2025-01-28

* fix(android): improve resource-update tool handling of host Activity's closure (#13004)
* fix(windows): bool return value of KeymanIsTextSelected (#12841)
* chore(mac): add breadcrumbs for Sentry (#12939)
* fix(mac): show and hide OSK consistently for all keyboards (#12988)
* fix(linux): remove unnecessary files from source tarball (#13053)
* fix(windows): handle keyboard package not downloaded (#12948)

## 18.0.178 alpha 2025-01-27

* fix(web): fix handling of data: url fonts, fonts with single quotes in filename (#13032)
* fix(android): disable early keyboard-loading when not feasible (#13001)
* fix(web): add null guard for focusing next element via keystroke (#13013)
* chore: adjust one bookmark link in windows doc (#13050)
* chore: adjust bookmark links in developer docs (#13049)

## 18.0.177 alpha 2025-01-24

* fix(android): restores DOMRect polyfill for old Chrome support (#13012)
* fix(developer): produce correct index parameter value for context in kmw compiler (#13003)
* chore: add platform to what's new titles (#12989)
* chore: add platforms to welcome titles (#12990)
* chore: add platforms to version history titles (#13016)
* chore: add platforms to troubleshooting titles and windows to help homepage (#13019)
* chore: add platforms to system requirements and download-and-install docs (#13024)
* chore: add mac platform to titles (#13027)
* chore: add linux platform to titles (#13028)
* chore: add ios platform to titles (#13031)
* fix(developer): escape font facename in touch layout editor (#13020)
* fix: use correct path for resources/build/version (#13029)
* fix(developer): don't list .js keyboards for multi-keyboard packages in kmc-keyboard-info (#13034)
* fix(web): do not simplify nul-prefixed contexts when rule-matching (#12998)
* feat(android): add breadcrumbing during package install about package file, source (#13021)
* fix(linux): correct permissions and paths in Linux docker image (#13008)

## 18.0.176 alpha 2025-01-23

* chore(developer): stub out icon support in kmc-generate (#12971)
* chore(developer): remove '--rename' option from kmc-copy (#12973)
* feat(developer): add 'verbose' and 'debug' log levels to kmc (#12976)
* chore(developer): validate output folder name in kmc-copy (#12979)
* chore(developer): report on filename collisions in kmc-copy (#12982)
* feat(developer): support copying disorganized projects in kmc-copy (#12984)
* chore(linux): docker build: update package index before installing packages in Dockerfiles (#12992)
* feat(web): allow main manual-test page to load from KMP (#12977)
* fix(android): Remove access to MEDIA storage permissions (#12999)

## 18.0.175 alpha 2025-01-22

* feat(windows): Fixes check now to use SM and LastUpdateCheckTime key on first run. (#12922)
* chore(developer): validate keyboard and model IDs in kmc-generate (#12958)
* fix(developer): use correct terms in kmc-generate lexical model help (#12960)
* fix(developer): use 1.0.0 as default for ldml keyboards in kmc-generate (#12962)
* chore: adds h1 tags (#12967)
* fix(web): prevent warning that occurs when multitapping longpressable modifier keys (#12964)
* chore(developer): add test for basic transform to kmc-generate (#12966)
* fix(android/engine): Revert how keyboard picker menu launches (#12986)

## 18.0.174 alpha 2025-01-21

* chore(developer): validate targets parameter in kmc-generate (#12955)
* fix(developer): support `&displayMap` font in web debugger (#12929)

## 18.0.173 alpha 2025-01-20

* feat(developer): rewrite font data in .kvk from package metadata (#12949)
* docs(ios): document gestures within iOS app help (#12900)
* fix(developer): include developer-utils deps in server build (#12951)

## 18.0.172 alpha 2025-01-19

* chore(common): Add 17.0.333 to version history (#12926)

## 18.0.171 alpha 2025-01-18

* fix(common/web): add StrsItem.isEqual() method (#12868)
* fix(common/web): handle invalid order and tertiary arguments to ElementString.fromString() (#12882)

## 18.0.170 alpha 2025-01-17

* chore(linux): update copyright year (#12918)
* fix(web): patch unit test with execution dependent on autocorrect state (#12940)
* test(common/web): unit tests for element-string (#12811)
* chore(linux): Update debian changelog (#12917)

## 18.0.169 alpha 2025-01-17

* chore(windows): remove `postinstall` state from mermaid diagram (#12923)

## 18.0.168 alpha 2025-01-16

* chore: update macOS environment-variable shell script (#12878)
* fix(android): use main looper to dispatch key events when OSK is hidden (#12871)
* chore(web): integrates predictive-text builds to top level script, reconnects headless tests (#12866)
* chore(ios): Update crowdin strings for Khmer (#12910)
* chore(windows): merge master epic windows updates (#12904)
* fix(developer): detect invalid key ids in touch layout files (#12895)
* chore: use GitHub PR titles when writing HISTORY.md (#12907)
* fix(developer): filter incorrect fonts out of .keyboard_info (#12909)
* change(web): make 'keep' transform pattern match standard suggestion pattern by including the prefix string (#12906)
* chore: Add docker images for building the different platforms (#11397)

## 18.0.167 alpha 2025-01-15

* chore: changes to use https and removes anchor in docs (#12838)
* fix(developer): ensure license parameter is required and add tests (#12879)
* fix(web): Improve predictive-text handling of text wordbreaking transitions (#12864)
* chore(common): Update crowdin strings for Spanish (#12886)
* docs(developer): Clarify square brackets in `U_####[_####]` shortcut (#12876)
* feat: add builder tab-completion script (#12296)
* docs(android): document longpress up-flick to quick-display shortcut (#12899)
* docs(web): fix `adding-keyboards.md` (#12890)

## 18.0.166 alpha 2025-01-10

* fix(web): numpad + and - with zoom shortcut use (#12865)

## 18.0.165 alpha 2025-01-09

* feat(web): add KeymanWeb API for auto-correct (#12857)
* chore(web): disable auto-correct (#12858)
* refactor(web): modernize `web/ci.sh` script (#12862)

## 18.0.164 alpha 2025-01-08

* fix(windows): add language and close button fit inside pop-window (#12855)
* chore(windows): add bcp47 code and tips to diagnostic (#12843)
* fix(android/engine): Cleanup KMManager API keys and calls (#12861)
* docs(developer,mac): add playable video links to the help documentation (#12828)

## 18.0.163 alpha 2025-01-06

* docs: clarify that sections in KMX file might not be aligned (#12856)

## 18.0.162 alpha 2024-12-19

* fix(mac): standardize interaction between OSK and physical keyboard (#12836)

## 18.0.161 alpha 2024-12-17

* fix(mac): OSK layers displayed consistently for hardware and OSK modifiers (#12829)

## 18.0.160 alpha 2024-12-16

* feat(ios): configurable keyboard height (#12571)
* chore(common/web): remove .c8rc.json from types and merge exclusions into package.json (#12813)
* fix(android/engine): Initialize index when resuming KeyboardPicker (#12832)

## 18.0.159 alpha 2024-12-13

* chore(core): remove meson warnings for wasm builds (#12827)
* chore(linux): Update debian changelog (#12023)

## 18.0.158 alpha 2024-12-11

* fix(linux): pushing of updated changelog branch (#12818)
* fix(linux): work around Lintian errors (#12815)

## 18.0.157 alpha 2024-12-10

* test(common/web): unit tests for kvk-file-writer (#12734)

## 18.0.156 alpha 2024-12-09

* fix(developer): remove platforms from kmc-generate LM readme (#12803)
* test(developer): add test for ERROR_DescriptionIsMissing to kmc-keyboard-info (#12804)
* chore(developer): verify bundled node version when building installer (#12806)
* fix(developer): support hint property in displaymap (#12807)
* fix(common/web): delete replaceExtension in types/src/util/file-types.ts (#12762)
* chore(developer): add some docs for language examples in kmp.json (#12805)
* fix(core): implement ldml_processor::get_key_list() (#12644)

## 18.0.155 alpha 2024-12-07

* chore(android,windows): Update crowdin for Czech (#12792)

## 18.0.154 alpha 2024-12-06

* feat(developer,core): local imports (#12750)
* chore(core): remove `km_core_keyboard_load` API (#12769)
* chore: rename TestCompilerCallbacks.ts (#12775)
* chore(mac): update to SIL logo with glyph in About window (#12766)
* chore(android,windows): Update crowdin for Italian (#12793)
* change(developer): use full github url in kmc copy parameters (#12773)
* chore(developer): add baseline tests for bcp47 codes to kmc-package (#12506)
* chore(core): only install node on Windows if not available (#12772)
* chore(android): Disable auto-correct UI controls (#12791)

## 18.0.153 alpha 2024-12-05

* feat(developer,common): verify normalization of strings (#12748)
* chore(core): Add link to Keyman Glossary (#12774)
* test(common/web/types): unit tests for file-types (#12716)

## 18.0.152 alpha 2024-12-04

* refactor(mac): pass kmx data blob to keyman core instead of file path (#12760)
* fix(developer): honour provided script when checking for matching scripts (#12768)
* chore(common): rename test files (#12709)
* fix(common): rename test file (#12770)

## 18.0.151 alpha 2024-12-03

* feat(android): Enhance how ENTER key is handled for FV and KMSample2 (#12745)
* feat(developer): report on mismatching lang tag scripts when building keyboard-info (#12753)

## 18.0.150 alpha 2024-12-02

* fix(core,developer): use `NDEBUG` flag to disable assertions in release build (#12715)

## 18.0.149 alpha 2024-12-01

* refactor(developer): unify test action (#12736)

## 18.0.148 alpha 2024-11-29

* test(common/web/types): unit tests for unicodeset-parser-api (#12714)
* chore(developer): rename test files (#12707)
* feat(core,linux,developer,windows): implement loading KMX from blob (#12721)
* chore(common): add offline support for emscripten (#12740)

## 18.0.147 alpha 2024-11-28

* docs(android): Add android/docs/internal/README (#12717)
* test(common/web/types): unit tests for string-list (#12702)
* docs(common): linux and macOS emscripten setup (#12701)
* refactor(developer): output number of tests when running on TC (#12710)
* refactor(common): output number of tests when running on TC (#12719)
* chore(web): rename file missed in #12704 (#12720)
* fix(core): permanently disable logging (#12724)
* fix(linux): disable assertions in release builds of ibus-keyman (#12725)
* chore(common): improve offline builds (#12739)

## 18.0.146 alpha 2024-11-27

* test(developer): kmcmplib compiler unit tests 5 (#12612)
* refactor(common): move all lexical model types into `LexicalModelTypes` container (#12712)
* refactor(common): move remaining LDML keyboard types into `LdmlKeyboardTypes` (#12713)
* chore(web): rename test files and folders (#12704)
* chore(core): rename test files (#12705)
* chore(linux): rename test files (#12706)

## 18.0.145 alpha 2024-11-26

* docs(windows): update emscripten bash setup (#12700)
* chore(common): Add link to onboarding doc to `CONTRIBUTING.md` (#12697)

## 18.0.144 alpha 2024-11-25

* chore(deps): bump cross-spawn from 7.0.3 to 7.0.6 in /developer/src/server/src/win32/trayicon/addon-src (#12687)
* chore(developer): make package subfile description fully optional (#12665)
* fix(developer): box package compiler info fields (#12666)
* fix(developer): correct whitespace handling in virtual keys and remove partially implemented virtual key series in kmcmplib compiler (#12604)

## 18.0.143 alpha 2024-11-22

* chore(deps): bump cross-spawn from 7.0.3 to 7.0.6 (#12685)

## 18.0.142 alpha 2024-11-20

* chore(common): Update CODEOWNERS (#12680)

## 18.0.141 alpha 2024-11-15

* chore(linux): add support for Ubuntu 25.04 Plucky Puffin (#12675)

## 18.0.140 alpha 2024-11-13

* chore(common): Add 17.0.330 - 17.0.332 to version history (#12663)
* fix(developer): reconnect `--full-test` in kmcmplib build and enable for CI (#12631)
* docs(developer): kmc-generate (#12647)

## 18.0.139 alpha 2024-11-12

* fix(windows): help links updated (#12646)

## 18.0.138 alpha 2024-11-08

* fix(common): check for invalid markers (#12613)
* chore: update minimum versions (#12632)
* fix(windows): correct path to output file in publish step for fv keyboards (#12637)
* chore(core): move API docs from help.keyman.com (#12642)
* feat(developer): kmc generate (#11014)
* feat(developer): kmc-copy (#12555)
* feat(developer): add GitHub and Cloud support to kmc-copy (#12586)

## 18.0.137 alpha 2024-11-07

* fix(windows): correct engine help source path for upload (#12625)
* fix(developer): use 'N' for nomatch store debug strings (regression in #12107) (#12629)
* chore(developer): skip masaram_gondi in kmcmplib full test (#12630)
* feat(developer): analyze osk-char-use merge with existing mapping file (#12622)
* feat(developer): Report key 'address' in validation failures in layout compiler (#12588)

## 18.0.136 alpha 2024-11-06

* fix(developer): handle merge commits when checking git log date (#12627)

## 18.0.135 alpha 2024-11-05

* fix(developer): handle paste of TSV into Wordlist grid (#12594)
* fix(developer): handle missing files in kmc-kmn (#12595)
* fix(developer): handle missing files in kmc-model (#12596)
* refactor(android): move Android Engine help in-repo (#12598)
* refactor(ios): move ios Engine help in-repo (#12599)
* refactor(windows): move windows Engine help in-repo (#12600)
* refactor(web): move web engine help to app repo (#12601)
* docs: Update websites readme with debug info (#12602)
* fix(developer): create Server config directory before options save (#12608)
* chore(linux): add support for Ubuntu 25.04 Plucky Puffin (#12614)
* fix(linux): properly check for missing dependencies (#12615)
* fix(linux): set environment variable for rendering of downloads dialog (#12616)

## 18.0.134 alpha 2024-11-04

* fix(developer): ldml don't allow a uset as right-hand-side variable (#12606)

## 18.0.133 alpha 2024-11-01

* test(developer): kmcmplib compiler unit tests 4 (#12489)

## 18.0.132 alpha 2024-10-30

* feat(windows): kmdevlink app (#12552)

## 18.0.131 alpha 2024-10-25

* chore(android,windows): Update Crowdin strings for Khmer (#12574)
* refactor(developer): add kps-file-reader and kps-file-writer (#12545)
* fix(mac): make modifiers operational in OSK (#12556)
* fix(mac): support missing alt layers in OSK (#12565)

## 18.0.130 alpha 2024-10-24

* fix(android/engine): Increase robustness when checking package kmp.json languages (#12567)
* chore(ios): support xcode 16 build (#12570)

## 18.0.129 alpha 2024-10-21

* chore(mac): support Xcode 16 build (#12554)

## 18.0.128 alpha 2024-10-14

* docs(developer): add refs to Keyman MIME types (#12540)

## 18.0.127 alpha 2024-10-12

* fix(developer): use TextDecoder to convert Uint8Array to string (#12537)

## 18.0.126 alpha 2024-10-11

* test(common): add markdown link check test for product documentation (#12472)
* chore(linux): improve output if `dpkg-gensymbols` fails and run other tests (#12527)

## 18.0.125 alpha 2024-10-10

* chore(common): allow to run `build.sh` scripts in `bashdb` debugger (#12518)
* chore(linux): Improve output of API Verification (#12522)
* chore(linux): allow to skip API change (#12519)
* fix(linux): fix problem with API checks with merge commits (#12520)
* refactor(web): move `KeyboardObject` type to `common/web/types` (#12514)

## 18.0.124 alpha 2024-10-09

* chore(common): fix links in minimum-versions.md (#12507)
* fix(developer): use richedit in debug memo to support Egyptian cartouches (#12464)
* feat(android): Add controls for auto-correct (#12443)

## 18.0.123 alpha 2024-10-08

* chore(developer,common): deps: replace xml2js with fast-xml-parser (#12502)
* chore(ios): renew certificate (#12512)

## 18.0.122 alpha 2024-10-07

* feat(mac): both option keys generate right alt if no left alt mapping (#12458)
* chore(common): improve configuration detection for hextobin (#12481)

## 18.0.121 alpha 2024-10-03

* docs(developer): Fix image links in help (#12488)
* fix(oem/fv): Update keyboard versions and names for fv_all.kmp 13.1 (#12486)
* feat(common): unified XML parser/writer (#12482)

## 18.0.120 alpha 2024-10-02

* chore(common): Add note on troubleshooting website errors (#12487)
* docs(common): mention `KEYMAN_USE_NVM` in minimum versions doc (#12490)
* docs(web): fix paths to several help pages (#12491)
* docs(web): fix structure of test document (#12492)

## 18.0.119 alpha 2024-09-28

* refactor(developer): copy dev 17.0 help into repo (#12427)
* fix(developer): warn before importing over touch layout (#12478)
* chore(linux): display branch name with API verification (#12480)
* docs(core): Update kmx-plus-file-format.md (#12479)

## 18.0.118 alpha 2024-09-26

* chore(developer): add context/options (#11566)
* chore(deps-dev): bump rollup from 4.16.4 to 4.22.4 (#12462)
* fix(developer): ignore excess whitespace in `<row keys>` attribute (#12468)
* refactor(common): move help into common prod/docs/help folders (#12424)
* fix(developer): publish developer-utils package during build (#12471)
* fix(linux): ignore additional C++ symbol in API check (#12474)

## 18.0.117 alpha 2024-09-25

* docs(common): Document how to skip generating CDN on websites (#12446)
* fix(android): Remove toggle for "Always Show Banner" (#12430)
* fix(android): Hide suggestion banner on password fields (#12442)
* fix(developer): prevent invalid string ids (#12465)

## 18.0.116 alpha 2024-09-20

* change(mac): remove verbose logging option (#12431)
* chore(common): Allow to build offline (#12439)

## 18.0.115 alpha 2024-09-19

* chore(common): detect ssh remotes in git hooks (#12437)
* fix(common): add proper configure output for hextobin (#12440)
* fix(core): add missing dependency for core (#12438)
* chore(developer): remove .js output from LDML compiler (#12432)

## 18.0.114 alpha 2024-09-17

* fix(developer): rewrite ldml visual keyboard compiler (#12402)
* fix(developer): check vars string usage before definition (#12404)
* change(mac): remove 'Always show OSK' option (#12355)

## 18.0.113 alpha 2024-09-16

* test(developer): kmcmplib compiler unit tests 3 (#11990)

## 18.0.112 alpha 2024-09-14

* chore(deps): bump express from 4.19.2 to 4.20.0 (#12396)

## 18.0.111 alpha 2024-09-13

* chore(common): Update crowdin strings for Italian (#12408)
* fix(common): correct offsets in KMX+ spec (#12350)
* fix(android): Add gating to setLongpressDelay() (#12410)

## 18.0.110 alpha 2024-09-12

* chore(common): Update to Unicode 16.0 (#12393)
* refactor(web): move `common/web/es-bundling` → `web/src/tools/es-bundling` (#12389)
* refactor(web): move `common/web/eslint` → `common/tools/eslint` (#12390)
* refactor(web): move sentry-manager → `web/src/engine/sentry-manager` (#12397)
* refactor(web): merge `device-detect` with `web/src/engine/main` (#12399)
* chore(web): allow to run unit tests in vscode test explorer (#12400)
* fix(developer): index() requires comma between parameters in kmcmplib compiler (#12328)

## 18.0.109 alpha 2024-09-11

* chore(common): Update history with 17.0.329 stable (#12394)
* refactor(web): move `model/templates` to `web/src/engine/predictive/text` (#12382)
* refactor(web): move `common/models` to `web/src/engine/predictive-text` (#12383)
* refactor(web): move `common/web/utils` to `web/src/engine/common/web-utils/` (#12384)

## 18.0.108 alpha 2024-09-10

* docs(android): Update help docs (#12367)
* fix(developer): fix building with Ubuntu 24.04 (#12379)
* refactor(android): Move build-publish.sh to builder script (#12351)
* fix(android): Separate `publishSentry` Gradle task to publish symbols to Sentry (#12358)
* refactor(web): move `model/types` to `web/types` (#12370)

## 18.0.107 alpha 2024-09-09

* fix(android): Update Text Size menu icons for RTL support (#12290)

## 18.0.106 alpha 2024-09-06

* feat(windows): add right modifier included in hotkey optional functionality (#12259)
* fix(android): Skip language counts for lexical-model packages (#12361)
* docs(web): fix link to documentation page (#12369)

## 18.0.105 alpha 2024-09-05

* chore(common): Fix missing entries in HISTORY.md (#12352)
* docs(android): Add in-app help for adjusting longpress delay (#12359)
* fix(mac): avoid crash on startup with macOS 10.15 (Catalina) (#12354)
* chore(oem/fv): Update to fv_all 13.0 (#12362)
* feat(windows): Add two new strings for SIL Global name instead of SIL International (#12327)

## 18.0.104 alpha 2024-09-03

* fix(mac): display package info after keyboard installation (#12326)

## 18.0.103 alpha 2024-09-02

* feat(windows): Remove hotkey related feature flags (#12252)
* feat(windows): update SIL logo for Windows UI (#12250)

## 18.0.102 alpha 2024-08-30

* docs(web): add documentation comments for touch layout interfaces (#12314)
* change(mac): store data in Library directory instead of Documents (#12106)
* change(mac): store partial path in UserDefaults (#12144)

## 18.0.101 alpha 2024-08-29

* refactor(web): move `gesture-recognizer` → `gesture-processor` (#12194)
* docs(core): fix a typo in the KMX+ doc (#12302)
* chore(web): remove obsolete comment (#12304)
* chore(android,ios): Update FirstVoices keyboards to fv_all.kmp 12.15 (#12300)
* fix(developer): make LDML import path consistent for all bundlings of kmc (#12280)
* fix(core): properly support 'other' modifier state with `uint32_t` type (#12281)
* chore(windows): fix typo in environment.inc.sh (#12286)
* fix(android): Check in material-stepper as internal Maven dependency (#12267)
* docs(core): improve formatting of KMX+ doc (#12303)
* fix(android): Prioritize certain actions over multi-line for ENTER key (#12315)
* fix(linux): add `keymanFacename` to .ldml file (#12277)
* chore(common): Update crowdin strings for Czech (#12316)
* fix(web): prevent unintuitive space-output blocking for mid-context suggestions (#12313)

## 18.0.100 alpha 2024-08-28

* fix(windows): check IM window will be in a visible location (#11967)

## 18.0.99 alpha 2024-08-27

* feat(web): import the generator for the pred-text wordbreaker's Unicode-property data-table (#10690)
* feat(web): optimize the wordbreaker data table for filesize and ease of first-load parsing (#10692)
* fix(web): fixes wordbreaker test import path (#12297)
* feat(web): enable utf8 charset encoding for the build artifacts (#12115)

## 18.0.98 alpha 2024-08-26

* change(ios): defer registration of fonts past initialization (#12190)
* refactor(ios): optimize font registration (#12210)
* test(mac): add unit tests to validate first calls to compliance check (#11724)
* fix(mac): limit short bundle version string to x.y.z format in info.plist (#12233)
* chore(developer): extend timeouts for lm compiler tests to 5 secs (#12273)
* fix(developer): find last matching key in LDML key bag when building KVK (#12278)
* chore(android): Cleanup stray debug statements in console (#12287)
* fix(developer): ensure call() detects invalid store in kmcmplib compiler (#12263)

## 18.0.97 alpha 2024-08-24

* refactor(linux): cleanup API of kvk2ldml.py (#12276)
* chore(common): adjust build settings for windows clean builds (#12264)
* chore(windows): remove remaining unused Makefiles (#12274)
* docs(developer): update build documentation to refer to build.sh (#12272)

## 18.0.96 alpha 2024-08-23

* fix(android): Fix navigation arrows in Info Activity for RTL (#12244)
* fix(web): fix documentation-keyboard spacebar-text scaling (#12232)
* fix(android): Use increment and decrement arrows on longpress delay menu (#12242)
* fix(android): Add RTL assets for adjusting keyboard height menu (#12261)
* chore(common): use `npm install` in emsdk update (#12269)
* docs: refresh windows.md (#12248)

## 18.0.95 alpha 2024-08-22

* chore(common): allow build agents to automatically select emsdk version, and enable support for 3.1.60+ (#12243)

## 18.0.94 alpha 2024-08-21

* fix(core): look for `emcc` instead of `emcc.py` (#12235)
* fix(web): improve tokenization output when wordbreaker breaks spec for span properties in output (#12229)
* chore(android): Use RTL-aware alignment and padding for layouts (#12225)
* fix(web): disable fat-finger data use when mayCorrect = false (#12220)
* fix(android): Auto-mirror back and forward arrows for RTL support (#12227)
* feat(android): Add localization for Arabic (#12228)
* fix(android): Auto-mirror increment and decrement arrows for RTL support (#12230)

## 18.0.93 alpha 2024-08-20

* refactor(web): remove engine/interfaces dependency on engine/js-processor (#12188)
* fix(web): fix malformed reversion display strings (#12201)
* feat(android): Add menu to specify long-press delay (#12170)
* feat(android): Pass longpress delay to KeymanWeb (#12185)
* fix(core): set mac build version for meson cli build to 10.13 (#12223)

## 18.0.92 alpha 2024-08-19

* chore(deps-dev): bump @75lb/deep-merge from 1.1.1 to 1.1.2 (#12118)
* chore(deps): bump semver from 7.5.4 to 7.6.0 (#12119)
* fix(windows): "Keyman" is not localized in UI strings (#12162)
* feat(android): Enhance how ENTER key is handled in apps (#12125)
* refactor(web): move `lm-worker` → `worker-thread` (#12150)
* fix(developer): remove redundant check in LdmlKeyboardCompiler.validate() (#11858)

## 18.0.91 alpha 2024-08-16

* refactor(web): move `predictive-text` → `worker-main` (#12146)
* fix(web): restore flick functionality (#12187)
* refactor(web): move `lm-message-types` → `predictive-text/types` (#12149)
* fix(developer): enforce presence of kps Info.Description field in info compilers (#12204)
* fix(developer): enforce presence of Version field when FollowKeyboardVersion is not set, in package compiler (#12205)

## 18.0.90 alpha 2024-08-15

* refactor(web): move parts of `keyboard-processor` → `js-processor` (#12111)
* fix(web): allow `lm-worker` to build on Linux (#12181)
* refactor(web): move remaining parts of `keyboard-processor` → `keyboard` (#12131)
* docs: update .kmx documentation around bitmaps, modifier state (#12183)
* refactor(web): rename `package-cache` → `keyboard-storage` (#12135)

## 18.0.89 alpha 2024-08-14

* feat(web): test skipped prediction round handling (#12169)
* fix(web): support live configuration of longpress delay (#12175)
* feat(web): add osk.gestureParams for better gesture-config persistence (#12176)
* refactor(core): move utfcodec to common (#12171)
* refactor: move kmx_u16 to common and rename to km_u16 (#12177)
* refactor(developer): use npm or local source for server addons instead of github references (#12090)
* fix(developer): fix crash with Windows Clipboard by ignoring zero scan code in debugger (#12166)
* chore(developer): update SIL logo (#12168)

## 18.0.88 alpha 2024-08-13

* docs: add .kmx specification (#12163)

## 18.0.87 alpha 2024-08-12

* chore(web): drop flaky auto-test component (#12155)
* fix(common): show description even if child projects specifies path (#12145)

## 18.0.86 alpha 2024-08-09

* chore(android): Update splash screen with SIL Tai Heritage Pro logo (#12127)
* chore(common): Update localization for Greek Polytonic (#12112)
* chore(common): add support for build target platform exclusions (#12113)
* feat(common): add top-level build.sh (#12114)
* chore(developer): start building Developer on linux,mac (#12117)
* refactor(web): Move `common/web/recorder` → `web/src/tools/testing/recorder-core` (#12092)

## 18.0.85 alpha 2024-08-08

* fix(developer): update date for last git commit date fixture (#12122)
* refactor(android/engine): Parse keyboards.json for FirstVoices app (#11943)

## 18.0.84 alpha 2024-08-07

* refactor(common): move kpj-related files into developer-utils (#11531)
* refactor(common): move kps-file.ts to @keymanapp/developer-utils (#11763)
* refactor(common): move kvks-file to @keymanapp/developer-utils (#11764)
* refactor(common): move .keyman-touch-layout reader/writer to @keymanapp/developer-utils (#11765)
* refactor(common): move LDML keyboard .xml reader/writer and kmx-plus builder to @keymanapp/developer-utils (#12081)
* refactor(common): move compiler-interfaces to @keymanapp/developer-utils (#12088)
* refactor(common): move xml2js and related deps to @keymanapp/developer-utils (#12101)

## 18.0.83 alpha 2024-08-06

* chore(common): Update history from 17.0.328 (#12093)
* change(common/models): change model tokenization to also tokenize whitespace (#11975)
* feat(web): support transform tokenization when given a root context (#11998)
* change(web): track whitespace-aware tokenization for context + correction-search caching (#11979)
* change(web): leverage tokenization to preserve punctuation and whitespace when predicting (#11997)
* fix(web): patch up worker build to provide artifacts for its tests (#12082)
* chore(web): move `web/src/engine/paths/` → `web/src/engine/interfaces/` (#12064)
* chore(web): move `common/web/input-processor/` → `web/src/engine/main/` (#12066)
* refactor(web): refactor and harmonize constants (#12072)
* chore(common): add data versions to minimum-versions.inc.sh (#12103)
* fix(web): fix lm-worker test broken by botched merge conflict resolution (#12104)

## 18.0.82 alpha 2024-08-05

* chore(windows): remove QIT_VSHIFTDOWN QIT _VSHIFTUP (#11973)
* chore(developer): add language/reference (#11799)

## 18.0.81 alpha 2024-08-03

* chore(developer): Revert "chore(developer): remove redundant references from tsconfig.json" (#12076)
* chore(common): use nvm in builds to select a node version automatically (#12069)
* refactor(developer): move functions and variables into CompilerErrors.cpp (#12030)
* refactor(developer): improve the callbacks for kmcmplib (#12031)
* refactor(developer): align kmcmplib error codes and names with kmc-kmn (#12044)
* refactor(developer): move error reporting to error site in kmcmplib (part 1) (#12045)
* refactor(developer): move error reporting inside `GetCompileTargetsFromTargetsStore` and `ProcessGroupFinish` (#12048)
* refactor(developer): move kmcmplib message construction to kmc-kmn (#12059)
* refactor(developer): rename compiler error reporting functions in kmcmplib (#12060)
* chore(developer): support filename field in kmcmplib compiler messages (#12061)
* refactor(developer): replace `VERIFY_KEYBOARD_VERSION()` calls with `VerifyKeyboardVersion()` (#12063)

## 18.0.80 alpha 2024-07-31

* chore(developer): remove redundant references from tsconfig.json (#12037)
* fix(web): add nullish test in setOsk (#12039)
* fix(web): unrevert #11258, leaving OSK hidden before instructed to display (#12049)
* test(developer): check correct use of u16chr when second parameter could be null (#11894)
* change(web): remove support for es5 (#11881)

## 18.0.79 alpha 2024-07-30

* change(mac): add custom tags in sentry to better identify errors (#11947)
* feat(web): add unit tests for case-detection & handling (#11950)
* refactor(web): spin off method for correction-search probability-thresholding check (#11952)

## 18.0.78 alpha 2024-07-29

* chore(common): Update history from 17.0.327 and add missing descriptions (#12021)
* change(web): revert #11174, which loads keyboards before initializing the OSK (#12015)
* feat(web): add unit testing for finalization of generated suggestions (#11946)
* feat(web): add unit tests for prediction lookup component (#11949)

## 18.0.77 alpha 2024-07-27

* refactor(windows): clean up logging (#11921)
* chore(developer): rename to analyzer-messages.ts (#12017)
* fix(developer): remove `paths` from tsconfig.json (#12028)
* chore(developer): api doc refresh (#12029)

## 18.0.76 alpha 2024-07-26

* change(linux): improve changelog PRs after upload to debian (#12024)
* test(developer): kmcmplib compiler unit tests 2 (#11663)
* fix(linux): set local directory if not specified (#12032)

## 18.0.75 alpha 2024-07-25

* chore(windows): remove the posting WM_KEYUP/DOWN events to IM (#12002)
* feat(web): check for low-probability exact + exact-key correction matches (#11876)
* refactor(web): extract suggestion-finalization block into its own function (#11899)
* chore(developer): remove `CompilerMessages` stub and use `KmnCompilerMessages` (#11986)
* chore(developer): rename kmc-ldml `CompilerMessages`, `LdmlKeyboardCompilerMessages` to `LdmlCompilerMessages` (#11988)
* chore(developer): rename kmc-package `CompilerMessages` to `PackageCompilerMessages` (#11989)
* fix(developer): handle errors parsing .kps file when loading project (#12008)
* chore(common): updated stats script to support end date/sprint (#12009)
* fix(windows): align engine.sln platforms and configurations (#12011)
* refactor(web): spin off deduplication, suggestion-similarity sections (#11900)
* refactor(web): extract the correct-and-raw-predict blocks into their own method (#11888)
* refactor(web): convert internal prediction methods to stateless format (#11940)
* feat(web): add unit tests for predict auto-selection method (#11941)
* feat(web): extend unit-test oriented dummy model (#11948)
* feat(web): add unit tests for suggestion-similarity detection (#11944)
* feat(web): add unit testing for suggestion deduplication (#11945)
* feat(developer): add hint when index() store is longer than any() store (#12000)
* chore(android,ios): Add ojibwa ifinal/rdot keyboards to FirstVoices (#11889)

## 18.0.74 alpha 2024-07-24

* fix(developer): correct handling of trailing spaces by GetDelimitedString() in kmcmplib compiler (#11938)
* chore(linux): remove Ubuntu Mantic, add Oracular (#12003)

## 18.0.73 alpha 2024-07-23

* fix(windows): add text selected bool emit backspace key when text selected in TSF (#11884)
* fix(developer): prevent buffer overrun in `u16tok` (#11910)
* fix(developer): prevent invalid values in targets store (#11918)
* feat(developer): automatically detect version for `U_xxxx_yyyy` ids (#11957)
* feat(developer): handle automatic versioning of chiral modifiers (#11965)
* test(developer): Add tests for automatic versioning of notany() with context() (#11980)
* feat(developer): handle automatic versioning of special key caps on normal keys (#11981)
* feat(developer): automatically upgrade version when gestures are found in the touch layout (#11982)
* refactor(developer): rename `verifyMinimumKeymanVersion` (#11983)
* feat(developer): add searching for message identifiers to `kmc message` (#11984)
* fix(developer): kmc-keyboard-info: use default version 1.0 if version information missing (#11985)
* fix(web): present a "keep" option when a context-altering suggestion is auto-selected (#11969)
* fix(web): prevents auto-accept immediately after reversion (#11970)

## 18.0.72 alpha 2024-07-22

* fix(web): remedy unit-test stability issues (#11933)
* refactor(web): fix TypeScript errors and warnings (#11911)

## 18.0.71 alpha 2024-07-18

* chore(windows): add comments for _WIN64 tests (#11929)
* chore(common): Update Crowdin strings for Portuguese (#11974)

## 18.0.70 alpha 2024-07-08

* feat(web): provide lexicon probabilities directly on the search path (#11868)
* feat(common/models): support direct-child access for Trie node iteration (#11869)
* change(common/models/templates): rework Trie predict method to utilize traversals (#11870)
* change(web): track the base correction for generated predictions (#11875)
* feat(web): add and enable auto-correction (#11866)

## 18.0.69 alpha 2024-07-05

* fix(core): allow to successfully build on Ubuntu 24.04 (#11926)
* chore(windows): correct output file for 64-bit build of keyman32 in build.sh (#11930)
* chore(android,ios): Add Crowdin localization for Polytonic Greek (#11877)

## 18.0.68 alpha 2024-07-04

* refactor(windows): merge keyman64 build into keyman32 (#11906)
* refactor(windows): remove wm_keyman_keydown and wm_keyman_keyup (#11920)

## 18.0.67 alpha 2024-07-03

* refactor(common/models): move TS priority-queue implementation to web-utils (#11867)

## 18.0.66 alpha 2024-07-02

* fix(developer): handle second parameter of index correctly in kmcmplib compiler (#11815)

## 18.0.65 alpha 2024-07-01

* fix(developer): prevent non-BMP characters in key part of rule (#11806)
* chore(linux): remove unused building with pbuilder (#11862)

## 18.0.64 alpha 2024-06-28

* fix(web): use fat-finger data with simple keypresses (#11854)

## 18.0.63 alpha 2024-06-26

* feat(linux): implement Linux side of SimulateAltGr option (#11852)

## 18.0.62 alpha 2024-06-25

* chore(common): update C/C++ formatting options (#11836)
* chore(linux): use shared meson config (#11863)
* fix(linux): ignore exceptions trying to install cache (#11861)

## 18.0.61 alpha 2024-06-24

* feat(web): optimization via lazy preprocessing of keyboard touch-layout info (#11265)
* fix(android): clear globe highlight when displaying keyboard picker (#11826)
* refactor(linux): add KeymanOption class for options (#11850)
* refactor(linux): rename methods that deal with keyboard options (#11851)

## 18.0.60 alpha 2024-06-21

* chore(web): define common timeout variable for automated testing (#11839)
* feat(developer): warn on empty keycaps (#11810)
* change(web): optimization for keyboard-layout preprocessing (#11263)
* feat(web): optimization via lazy construction of OSK layers (#11264)
* fix(developer): layr: fix modifier err message on layer w/o id (#11843)

## 18.0.59 alpha 2024-06-19

* chore(deps-dev): bump braces from 3.0.2 to 3.0.3 (#11756)
* chore(developer): clarify project upgrade messages about file locations (#11819)
* chore(deps): bump ws from 8.16.0 to 8.17.1 (#11822)
* chore(common): update base-package node-engine setting (#11798)
* change(web): change after-word whitespace check to be more selective (#11800)
* chore: Revert "chore(common): update base-package node-engine setting" (#11829)
* change(web): drop correction batching (#11768)
* chore(web): move correction-search execution timer to its own file (#11757)
* refactor(web): overhaul predictive-text engine's timer to better detect paused time (#11758)
* feat(web): improve predictive-text responsiveness when typing rapidly (#11784)
* feat(linux): re-create missing files at run-time (#11789)

## 18.0.58 alpha 2024-06-18

* test(core): Add a minimal test that exercises the core API (#11781)
* fix(developer): check HISTORY.md to get last modified date for keyboard_info and model_info (#11805)
* docs(developer): extra context help for keyboard-editor (does not exist in Keyman Developer 17.0) (#11771)

## 18.0.57 alpha 2024-06-17

* fix(web): fix id of longpress keys with modifier set in touch layout (#11783)
* fix(web): prevent desktop OSK crash when addKeyboards is called before engine init (#11786)
* fix(core): serialize tests for core/wasm on mac agents (#11795)
* fix(developer): refactor kmcmplib compiler messages to use map (#11738)
* fix(developer): make native compilation of kmcmplib under Linux possible (#11779)
* feat(core): devolve regex to javascript (#11777)
* feat(core): remove ICU from core under wasm (#11778)
* fix(linux): restart ibus after manual integration test run (#11775)

## 18.0.56 alpha 2024-06-14

* feat(core): devolve normalization to js (#11541)
* fix(developer): show message if no more platforms to add to touch layout editor (#11759)
* docs(developer): context help in package-editor and put the existing context help in their own tab comments (#11760)
* docs(developer): context help in keyboard-editor section (#11754)
* docs(developer): context help in new-project section (#11767)
* docs(developer): context help for new-project-parameters in keyman developer (#11769)
* docs(developer): context help for Select BCP 47 tag in Keyman Developer (#11770)
* change(common): update esbuild to 0.18.9 (#11693)
* change(web): more prep for better async prediction handling (#10347)
* fix(web): set new-context rules' device to match that of the active OSK (#11743)
* chore(linux): Update debian changelog (#11671)
* fix(web): add limited Array.from polyfill for lm-worker use (#11732)

## 18.0.55 alpha 2024-06-13

* fix(developer): handle missing OSK when importing a Windows keyboard into a touch-only project (#11720)
* fix(developer): verify email addresses in .kps and .keyboard_info (#11735)
* change(web): prep for better asynchronous prediction handling (#10343)

## 18.0.54 alpha 2024-06-12

* fix(common): remove subpackage entries for older TS version (#11745)
* chore(common): end use of ts-node (#11746)
* feat(web): add bulk_render variant that loads and renders keyboards from local KMP (#10432)

## 18.0.53 alpha 2024-06-10

* fix(android): check current orientation when redisplaying system keyboard (#11604)
* fix(android): fix keyboard size after rotation and restore via onSizeChanged, after layout (#11722)
* fix(developer): fix kmcmplib unit-test include paths (#11749)

## 18.0.52 alpha 2024-06-08

* fix(developer): prevent two touch layout editors opening for the same file (#11717)
* chore(common): cleanup meson deprecations and warnings (#11523)
* feat(developer): support language reference in context help (#11737)
* test(developer): kmcmplib compiler unit tests (#11378)

## 18.0.51 alpha 2024-06-07

* fix(web): fix osk touch-focus tracking (#11705)
* fix(web): defer keyboard activation requests made during engine initialization (#11713)
* chore(developer): add context/character-map (#11656)
* chore(developer): add context/wordlist-editor (#11658)
* chore(developer): add context/new-model-project-parameters (#11677)
* fix(common): remove allowJs from web's tsconfig.base.json (#11718)
* change(web): precompile all TS-based tests (#11723)
* chore(developer): add extra logging for assertion failure when pressing backspace in debugger (#11707)
* chore: add cherry-pick information in commit messages (#11708)
* fix(developer): handle encoding errors when loading wordlists (#11711)
* chore(ios): remove dead Swift-side keyboard gesture code (#11672)
* fix(mac): change build configuration to prevent cycle error in Xcode 15 (#11730)
* refactor(web): Replace deprecated substr with substring (#11637)

## 18.0.50 alpha 2024-06-06

* chore(common): adds retry mechanism for build script npm ci calls (#11451)
* fix(web): get row-height for flick constraints after performing layout (#11691)
* chore(ios): enable webview debugging (#11229)
* fix(android): handle `IllegalArgumentException` when initializing `CloudDownloadMgr`, add logging to check for unhandled side-effects (#11626)
* chore: replace git dep on restructure with 3.0.1 in npm (#11657)
* chore: move xml2js into the repo to eliminate npm git dependency (#11660)
* chore(common): move CLDR import copy into build step for common/web/types (#11690)
* fix(developer): handle editor initializing after debugger when setting execution point (#11587)
* fix(developer): treat js files with unrecognized encodings as non-keyboard files (#11698)
* fix(developer): disable example edit controls if no examples in Package Editor (#11701)

## 18.0.49 alpha 2024-06-05

* fix(web): revert #11598 to eliminate use of `finalInput` which caused crash after moving caret (#11685)
* fix(common): correctly display result of multiple option parameters in builder (#11679)
* fix(linux): specify path with package name (#11694)
* fix(linux): remove debug output and re-enable failures (#11695)

## 18.0.48 alpha 2024-06-04

* fix(linux): add debug output (#11668)
* chore(windows): add debug log messages for modifier key press processing (#11665)
* test(core): clarify output of test_unicode (#11572)
* fix(linux): add more debug output (#11680)
* fix(linux): try a different way (#11681)

## 18.0.47 alpha 2024-06-03

* fix(developer): save touch layout editor selection when loading state (#11575)
* chore: Update history from 17.0.326 (#11649)
* refactor(web): refactor keyboard-layout preprocessing property transplantation (#11601)
* fix(web): fix keyboard-processing bugs uncovered by stricter TS settings (#11603)
* fix(web): Don't apply suggestion unless fully configured (#11594)
* chore(web): clean up remaining low-level Web .tsconfig settings (#11424)
* chore(web): correct cases of implicit-any in web/ (#11462)
* chore(web): enforce strict function types in web/ (#11463)
* chore(web): finalize web/ project parity with base-repo TS config settings (aside from JS version target) (#11464)
* fix(android): include DOMRect polyfill for older ES6-supporting devices (#11653)
* fix(linux): move environment to separate job and checkout to separate subdirectory (#11659)
* fix(linux): paths in `uses:` can't use variables (#11661)
* fix(linux): restore of artifacts still needs to be in same job (#11662)
* fix(linux): include api version as part of package filename (#11664)
* fix(linux): don't escape wildcard for package filename (#11666)
* fix(linux): match digit in package filename (#11667)

## 18.0.46 alpha 2024-06-01

* chore(linux): More debugging of API verification (#11639)

## 18.0.45 alpha 2024-05-31

* change(mac): adopt unified logging APIs (#11515)
* chore(common): update labels to include change, style (#11527)
* chore(linux): run deb-package sourcepackage job on Ubuntu 24.04 (#11565)
* test(core): verify Unicode and ICU versions cross-platform (#11418)
* chore(linux): Debugging API verification (#11536)
* refactor(core): prepare normalization code to call into JS under WASM (#11519)
* feat(core): change normalize_nfd() to use JS native call instead of ICU on wasm (#11520)
* fix(linux): Restore artifacts for API verification in the correct job (#11571)
* fix(linux): Don't clean workdir after checkout in API verification (#11592)
* fix(developer): test existence of ngrok.exe before atttempting to start (#11543)
* fix(developer): handle unsupported `return` statement in `match` and `nomatch` in web compiler (#11546)
* fix(developer): handle invalid project file when scanning for owner project (#11558)
* fix(developer): ensure folder for MRU list is always created (#11552)
* fix(web): fixes error on tab, enter when only a single input element is on the page (#11470)
* fix(web): correctly handle cross-origin stylesheets when calculating keyboard size and key cap font size (#11472)
* fix(web): pre-init keyboard activation awaits init before proceeding (#11505)
* fix(android): host-page banner initialization (#11508)
* fix(web): add null guard for missing debug info when reporting to Sentry (#11579)
* chore(web): updates TypeScript version to 5.4.5 (#11414)
* fix(ios): remove incorrect cast in log statement to prevent crash (#11569)
* fix(web): headless recorder unhandled paths, invalid test-sequence references (#11596)
* fix(web): remove dead code-branches (#11597)
* chore(web): fixes implicit-this cases (#11459)
* fix(web): use intended source for prediction-to-context matching (#11598)
* fix(web): explicitly terminate banner gesture-handling when banner is swapped (#11599)
* chore(web): removes unused locals, imports, and private fields (#11460)
* fix(web): use correct parameter name in button UI OSK `hide` event (#11600)
* chore(mac): rework of main build script (#11444)
* fix(ios): do not write to shared storage from system keyboard (#11613)
* fix(developer): handle invalid default project path in options (#11555)
* fix(developer): handle missing data in .kps `<Keyboard>` (#11563)
* fix(developer): use correct variable when fixing up flick and multitap in Layer Properties dialog (#11577)
* fix(developer): correct the inference of default hint from flicks (#11582)
* fix(developer): make top row of Character Map 1 pixel to avoid DIV/0 in gestures (#11590)
* chore: increase kmc-kmn build test timeout (#11608)
* chore(windows): support port in THTTPUploader (#11611)
* change(common): add extra commit message hints (#11585)
* chore(developer): add context/debug (#11629)

## 18.0.44 alpha 2024-05-24

* fix(linux): Pass artifacts key to API verification GHA (#11517)
* fix(web): properly pass test-runner verbose-logging flag (#11509)
* chore(developer): add context/key-test (#11512)
* fix(android/engine): Ignore updating invalid selections (#11510)
* docs(common): update min emscripten version in docs to 3.1.46 (#11530)
* fix(linux): Properly use and pass run_id (#11533)

## 18.0.43 alpha 2024-05-24

* fix(android): Revert Sentry version for FV Android (#11522)
* chore(windows): remove schedule task clean up introduced in 15.0 Alpha (#11521)
* fix(windows): add FirstVoices Keyboards build to Windows release build (#11524)

## 18.0.42 alpha 2024-05-23

* chore(web): conversion of Web's browser-integration auto-tests for @web/test-runner use (#11455)
* chore(web): conversion of Web's end-to-end style auto-tests for @web/test-runner use (#11456)
* chore(web): finalization for replacement of Karma with Web Test Runner (#11300)
* fix(android): Delete sentry-cli-2.31.1.exe during CI builds (#11493)
* change(common): remove `is:open` from issue templates (#11492)
* chore(developer): build.sh for developer (#11421)
* chore(windows): move from Makefile to build.sh (#11461)
* fix(linux): Add missing variable to API verification GHA (#11511)
* chore(common): Update standards data (#11003)

## 18.0.41 alpha 2024-05-22

* fix(developer): handle `KM_CORE_IT_INVALIDATE_CONTEXT` in debugger (#11488)
* chore(linux): Trigger GHA packaging for stable builds (#11495)
* chore(android,mac,windows): Update crowdin strings for DE (#11497)
* feat(web): custom infrastructure for @web/test-runner use (#11403)
* chore(web): conversion of lm-worker browser-test for @web/test-runner use (#11404)
* chore(web): conversion of lm-layer browser-tests for @web/test-runner use (#11452)
* chore(web): conversion of Web keyboard-loading browser-tests for @web/test-runner use (#11453)
* chore(web): conversion of Web gesture-engine browser-tests for @web/test-runner use (#11454)

## 18.0.40 alpha 2024-05-21

* refactor(common): move `enabled` declaration into test_color.cpp (#11483)
* test(developer) kmn compiler messages unit tests (#11284)
* chore(linux): Move API verification to separate action (#10637)

## 18.0.39 alpha 2024-05-20

* chore(mac): change indentation of source code to equal standard two spaces (#11432)
* fix(common): calculate already-build dependency targets correctly (#11477)
* fix(common): properly quote `builder_run_action` call (#11478)
* fix(android/app): Verify extracted text is not null (#11481)

## 18.0.38 alpha 2024-05-17

* chore(common): Auto-generate minimum-versions.md from min-ver.sh (#11399)
* fix(android): Replace deprecated APIs for Display, Size, Metrics (#11436)
* chore(common): Clear whatsnew help files (#11469)

## 18.0.37 alpha 2024-05-16

* chore(android): update Gradle wrapper version (#11437)
* fix(android/engine): Handle globe key on lock screen (#11458)
* chore(oem/fv/ios): Remove build_keyboards.sh script (#11438)

## 18.0.36 alpha 2024-05-15

* chore: merge beta into master, final (#11445)
* chore(web): updates chai, @types/chai dependencies (#11318)
* chore(web): updates most @types/node dependencies, makes them consistent (#11319)
* chore(web): adds '@web/test-runner' and related packages (#11323)
* chore(web): EventEmitter3, sinon dependency updates (#11402)

## 18.0.35 alpha 2024-05-14

* chore(core): update core to C++17 (#11340)

## 18.0.34 alpha 2024-05-13

* feat(windows): Don't install desktop shortcut for Keyman for windows on installation (#11401)
* feat(common): improve builder parameter passing for child and dep builds (#11410)
* chore: merge 17.0 beta into master A17S2 (#11431)
* chore(android): Update dependencies (#11393)

## 18.0.33 alpha 2024-05-10

* chore(common): Merge beta to master for Sprint A18S1 (part 2) (#11413)

## 18.0.32 alpha 2024-05-09

* chore(common): Add `minimum-versions.inc.sh` (#11380)
* chore(core): km_core_cp -> km_core_cu (#11341)

## 18.0.31 alpha 2024-05-08

* fix(windows): "Keyboard" should be lower case in UI string for font helper tool (#11392)

## 18.0.30 alpha 2024-05-07

* chore(web): Improve dependencies (#11377)
* test(developer) keyboard info compiler unit tests 3 (#11255)

## 18.0.29 alpha 2024-05-06

* chore(web): Improve web/test.sh script (#11355)
* chore(linux): Fix typo (#11356)

## 18.0.28 alpha 2024-05-04

* chore(common): maintenance on build scripts - cd (#11329)

## 18.0.27 alpha 2024-05-03

* chore(linux): Adjust distros for GHA (#11333)
* fix(common): Fix colorization of help output of builder script (#11336)

## 18.0.26 alpha 2024-05-02

* chore: merge beta into master A18S1 (#11332)

## 18.0.25 alpha 2024-04-30

* fix(linux): Improve detection of Gnome environment (#11292)

## 18.0.24 alpha 2024-04-29

* fix(common): Retry curl downloads up to 5 times (#11314)

## 18.0.23 alpha 2024-04-26

* chore(common): Merge beta to master for Sprint B17S6 (part 2) (#11305)
* fix(web): Fix layout for subkey menus with unusually-sized keys (#11266)

## 18.0.22 alpha 2024-04-25

* chore(linux): Install python3-dev (#11296)

## 18.0.21 alpha 2024-04-24

* chore: merge beta into master B17S6 (#11291)

## 18.0.20 alpha 2024-04-19

* chore(deps): bump tar from 6.1.13 to 6.2.1 (#11211)

## 18.0.19 alpha 2024-04-15

* chore(linux): Fix wrong merge (#11224)

## 18.0.18 alpha 2024-04-12

* chore(common): Merge beta to master for Sprint B17S5 (#11217)

## 18.0.17 alpha 2024-04-05

* chore(linux): Show failed job for next Ubuntu version as failed (#11168)

## 18.0.16 alpha 2024-04-04

* chore(linux): Revert "Ignore failed package builds differently" (#11157)
* chore(linux): Sign packages even if build for next Ubuntu fails (#11163)

## 18.0.15 alpha 2024-04-03

* chore(linux): Build packages for next Ubuntu version separately (#11145)
* chore(linux): Fix typo in path of `build-binary-packages` action (#11154)
* chore(linux): More fixes for workaround for failing linux builds (#11156)

## 18.0.14 alpha 2024-04-02

* chore(linux): Update debian changelog (#11097)

## 18.0.13 alpha 2024-03-29

* chore(deps): bump express from 4.17.3 to 4.19.2 (#11103)

## 18.0.12 alpha 2024-03-28

* chore(common): Merge beta to master for Sprint B17S4 (#11105)

## 18.0.11 alpha 2024-03-20

* chore(deps-dev): bump follow-redirects from 1.15.4 to 1.15.6 (#11010)

## 18.0.10 alpha 2024-03-16

* chore: Merge beta to master for Sprint B17S3 (#11008)

## 18.0.9 alpha 2024-03-05

* chore: B17S2 merge beta to master (#10909)

## 18.0.8 alpha 2024-03-04

* chore(linux): Update debian changelog (#10898)

## 18.0.7 alpha 2024-02-29

* chore: Merge 17.0.270 beta back to master (#10886)

## 18.0.6 alpha 2024-02-27

* chore(linux): Temporarily disable autopkgtests gha (#10853)
* chore(linux): Update debian changelog (#10827)

## 18.0.5 alpha 2024-02-26

* fix(linux): Dynamically get package name (#10826)
* chore(linux): Add running autopkgtests on GHA (#10823)
* fix(linux): Fix autopkgtest gha (#10849)

## 18.0.4 alpha 2024-02-23

* chore(linux): Update debian changelog (#10786)

## 18.0.3 alpha 2024-02-21

* chore(deps): bump ip from 2.0.0 to 2.0.1 (#10792)

## 18.0.2 alpha 2024-02-20

* chore(linux): Add testbuild info to workflow title (#10771)

## 18.0.1 alpha 2024-02-15

* chore(common): move to 18.0 alpha (#10713)
* chore: move to 18.0 alpha

## 17.0.335 alpha 2025-02-06

* fix(android): improve resource-update tool handling of host Activity's closure (#13057)
* fix(ios): prevent message-handler collision (#13058)
* chore(linux): improve compatibility with Gentoo Linux (#12889)
* chore(linux): Update debian changelog (#13062)
* fix(web): keyboard query error states should always provide informative Error objects, reports (#13079)
* chore: push the branch when creating PR in version history writer (#13085)
* fix(web): disabled correction state should prevent predictive corrections (#13074)
* change(ios): set host-app bundle identifier in embedded Web debug-report (#13112)
* chore(mac, ios): build and run with xcode 16 (#13121)

## 17.0.334 stable 2025-01-27

* chore(linux): update copyright year (#12919)
* chore(linux): Update debian changelog (#12916)
* fix(android): Remove access to MEDIA storage permissions (#13005)
* fix(android): restores DOMRect polyfill for old Chrome support (#13014)
* fix(web): add null guard for focusing next element via keystroke (#13035)
* fix: use correct path for resources/build/version (#13030)
* fix(web): fix handling of fonts with single quotes in filename (#13041)
* fix(android): disable early keyboard-loading when not feasible (#13039)
* fix(web): do not simplify nul-prefixed contexts when rule-matching (#13040)
* feat(android): add breadcrumbing during package install about package file, source (#13038)
* fix(web): prevent multitap warning that occurs multitapping longpressable modifier keys (#12974)

## 17.0.333 stable 2025-01-16

* fix(core): permanently disable logging (#12674)
* fix(linux): pushing of updated changelog branch (#12819)
* fix(linux): work around Lintian errors (#12817)
* fix(core): implement ldml_processor::get_key_list() (#12816)
* chore(linux): Update debian changelog (#12022)
* fix(android): use main looper to dispatch key events when OSK is hidden (#12875)
* chore: use GitHub PR titles when writing HISTORY.md (#12908)
* fix(developer): filter incorrect fonts out of .keyboard_info (#12913)

## 17.0.332 stable 2024-11-06

* fix(developer): create Server config directory before options save (#12609)
* fix(developer): handle merge commits when checking git log date (#12628)
* fix(linux): set environment variable for rendering of downloads dialog (#12617)

## 17.0.331 stable 2024-10-30

* fix(android): Hide suggestion banner on password fields (#12466)
* fix(common): declare dep on @keymanapp/ldml-keyboard-constants (#12475)
* fix(oem/fv): Update keyboard versions and names for fv_all.kmp (#12504)
* chore(ios): renew certificate (#12513)
* fix(developer): prevent invalid string ids (#12524)
* fix(developer): ignore excess whitespace in `<row keys>` attribute (#12523)

## 17.0.330 stable 2024-09-16

* refactor(android): Move Sentry and APK to publish task (#12392)
* fix(developer): rewrite ldml visual keyboard compiler (#12406)
* fix(developer): check vars string usage before definition (#12407)

## 17.0.329 stable 2024-09-09

* chore(android,ios): Add ojibwa ifinal/rdot keyboards to FirstVoices (#12020)
* change(web): revert #11174, which loads keyboards before initializing the OSK (#12040)
* fix(web): unrevert #11258, leaving OSK hidden before instructed to display (#12058)
* chore(common): use `nvm` to select version of node for builds (#12074)
* fix(developer): ignore scan code if zero in debugger (#12182)
* fix(developer): enforce presence of Version field when FollowKeyboardVersion is not set, in package compiler (#12206)
* fix(developer): enforce presence of kps Info.Description field in info compilers (#12207)
* fix(web): disable fat-finger data use when mayCorrect = false (#12226)
* chore(common): allow build agents to automatically select emsdk version, and enable support for 3.1.60+ (#12245)
* fix(web): fix documentation-keyboard spacebar-text scaling (#12240)
* fix(core): set mac build version for meson cli build to 10.13 (#12246)
* change(ios): defer registration of fonts past initialization (#12241)
* chore(android,ios): Update FirstVoices keyboards to 12.15 (#12301)
* fix(core): properly support 'other' modifier state with `uint32_t` type (#12285)
* fix(developer): find last matching key in LDML key bag when building KVK (#12284)
* fix(android): check in material-stepper as internal Maven dependency (#12324)
* fix(linux): add `keymanFacename` to .ldml file (#12283)
* chore(oem/fv): Update to fv_all 13.0 (#12363)
* fix(mac): avoid crash on startup with macOS 10.15 (Catalina) (#12364)
* fix(android): skip language counts for lexical-model packages (#12368)

## 17.0.328 stable 2024-07-27

* fix(web): add nullish test in setOsk (#12041)

## 17.0.327 stable 2024-07-25

* fix(android): include DOMRect polyfill for older ES6-supporting devices (#11654)
* fix(web): Don't apply suggestion unless fully configured (#11636)
* fix(mac): handle command keys without crashing (#11675)
* fix(web): get row-height for flick constraints after performing layout (#11692)
* fix(android): handle IllegalArgumentException when initializing CloudDownloadMgr, add logging to check for unhandled side-effects (#11628)
* fix(developer): handle editor initializing after debugger when setting execution point (#11588)
* fix(developer): treat js files with unrecognized encodings as non-keyboard files (#11699)
* fix(developer): disable example edit controls if no examples in Package Editor (#11703)
* chore(developer): add extra logging for assertion failure when pressing backspace in debugger (#11709)
* fix(developer): handle encoding errors when loading wordlists (#11712)
* fix(mac): change build configuration to prevent cycle error in Xcode 15 (#11731)
* fix(developer): handle missing OSK when importing a Windows keyboard into a touch-only project (#11721)
* fix(developer): prevent two touch layout editors opening for the same file (#11727)
* fix(android): check current orientation, fix keyboard size after system keyboard rotations and resumes (#11747)
* chore(linux): Update debian changelog (#11670)
* feat(developer): support language reference in context help (#11741)
* fix(developer): show message if no more platforms to add to touch layout editor (#11766)
* fix(web): add limited Array.from polyfill for lm-worker use (#11733)
* fix(web): set new-context rules' device to match that of the active OSK (#11744)
* fix(web): prevent desktop OSK crash when addKeyboards is called before engine init (#11787)
* fix(windows): add -k parameter for keyboards build.sh (#11811)
* fix(core): serialize tests for core/wasm on mac agents (#11809)
* chore(developer): clarify project upgrade messages about file locations (#11820)
* fix(developer): check HISTORY.md to get last modified date for keyboard_info and model_info (#11808)
* fix(web): fix id of longpress keys with modifier set in touch layout (#11797)
* change(web): change after-word whitespace check to be more selective (#11824)
* fix(android): clear globe highlight when displaying keyboard picker (#11827)
* fix(web): use fat-finger data with simple keypresses (#11871)
* fix(developer): prevent non-BMP characters in key part of rule (#11807)
* fix(linux): ignore exceptions trying to install cache (#11885)
* chore(common): Update Crowdin strings for Portuguese (#11976)
* chore(linux): remove Ubuntu 23.10 Mantic (#12004)

## 17.0.326 stable 2024-06-02

* cherrypick(android/engine): Handle globe key on lock screen (#11468)
* fix(android/app): Verify extracted text is not null (#11482)
* chore(linux): Trigger GHA packaging for stable builds (#11494)
* chore(android,mac,windows): Update crowdin strings for DE (#11498)
* fix(developer): handle `KM_CORE_IT_INVALIDATE_CONTEXT` in debugger (#11489)
* fix(developer): test existence of ngrok.exe before atttempting to start (#11544)
* fix(developer): handle unsupported `return` statement in `match` and `nomatch` in web compiler (#11547)
* fix(developer): ensure folder for MRU list is always created (#11553)
* fix(developer): handle invalid project file when scanning for owner project (#11559)
* fix(ios): remove incorrect cast in log statement to prevent crash (#11570)
* fix(ios): do not write to shared storage from system keyboard (#11622)
* fix(android): Ignore updating invalid selections (#11529)
* fix(developer): handle invalid default project path in options (#11556)
* fix(developer): handle missing data in .kps `<Keyboard>` (#11564)
* fix(developer): save touch layout editor selection when loading state (#11576)
* fix(developer): use correct variable when fixing up flick and multitap in Layer Properties dialog (#11578)
* fix(developer): correct the inference of default hint from flicks (#11583)
* fix(developer): make top row of Character Map 1 pixel to avoid DIV/0 in gestures (#11591)
* fix(web): fixes error on tab, enter when only a single input element is on the page (#11615)
* fix(web): correctly handle cross-origin stylesheets when calculating keyboard size and key cap font size (#11616)
* fix(web): pre-init keyboard activation awaits init before proceeding (#11617)
* fix(web): add null guard for missing debug info when reporting to Sentry (#11619)
* fix(android): host-page banner initialization (#11618)
* fix(developer): support Windows and Unicode names in .ttf (#11634)

## 17.0.325 stable 2024-05-15

* chore: update history for stable (#11448)

## 17.0.324 stable 2024-05-15

* chore(android): 17.0 stable release
* chore(ios): 17.0 stable release
* chore(linux): 17.0 stable release
* chore(mac): 17.0 stable release
* chore(web): 17.0 stable release
* chore(windows): 17.0 stable release
* chore(developer): 17.0 stable release

## 17.0.323 beta 2024-05-13

* chore(windows): Update crowdin string for Khmer (#11409)
* fix(android): disable Sentry session tracking (#11419)
* fix(mac): restore OSK keys that became invisible in mac OS 14 Sonoma (#11388)
* chore(android,ios): Add fv_nlakapamuxcheen (#11405)

## 17.0.322 beta 2024-05-10

* fix(web): fixes illegal KMW event state - can't focus a null element (#11385)

## 17.0.321 beta 2024-05-09

* fix(android/engine): Skip updating selection range if invalid (#11384)
* refactor(android/engine): Refactor updateSelection (#11389)

## 17.0.320 beta 2024-05-07

* fix(android): prevents mid-keystroke desynchronization when deleting selected text (#11367)
* fix(web): support SVG elements when checking className (#11365)
* fix(developer): define `lastSelLength` variable (#11366)

## 17.0.319 beta 2024-05-04

* fix(android): inverting a selection range would crash Keyman (#11345)
* fix(developer): handle missing Name element for File element in package compiler (#11352)
* fix(developer): handle missing Description element for File element in package compiler (#11354)

## 17.0.318 beta 2024-05-02

* fix(web): longpress shortcut activation should only consider northward part (#11306)
* chore(ios,mac): support build on Apple Silicon using Xcode 15.3 (#11302)

## 17.0.317 beta 2024-05-01

* chore(web): remove old reference-doc from alpha that has completed its purpose (#11322)
* fix(web): gesture-model initial-state, callback failure handling (#11321)
* fix(linux): Fix icon for .kmp files (#11295)

## 17.0.316 beta 2024-04-30

* fix(windows): check font count display none found (#11282)

## 17.0.315 beta 2024-04-26

* fix(web): osk-view hidden by default on construction (#11258)
* fix(android): fixes kbd text zoom to prevent accessibility cross-effects (#11281)
* fix(developer): support export of visual keyboard when Keyman for Windows not installed (#11244)
* chore(linux): Prepare for stable release (#11301)
* fix(core): reset on frame keys (#11172)
* fix(core): ldml backspace processing should delete all markers (#11254)

## 17.0.314 beta 2024-04-25

* fix(android/engine): URIEncode strings passed to Javascript (#11206)
* fix(android/app): Update storage permissions for Android 12.0+ (#11299)
* test(developer): keyboard info compiler messages unit tests 2 (#11253)

## 17.0.313 beta 2024-04-23

* chore(common): Set fetch-latest-cldr.sh executable (#11289)
* chore(common): Fix missing entries in HISTORY.md (#11290)
* fix(developer): report missing help to sentry instead of local xml (#11271)
* fix(developer): "use strict" for downlevel browsers in Server (#11276)

## 17.0.312 beta 2024-04-22

* fix(developer): emit JSON strings as characters, not surrogate pairs (#11243)
* fix(developer): prevent xmlns for LDML keyboard template child elements (#11251)
* fix(developer): test that MRU project exists before attempting to reference on startup (#11252)
* fix(developer): handle invalid Unicode values in touch layout builder (#11270)
* fix(developer): handle multiple instances of Server starting (#11272)
* fix(developer): lazy initialization of test window (#11275)
* chore(web): drops redundant builder-dependency from early gesture dev (#11278)
* feat(web): add recent-history log to gesture engine (#11277)
* test(developer): keyboard info compiler unit tests 2 (#11130)
* fix(web): fixes pred-text unit test instability (#11283)
* fix(developer): keep escaped plus-sign escaped (#11269)
* chore(resources): sync up with CLDR v45 release (#11004)

## 17.0.311 beta 2024-04-19

* fix(web): default parse of length styling for empty string (#11235)
* change(web): initializes OSK with keyboard if available during init (#11174)
* fix(ios): prevents post-restore kbd issues by refreshing kbd (#10952)
* fix(mac): handle text replacement cases during compliance detection (#11190)

## 17.0.310 beta 2024-04-18

* fix(web): now auto-scrolls if target element would be hidden after device rotation (#11210)
* fix(ios): deletion of selected text (#11179)
* fix(common): update emoji stripping for Unicode 15.1 (#11242)
* change(web): adjusts multitap timings (#11246)
* fix(web): prevents dropping of input during rapid multitouch typing (#11245)

## 17.0.309 beta 2024-04-17

* fix(ios): sample build script --debug detection (#10953)
* chore(web): simple layout reflow polish (#11237)
* chore(android): enables debugging and inspection of mobile app internal webviews (#11215)
* fix(web): prevents selection-clear for pure layer-switching multitaps (#11232)
* change(web): drops need for closures to optimize layout-reflow (#11238)

## 17.0.308 beta 2024-04-13

* chore(developer): use keyboard3 tag rather than DTD to identify LDML keyboard xml files (#11214)

## 17.0.307 beta 2024-04-12

* fix(common): specify title explicitly when opening PR with hub (#11173)
* refactor(web): better centralizes OSK layout internals to prepare for optimization efforts  (#11176)
* feat(web): VisualKeyboard layout-reflow optimization  (#11177)
* change(web):  OSK optimization, improved responsiveness  (#11140)
* fix(web): fixes up-flick shortcut issue for longpress when keys support downflicks (#11216)

## 17.0.306 beta 2024-04-11

* docs(ios): updates iOS app help for 17.0 banner changes (#11200)
* chore(oem/fv): Add fv sguuxs and update keyboard versions (#11198)

## 17.0.305 beta 2024-04-10

* fix(core): skip leading trail surrogate char in km_core_state_context_set_if_needed() (#11169)
* change(web): merges split async method in gesture engine  (#11142)
* fix(web): blocks nextLayer for keys quickly typed when multitapping to new layer when final tap is held (#11189)
* refactor(web): OSK spacebar-label updates now managed by layer object  (#11175)

## 17.0.304 beta 2024-04-09

* fix(android): atomically updates selection with text (#11188)
* fix(web): fix crash in nearest-key row lookup when touch moves out-of-bounds (#11178)

## 17.0.303 beta 2024-04-05

* fix(windows): decode uri for Package ID and filename (#11152)
* fix(common/models): suggestion stability after multiple whitespaces (#11164)

## 17.0.302 beta 2024-04-04

* fix(mac): load only 80 characters from context when processing keystrokes (#11141)

## 17.0.301 beta 2024-04-03

* feat(core): support modifiers=other (#11118)
* chore(core): dx better err message on embedded test vkeys (#11119)
* fix(web): key preview stickiness  (#10778)
* fix(web): early gesture-match abort when unable to extend existing gestures  (#10836)
* fix(web): infinite model-match replacement looping  (#10838)
* fix(web): proper gesture-match sequencing  (#10840)
* change(web): input-event sequentialization  (#10843)
* fix(web): proper linkage of sources to events  (#10960)
* fix(developer): handle buffer boundaries in four cases (#11137)
* chore(linux): Build packages for next Ubuntu version separately (#11153)
* fix(common): upgrade sentry-cli to 2.31.0 (#11151)
* fix(android/app): Track previous device orientation for SystemKeyboard (#11134)
* change(web): reworks nearest-key detection to avoid layout reflow (#11129)

## 17.0.300 beta 2024-04-02

* change(web): keyboard swaps keep original keyboards active until fully ready (#11108)
* fix(android/engine): Swap selection range if reversed (#11127)
* test(developer): keyboard info compiler unit tests (#11000)

## 17.0.299 beta 2024-04-01

* fix(ios):  address crash by reading full code point rather than code unit when trimming initial directional-mark (#11113)
* fix(mac): delete correct number of characters from current context when processing BMP or SMP deletes (#11086)
* feat(developer): disallow stray dollarsign in from pattern (#11117)

## 17.0.298 beta 2024-03-29

* chore(linux): Update debian changelog (#11096)
* fix(web): prevent layer switch key from erasing selection (#11032)
* fix(developer): prevent error when scrolling touch layout editor with no selected key (#11109)
* fix(common): make `isEmptyTransform` return true if passed a nullish transform (#11110)

## 17.0.297 beta 2024-03-28

* fix(common): properly handle illegal UnicodeSets to prevent crash in kmc-ldml compiler (#11065)
* fix(core,developer): variable/marker substitution in sets and strings (#11059)
* fix(developer):  in ldml compiler, generate compiler error if `from=` regex matches empty string (#11070)
* fix(core): calculate offset correctly when replacing marker in transform (fixes crash) (#11071)
* feat(developer): support comma in modifiers (#11075)
* fix(core): actions_normalize() length and dead store fix (#11100)
* chore(core): optimize ldml_event_state::emit_difference() when no diff (#11094)
* fix(ios): bad initial in-app layout, delayed banner, deprecated banner toggle (#10929)
* feat(developer/compilers): better unit test for suggestion accessibility (#11085)
* fix(core): fix pointer math in actions_normalize() (#11101)


## 17.0.296 beta 2024-03-27

* fix(developer): in model compiler, give correct key to shorter prefix words when a longer, higher-frequency word is also present (#11074)
* fix(oem/fv/android): Only add default keyboard if no keyboards exist (#11080)
* fix(core): correct typo in LDML test DTD reference (#11068)
* chore(android): Update crowdin strings for Mon (#11089)
* chore(android): Update crowdin strings for Khmer (#11090)

## 17.0.295 beta 2024-03-26

* docs(developer): add help site links (#10939)
* chore(common): Update keymanweb-osk.ttf to 4.1 (#11035)
* feat(linux): Start dbus if not running (#10863)

## 17.0.294 beta 2024-03-25

* chore(android,windows): Update crowdin strings for French (#11063)
* chore(common): Fix missing entries in HISTORY.md (#11064)
* fix(developer): catch sharing violation when saving mru (#11047)
* fix(developer): capture alt shortcuts in LDML keyboard debugger (#11049)
* fix(developer): correct path for kmc sourcemaps in sentry upload (#11051)

## 17.0.293 beta 2024-03-22

* chore(oem/fv): Update keyboard versions in keyboards.csv (#11013)
* fix(developer): suppress emission of new empty fields in package editor (#11009)
* fix(developer): return after calling await exitProcess (#11016)
* chore(core): refresh API docs for 17.0 (#10986)

## 17.0.292 beta 2024-03-21

* fix(developer): remove unused keyboard info compiler messages (#10991)
* fix(developer): font file meta data is invalid error (#10995)

## 17.0.291 beta 2024-03-20

* chore(mac): clean up code obsoleted by core (#10877)
* fix(linux): Replace deprecated `pkg_resources` module with `packaging.version` (#10860)

## 17.0.290 beta 2024-03-19

* fix(ios): Sync selection range for long contexts (#10956)

## 17.0.289 beta 2024-03-16

* fix(common): add unit test for --debug flag for builder (#10974)

## 17.0.288 beta 2024-03-13

* test(developer): kmc keyboard info compiler messages unit tests (#10848)
* fix(developer): getFontFamily now returns null if ttfMeta.promise errors (#10983)
* docs(android/app): Add data privacy policy page (#10985)
* fix(developer): min Keyman version for LDML is 17.0 (#10977)

## 17.0.287 beta 2024-03-12

* chore(developer): resolve excessive-fatal-exception issue (#10949)
* fix(linux): Remove `ibus-keyman.post{inst,rm}` and verify that `ibus-daemon` is running when we start km-config (#10788)

## 17.0.286 beta 2024-03-11

* chore(web): improve the clarity of hints on OSK for Android (#10971)

## 17.0.285 beta 2024-03-08

* fix(windows): GetKeyboardLanguage exits early on an invalid keyboard ID (#10936)

## 17.0.284 beta 2024-03-07

* fix(web): globe key highlighting (#10932)
* chore(web): updates mtnt model used by test page (#10935)
* fix(developer): kmc keyboard info compiler no kmp error (#10893)
* fix(developer): Treat Right Shift as Shift in debugger (#10919)
* docs(developer): kmc-model api documentation (#10921)
* docs(developer): kmc-model-info api documentation (#10922)
* docs(developer): kmc-keyboard-info api documentation (#10923)
* docs(developer): kmc-package api documentation (#10924)
* fix(developer): normalize all input paths in .kps compiler (#10950)

## 17.0.283 beta 2024-03-06

* fix(android): fixes context-change detection for repeated-char cases (#10873)
* fix(developer): search-term quote replacement was not global (#10934)
* chore(common): Update ldml out of techpreview (#10913)
* fix(developer): fix for errant \uXXXX error (#10946)
* fix(developer): suppport loose match for CompilerEvents (#10948)
* refactor(oem/fv/android): Install fallback keyboard (#10907)
* refactor(android/app): Move storage permission checks (#10904)
* fix(common): missing script exec bit (#10951)

## 17.0.282 beta 2024-03-05

* fix(developer): move osk.ts from developer-utils to kmc-kmn (#10903)
* fix(developer): hint text special characters (#10927)
* docs(developer): kmc-ldml api documentation (#10917)
* chore(developer): consolidate external links in Developer messages (#10918)
* fix(developer): message export filename was title case (#10941)

## 17.0.281 beta 2024-03-04

* chore(linux): Update debian changelog (#10897)
* fix(ios): cross-paragraph keyboard rules (#10905)
* chore(developer): deploy compiler messages to help site (#10906)

## 17.0.280 beta 2024-03-01

* fix(developer): make sure scroll bar appears when needed in global welcome (#10818)
* chore(common): loosen .keyman-touch-layout schema layer id requirements (#10819)
* docs(developer): kmc-kmn api documentation and some message documentation (#10856)
* refactor(developer): cleanup kmn compiler message namespaces (#10867)
* refactor(developer): reorganize messages for adding details (#10878)
* fix(ios): context-initial diacritic handling (#10589)
* feat(developer): adds kmc message command (#10888)
* test(developer): kmc unit test infrastructure messages (#10825)
* fix(android): handle surrogate pairs in selection range indexing (#10885)
* feat(windows): move the code kmshell.dpr to unit so that only the includes are in dpr (#10871)

## 17.0.279 beta 2024-02-29

* fix(linux): Fix libkeymancore-dev dependencies (#10880)
* chore(resources): update to latest ldml: xmlns, uset (#10865)

## 17.0.278 beta 2024-02-28

* docs(linux): Updated whatsnew for Keyman for Linux 17 (#10858)
* fix(web): Save context state on reset (#10869)
* chore(android/engine): Reduce toast notifications after installations (#10868)
* fix(android/engine): Check selection indexes (#10857)

## 17.0.277 beta 2024-02-27

* fix(developer): exit kmc test data if any failure messages occurred (#10805)
* docs(windows): add whats new Keyman for Windows  17.0 (#10783)
* feat(developer): add api-extractor and api-documenter, config, and build integration (#10839)
* feat(developer): publish api documentation to help.keyman.com (#10841)
* docs(developer): npm package readme files (#10842)
* refactor(developer): move osk module from common-types to developer-utils (#10845)
* chore(linux): Update debian changelog (#10828)
* fix(developer): repeated options in kmc must now be fully specified (#10821)
* docs(developer): add documentation for kmc-analyze (#10854)

## 17.0.276 beta 2024-02-26

* chore(linux): Fix autopkg tests (#10824)

## 17.0.275 beta 2024-02-23

* chore(core): Update sample ldml keyboard (#10791)
* fix(developer): publish keymancore-1.dll symbols (#10797)
* fix(developer): improve uploading of sourcemaps to sentry (#10798)
* fix(android/engine): Fix how keyboard picker menu exits (#10806)
* chore(linux): Fix `upload-to-debian.sh` script for beta releases (#10784)
* chore(linux): Fix API verification after adding SONAME (#10813)
* chore(linux): Address comments from Debian mailing list (#10800)
* docs(linux): Update how to build binary package with docker (#10814)
* chore(linux): Update debian changelog (#10785)

## 17.0.274 beta 2024-02-22

* fix(android/engine): Remove logs for uninitialized default keyboard (#10782)
* refactor(android/app): Allow SystemKeyboard to install default keyboard and dictionary (#10794)
* chore(android/app): Remove logging for install referrer details (#10795)
* docs(android/app): Update tablet screenshots (again) (#10796)
* docs(android): Update screenshots for help (#10808)
* chore(web): proper reporting of errors from es5 KMW (#10807)
* fix(android): first keystroke when context is empty (#10809)
* chore(linux): Update Debian `control` file (#10787)

## 17.0.273 beta 2024-02-21

* fix(developer): match kmc console formatting for messages in IDE (#10775)
* fix(developer): ensure fatal errors report message or made non-fatal (#10777)
* chore(developer): add LDML specific text to Project view (#10779)
* fix(developer): reset debugger after ldml keyboard compile (#10780)
* fix(developer): crash opening invalid project (#10781)
* docs(ios): document gestures (#10763)

## 17.0.272 beta 2024-02-20

* fix(core): fix output append logic in any_group::apply_transform (#10758)
* fix(android): Match phone suggestion banner styling on tablet (#10760)
* docs(mac): Document What's New for version 17.0 (#10764)
* fix(developer): Rename missing variables in tests (#10773)
* chore(developer): reduce WARN_TouchLayoutUsesUnsupportedGesturesDownlevel to HINT (#10766)
* fix(developer): prevent renaming of new project types in New Project dialog (#10767)
* fix(developer): return an error code if build-test-data fails (#10765)
* fix(developer): allow more parameters in kmc.cmd (#10759)
* chore(linux): Ignore missing .symbols file in stable branch (#10674)
* chore(linux): Update symbols file with current version (#10575)
* docs(linux): Update screenshots and FAQ (#10768)
* chore(linux): Fix `debian.sh` by removing quotes (#10772)

## 17.0.271 beta 2024-02-19

* chore(developer): add infrastructure messages tests (#10756)
* chore(windows): Update more PT strings (#10718)
* docs(android/app): Update tablet screenshots (#10726)
* fix(web): sticky special-key highlighting (#10729)
* fix(web): disables modipress for layer-switch keys with subkeys (#10745)
* fix(web): missing null-guard for hardware keystrokes without active Keyman keyboard (#10740)
* docs(ios): add what's new in 17.0 (#10748)
* fix(web): handling of page-elements focused before engine initialization (#10744)
* fix(ios): multitap consistency after new-lines (#10728)
* docs(common): Update website README (#10738)

## 17.0.270 beta 2024-02-17

* fix(core): Update more tests and fix ldml_transforms (#10735)

## 17.0.269 beta 2024-02-15

* docs(android): Document using gestures on touch, and remove references to Browser (#10686)
* docs(android/app): Add help page on using the banner (#10691)
* fix(web): corrects Android over-deletion of selected text, other context diffs involving selected text (#10662)
* fix(web): disables banner interaction when suggestions are absent (#10695)
* fix(web): longpress validation by base key, not current location (#10707)

## 17.0.268 beta 2024-02-15

* chore: move to beta

## 17.0.267 alpha 2024-02-14

* fix(linux): Use temp dir if we can't create cache dir (#10681)
* feat(developer): errs on unparseable regex (#10689)
* feat(developer): errs on \uXXXX escapes (#10701)
* docs(common): Fill in missing HISTORY.md entries (#10704)

## 17.0.266 alpha 2024-02-13

* chore(windows): add const to argument definition (#10677)
* feat(windows): update windows engine unit tests to match removal of engine cached context (#10184)
* fix(web): prevent invalid longpress shortcut triggers (#10641)

## 17.0.265 alpha 2024-02-12

* fix(developer): keyboard that targets only web can cause crashes and other problems (#10664)
* fix(android/engine): Download multiple dictionaries (#10680)

## 17.0.264 alpha 2024-02-09

* chore(linux): Don't set VERSION_TAG to …-local in .deb packages (#10675)
* fix(web): banner robustness when touched 2+ times at once (#10672)

## 17.0.263 alpha 2024-02-08

* fix(linux): Ignore keys with IBUS_MOD4_MASK set (#10668)

## 17.0.262 alpha 2024-02-07

* refactor(windows): Use km_core_actions struct instead of queue (#10557)
* chore(core): move action apis into keyman_core_api_actions.h (#10569)
* feat(core,developer): support normalization=disabled (#10586)
* feat(developer): range warning for non-NFD chars (#10614)
* fix(core): make `km_core_state_get_actions()` idempotent (#10585)
* fix(core): strip markers in `actions_update_app_context_nfu()` (#10607)
* fix(core): surrogate handling, markers in app context, and memory leak (#10618)
* fix(windows): support unicode strings properly in logs (#10650)
* fix(developer): ensure project populate files actually adds files (#10651)
* fix(linux): Use raw string for regex (#10652)
* fix(windows): `wstrtostr` should use `WideCharToMultiByte` (#10660)
* chore(windows): rename actionItem to outputString for clarity (#10661)
* fix(web): handling of backspaces when left-context is empty (#10584)
* fix(ios): long-held backspace handling (#10633)
* fix(web): banner scroll positioning after reversions (#10643)
* chore(web): removes unused banner events (#10644)

## 17.0.261 alpha 2024-02-06

* fix(web): app/webview sourcemap inlining (#10631)
* fix(android/app): Cleanup AndroidManifest handling of *.kmp (#10624)

## 17.0.260 alpha 2024-02-03

* chore(oem/fv): Update versions in keyboards.csv (#10606)
* fix(linux): Fix path to artifacts when uploading to llso (#10609)
* docs(linux): Update sample settings (#10610)
* chore(linux): Fix API check if lines got removed in .symbols file (#10612)

## 17.0.259 alpha 2024-02-02

* feat(developer):  double markers once again (#10541)
* feat(core): ldml double marker, C++ side (#10563)
* feat(core,developer): simplify markers (#10565)
* chore(common): cancel earlier builds when new test builds triggered (#10593)
* chore(linux): Use correct format specifier for `g_utf8_strlen` (#10599)
* fix(linux): Fix version comparison (#10576)
* chore(common): Add entries from 16.0.145 HISTORY.md (#10603)
* chore(common): history missing message fixup (#10601)
* chore(linux): Update debian changelog (#10596)

## 17.0.258 alpha 2024-02-01

* chore(developer): add unicode-license.txt for ldml keyboards data (#10568)
* feat(core): ldml reorder marker processing (#10539)
* chore(developer): kmc ldml build - call c8 directly for tests (#10522)
* fix(web): app/webview control flow for initial layer (#10571)
* chore(linux): Upgrade artifacts to v4 (#10577)
* chore(linux): Fix the path of downloaded artifacts (#10594)

## 17.0.257 alpha 2024-01-31

* epic: Keyman Core normalization (#10390)
* fix(web): OSK resource-loading promise's font-wait (#10506)
* fix(web): documentation-keyboard font size, font scaling (#10507)
* feat(web): testing page for font-load / key-scale interactions (#10534)
* fix(web): proper await for special-osk font, use of it in key scaling (#10535)
* fix(web): ensure use of actual key-cap / label font in key-scale ops (#10555)
* docs: Update websites README.md (#10566)
* fix(developer): kmc exit needs to wait for Sentry (#10558)
* fix(developer): make sourcePath calculation for kmc-keyboard-info more lenient (#10559)
* chore(developer): disable .js output for LDML keyboard compiler for 17.0 (#10560)
* chore(linux): Replace Lunar with Noble (#10561)
* chore(linux): Adjust test expectations for ICU >= 73 (#10573)

## 17.0.256 alpha 2024-01-30

* fix(linux): packaging on Ubuntu and Debian servers should give message instead of warning because we don't have patched ibus there (#10537)
* feat(developer): turn on developer side normalization (#10528)

## 17.0.255 alpha 2024-01-29

* chore(ios): eliminate xcglogger dependency (#10446)
* feat(developer): ldml marker normalization fix (#10527)
* chore(linux): Build with webkitgtk 4.1 instead of 4.0 (#10483)

## 17.0.254 alpha 2024-01-27

* feat(core): ldml marker normalization fix (#10517)

## 17.0.253 alpha 2024-01-26

* feat(web): keystroke pre-processing unit tests (#10464)
* feat(web): key-event rule processing unit tests for mnemonic, KMW 1.0 keyboards (#10475)
* fix(web): touch-layout fontsize property use (#10504)

## 17.0.252 alpha 2024-01-25

* fix(common): ldml: fix typo in kmc error message (#10484)
* chore(developer): `--enable-source-maps` parameter for kmc wrapper (#10496)
* fix(android/engine): Fix OSK widths (#10442)
* fix(web): banner suggestion resizing after device rotation (#10508)

## 17.0.251 alpha 2024-01-24

* chore(developer): upgrade ngrok to v3 (#10359)

## 17.0.250 alpha 2024-01-23

* fix(android/engine): Add RECEIVER_EXPORTED flag for broadcast receiver (#10463)
* fix(web): osk responsiveness to held modifiers on legacy kbds (#10437)
* fix(android/engine): Skip final globe key action (#10465)
* feat(developer): marker normalization in .ts (#10443)
* fix(android): hardware keystrokes now include mnemonic processing (#10445)

## 17.0.249 alpha 2024-01-22

* feat(web): suggestion banner UI enhancements - suggestion expanding + scrollable banner (#7934)
* chore(web): worker sourcemap cleanup, mobile app webview sourcemap compat (#10413)

## 17.0.248 alpha 2024-01-19

* chore(android): Update targetSdkVersion to 34 (#10393)
* fix(web): cancels active gestures on globe-key use (#10352)
* fix(web): keyboard-documentation rendering mode (#10417)
* fix(ios): multiple keyboard slide-in animations on app start (#10362)
* fix(web): U_ key id -> text for all subkeys; is now preprocessed (#10434)
* chore(android/app): Update whatsnew for 17.0 (#10395)
* fix(web): bulk renderer interface for recent-version targeting keyboards (#10427)
* feat(core): cross-segment markers (#10394)

## 17.0.247 alpha 2024-01-18

* fix(ios): banner image management (#10337)
* feat(core): unescape u (#10356)
* chore(developer,core): change sample and test files to use \u{…} (#10391)

## 17.0.246 alpha 2024-01-17

* fix(web): Add null check for changing the keyboard during typing (#10346)
* chore(common): Update crowdin strings for Khmer (#10411)
* docs(common): Update website README.md (#10399)
* docs(linux): Add documentation for Core API verification (#10409)
* fix(web): right-flick gesture-preview positioning (#10406)

## 17.0.245 alpha 2024-01-16

* chore(core): Ignore C++ symbols (#10386)
* fix(android): Fix OSK rotation issues (#10373)
* feat(developer): add `-m` parameter to kmc to control message severity (#10258)

## 17.0.244 alpha 2024-01-15

* refactor(core): Move `km_core_state_context_*` tests to separate file (#10375)
* feat(developer): warn on gesture support for downlevel (#10364)
* fix(web): use KMW OSK font for "**" special-text in key hints (#10371)
* docs(linux): Update sample settings (#10374)

## 17.0.243 alpha 2024-01-12

* chore(core): split out some files (#10327)
* chore(common): Update crowdin strings for French, German, Spanish (Latin America) (#10368)
* fix(web): prediction time-limiter issues, performance (#10330)
* fix(developer): test-page model loading (#10328)
* fix(ios): use safe area to position Keyman menu correctly (#10339)
* chore(linux): fix GHA packaging (#10366)
* refactor(linux): Use `km_core_state_context_set_if_needed` (#10354)

## 17.0.242 alpha 2024-01-11

* fix(windows): serve images for readme files in Keyboard Install (#10312)
* fix(developer): remove unused tools/customise menu (#10321)
* fix(developer): cleanly handle filling a new osk file from layout (#10322)
* fix(developer): new file support for project 2.0 (#10323)
* chore(common): builder action launch typos (#10361)
* feat(core): support stacked markers, prep for marker segments (#10326)

## 17.0.241 alpha 2024-01-10

* chore(deps-dev): bump follow-redirects from 1.14.9 to 1.15.4 (#10340)
* Update character-map.md (#10345)
* chore(linux): Output dput exit code after upload to llso (#10349)
* fix(android/engine): Use fallback keyboard if keyboard index undefined (#10342)
* fix(android): Use alert dialog when package is missing touch keyboards (#10335)
* chore: cleanup recent history entries without PR title (#10298)
* chore(web): better logging for gesture-recognizer tools build script (#10329)

## 17.0.240 alpha 2024-01-09

* chore(linux): Retry llso upload (#10331)

## 17.0.239 alpha 2024-01-08

* fix(mac): write persisted options returned from Core (#10275)
* chore(web): remove lm-worker `Array.from` polyfill and make `Symbol` polyfill conditional (#10255)
* change(web): lm-worker bundling + wrapper script rework (#10264)
* refactor(web): transformation of build-bundler into a common es-bundling command (#10265)
* change(web): KeymanWeb bundler-script centralization (#10267)
* feat(web): alternate artifact - es6-bundled Web (#10257)
* feat(web): es6 artifact for app/webview (#10274)
* fix(web): adds null-guard for longpress-key validation (#10299)
* fix(web): special key highlighting when pressed (#10296)

## 17.0.238 alpha 2024-01-03

* fix(developer): quell internal error when  a section fails (#10300)
* fix(core): ldml fixes for normalization between transform groups (#10290)
* feat(core): more regex testing (#10304)
* chore(core): better reporting in ldml tests (#10305)
* feat(developer): escape bad markers (#10306)

## 17.0.237 alpha 2024-01-02

* feat(developer): Consolidate public APIs for kmc modules (#10208)
* chore(developer): honor prompt to upgrade in kmc (#10218)

## 17.0.236 alpha 2023-12-22

* fix(oem/fv): Add fv_kwadacha_tsekene (#10292)
* fix(common): give warning if emcc.py is present but not executable (#10289)

## 17.0.235 alpha 2023-12-21

* feat(developer): ldml: more improvement for key-not-found (#10236)

## 17.0.234 alpha 2023-12-20

* fix(mac): Improve detection of non-compliant apps (#10282)
* fix(web): default multitap logic for shift-layer shift multitaps (#10281)
* fix(web): keyboard functionality on Android 5.0 / Chrome 35 (#10284)

## 17.0.233 alpha 2023-12-19

* fix(web): unbreak some ts paths (#10279)
* fix(core): clear core context on invalidate cache (#10230)
* fix(oem/fv): Add fv_hulquminum_combine (#10269)

## 17.0.232 alpha 2023-12-18

* feat(web): adds highlighting to BKSP while held (#10270)

## 17.0.231 alpha 2023-12-15

* chore(web): Better match directory structure for headless engine tests (#10232)
* fix(web): Fix types that listInputs and isKMWInput accept (#10233)
* chore(web): Add unit tests for `pageContextAttachment` (#10250)

## 17.0.230 alpha 2023-12-14

* fix(core): ldml: fix bad usage of unique_ptr (#10252)
* chore(linux): docker: fix Node.js install, make customizable (#10251)
* fix(web): unit test for null-null keyboard switch event from #10245 (#10253)
* fix(web): fixes android-webview emulation Web test page (#10238)
* epic: new gesture engine (#7324)
* fix(mac): improve code signing robustness (#10243)
* chore(mac): use core actions struct (#10066)
* chore(common): Add entries from 16.0.144 HISTORY.md (#10260)
* chore(web): Remove dead code (#10231)

## 17.0.229 alpha 2023-12-13

* chore(ios): FV certificate key (#10228)
* feat(core): action struct to action items conversion (#10115)
* fix(developer): various project fixes (#10151)
* feat(developer): Add Window menu (#10154)
* feat(developer): New Project opens in new process (#10155)
* fix(developer): two errors in touch layout editor (#10211)
* feat(developer): Move options to ~/.keymandeveloper/options.json (#10216)
* feat(core): ldml improve key-not-found (#10090)
* fix(core): remove a TODO-LDML (#10198)
* fix(core): improve memory allocation issues in calling into icu (#10222)

## 17.0.228 alpha 2023-12-11

* chore(mac): force detach of disk image during build after 5 failures (#10201)
* chore(linux): Small refactoring to make it easier to read (#10196)
* chore(common): Specify Keyman repo on GitHub actions (#10203)
* chore(android,mac): Update Kibaku crowdin strings (#10174)
* chore(android,mac): Update Mon crowdin strings (#10175)
* fix(developer): crash loading a project when referenced .xml is missing (#10209)
* feat(developer): kmc emit hint if project file is old version (#10158)
* fix(common): kpj loader needs to handle empty Files element (#10157)

## 17.0.227 alpha 2023-12-08

* fix(windows): debug includes libcmtd vs libcmt.dll (#10176)
* fix(linux): Do not restart Fcitx (#10180)

## 17.0.226 alpha 2023-12-07

* fix(web): accidental suggestion-banner retoggle when disabling predictions (#10014)
* fix(web): Fix null error with legacy keyboards (#10141)

## 17.0.225 alpha 2023-12-06

* feat(developer): Multi-process model for projects (#10114)
* fix(developer): prevent opening .kpj in multiple processes (#10137)
* fix(android/app): InfoActivity pass external links to browser (#10153)

## 17.0.224 alpha 2023-12-05

* chore(linux): Update debian changelog (#10121)

## 17.0.223 alpha 2023-12-04

* chore(web): Add option to create test coverage report file (#10124)

## 17.0.222 alpha 2023-12-02

* fix(common): publish @keymanapp/developer-utils as npm module (#10118)

## 17.0.221 alpha 2023-12-01

* chore(windows): removed cached context windows engine (#10065)
* change(android): flishy flashy mitigation, round 1 (#10017)
* change(android): smoother keyboard initialization (#10022)
* fix(android): no suggestions available when swapping pred-text target language (#10061)
* chore(developer): require project file to exist (#10092)
* chore(linux): Add support for loong64 architecture (#10109)
* fix(web): Don't throw errors after detach (#10086)

## 17.0.220 alpha 2023-11-30

* fix(core): set_if_needed updates an empty cached context (#10098)
* fix(core): check for null termination (#10101)

## 17.0.219 alpha 2023-11-29

* fix(developer): path separator for kmc-package (#10064)
* fix(developer): projects 2.0 internal path enumeration (#10016)
* fix(web): Fix attachment-api tests (#10085)
* fix(web): Also move source map (#10089)

## 17.0.218 alpha 2023-11-27

* feat(developer): ldml: err/hint on illegal/pua chars (#10029)

## 17.0.217 alpha 2023-11-24

* feat(developer): warn on usage of virtual keys in rule output (#10062)
* fix(core): memory management of options in action struct (#10073)
* chore(linux): Update debian changelog (#10047)
* chore(core): Add test keyboard for text selection tests (#10026)

## 17.0.216 alpha 2023-11-23

* fix(common): kmx struct alignment (#9977)
* fix(developer): vis kbd callbacks instead of throw (#9979)
* fix(core): dx: ldml: startContext should be optional in ldml test data (#10021)
* chore(windows): remove `wm_keymandebug` messages and functions (#10055)
* chore(windows): remove legacy LoadKeyboard (#10057)
* chore(windows): remove unused globals relating to old keyboard debugging (#10058)
* chore(core): remove vkey output from kmx processor (#10060)
* fix(web): app/webview did not clear deadkeys on context-reset (#10039)

## 17.0.215 alpha 2023-11-22

* feat(core): ldml marker normalization (#9761)
* feat(core): ldml backspace transform (#9960)
* feat(core): ldml tertiary reordering (#9962)
* fix(web): fixes OSK viewport scaling on iOS devices (#10035)

## 17.0.214 alpha 2023-11-20

* fix(ios): fv: replace Zip framework to prevent crash on startup (#10018)

## 17.0.213 alpha 2023-11-17

* fix(linux): Fix packaging GHA (#10020)
* fix(android): Always display HTML banner when suggestions aren't available (#9696)

## 17.0.212 alpha 2023-11-16

* chore(web): splits banner.ts into separate files per banner type (#9987)
* refactor(web): inactive banner management (#9988)

## 17.0.211 alpha 2023-11-15

* fix(windows): setup.inf generation had whitespace (#10000)
* feat(developer): Support v2.0 projects in TIKE (#9949)
* feat(developer): New Project - Description field and tweaks (#9950)
* chore(developer): hide 'Remove From Project' for v2.0 projects (#9956)
* feat(developer): Support loading XML LDML keyboards in TIKE (#9963)
* feat(developer): show .xml LDML keyboards in project (#9964)
* fix(developer): kmc build ldml keyboard should create output folder (#9966)
* fix(developer): Project upgrade messages now show in Messages panel (#9969)
* feat(developer): Project Settings Form for 17.0+ projects (#9984)
* chore(developer): only build source files (#9985)
* chore(developer): manage SourcePath in project upgrade (#9986)
* feat(developer): handle errors loading projects (#9989)
* chore(developer): show only source path files in project views (#9995)
* chore(developer): handle project version checks cleanly in kmc (#9996)
* fix(common): dx: don't call exit(0) on failure (#10005)
* fix(linux): Improve triggering of packaging GHA (#10009)
* chore(web,developer): Move keymanweb-osk.ttf to common/resources (#9993)

## 17.0.210 alpha 2023-11-14

* docs(common): Update CODEOWNERS for web (#9997)
* chore(core): Add additional API checks (#9867)

## 17.0.209 alpha 2023-11-13

* docs(windows): update meson and emscripten details (#9933)
* fix(web): Increase size of spacebar text (#9954)

## 17.0.208 alpha 2023-11-08

* fix(web): Fix clearing of deadkeys (#9944)
* fix(web): Ignore `osk-always-visible` on non-desktop devices (#9951)
* fix(linux): Fix baseline tests (#9967)
* chore(linux): Fix GHA triggering (#9965)
* fix(linux): Fix trigger for baseline tests (#9968)

## 17.0.207 alpha 2023-11-07

* refactor(web): Link to `index.html` in test pages (#9953)
* feat(developer): Add more non-printing characters (#9846)

## 17.0.206 alpha 2023-11-06

* chore(developer): remove compile.pas and CompileErrorCodes.pas (#9924)
* chore(common): remove prepublish step from package.json (#9937)
* feat(developer): provide line number for some kmw compiler messages (#9938)
* fix(developer): Sentry in Server should honour reporting settings (#9940)
* fix(developer): resilience in loading Server config and cache files (#9941)

## 17.0.205 alpha 2023-11-03

* fix(developer): use KeymanWeb.Codes for 17.0+ (#9913)
* fix(developer): don't use osk-always-visible on touch devices (#9917)
* chore(linux): Improve repository_dispatch (#9865)
* chore(linux): Refactor deb-packaging.sh script (#9866)
* chore(linux): Add Core API version number (#9877)
* fix(developer): kmc code generation for context(n) in context (#9932)
* chore(developer): remove obsolete 'Allow Multiple Instances' and 'Use Legacy Compiler' options (#9934)
* chore(developer): common/include dep for kmcmplib (#9935)
* feat(common): ldml update to WIP cldr data (#9919)

## 17.0.204 alpha 2023-11-02

* fix(web): fixes doc-kbd display of default layer when it's not defined first (#9891)
* fix(developer): compiler crash when no project loaded (#9898)
* fix(developer): debug flag for compiling keyboards (#9901)
* chore(developer): verify kmp.json output from kmc-package (#9844)
* fix(developer): Project MRU now saves correctly (#9902)
* chore(common): fixup SchemaValidators error handling (#9903)
* chore(developer): change field label to 'Related Package ID' in Related Packages dialog (#9904)
* fix(developer): open editor links in new window (#9905)
* fix(developer): enable and update unit tests (#9907)
* fix(developer): layout builder - maintain presentation during undo (#9914)
* feat(developer): extract font family from .ttf in kmc-keyboard-info (#9859)
* fix(developer): raise error if virtual key in context string (#9908)
* feat(developer): Compile button in TIKE toolbar (#9910)
* chore(developer): rename messages.ts for clarity (#9920)
* fix(developer): warn if layer switch key is missing ID (#9921)
* fix(developer): restore selection in layout builder even with duplicate ids (#9922)
* fix(developer): enable line breaks in debugger (#9906)

## 17.0.203 alpha 2023-10-30

* fix(linux): Fix uninstallation of shared keyboard (#9880)
* chore: include ci/pull-requests.inc.sh for help uploads (#9897)

## 17.0.202 alpha 2023-10-30

* chore(common): unify pull-request creation scripts (#9888)
* epic: Keyman Core for Mac (#7857)

## 17.0.201 alpha 2023-10-28

* fix(core): build: support -t parameter correctly (#9820)
* fix(developer): handle displayMap correctly with 'fill from layout' (#9861)

## 17.0.200 alpha 2023-10-27

* chore(developer,common,core): update to latest CLDR v44 (#9842)

## 17.0.199 alpha 2023-10-26

* fix(developer): handle xml errors in package compiler (#9821)
* fix(developer): server download Keyman link (#9822)
* chore(common): handle invalid XML in kpj-file-reader (#9824)
* fix(developer): reduce confusion in Unicode fields in touch layout editor (#9839)

## 17.0.198 alpha 2023-10-25

* chore(common): Add entries from 16.0 HISTORY.md (#9826)
* feat(core): new actions APIs (#9828)

## 17.0.197 alpha 2023-10-24

* chore(linux): Rename (lib)kmnkbp to (lib)keymancore  ️ (#9793)
* chore(linux): Rename `namespace kbp` to `core`  ️ (#9792)
* chore(linux): rename keyboardprocessor_ldml.* to keyman_core_ldml  ️ (#9791)
* chore(linux): Rename libkmnkbp0-0 package  ️ (#9795)
* fix(web): proper disabling of prediction timeout for prediction unit tests (#9835)

## 17.0.196 alpha 2023-10-20

* fix(mac): move keyboard menu items to main Input Menu from submenu (#9777)
* feat(core): initial normalization (#9728)
* chore(core): dx: ldml test improvement, backspace test (#9759)
* docs(common): macos build update (#9809)
* fix(web): enhances integrated test stability (#9718)
* chore(linux): Update packaging GHA  ️ (#9812)
* chore(linux): Rename KBP to CORE  ️ (#9794)
* chore(linux): Fix build by adding one character (#9817)

## 17.0.195 alpha 2023-10-19

* chore(linux): Allow to collect coverage on TC (#9790)
* fix(common): don't use URL in common/web/types (#9798)
* chore: update kmp.schema.json and docs for kps schema (#9800)
* docs(windows): update text and images for windows 11 (#9689)

## 17.0.194 alpha 2023-10-18

* chore(linux): Re-enable building for Ubuntu 23.10 Mantic (#9780)
* chore(linux): Add missing tests (#9783)
* fix(web): fixes touch form-factor default kbd on cookieless keymanweb.com page load (#9786)
* fix(developer): three kmc .keyboard_info generation bugs (#9784)
* fix(developer): handle invalid project folders cleanly (#9785)
* chore(linux): Fix build scripts (#9781)

## 17.0.193 alpha 2023-10-17

* fix(developer): kmc crash on start (#9771)
* fix(core): don't use double newlines in debuglog (#9258)
* chore(linux): Add code coverage index page (#9758)
* feat(linux): Allow installing keyboards with arbitrary language  ️ (#9756)
* fix(linux): Fix crash initializing Sentry with Python < 3.10 (#9774)
* chore(linux): Rename `kbp_state_get_intermediate_context` to `km_core…`  ️ (#9775)

## 17.0.192 alpha 2023-10-16

* fix(ios): missing backslash in build script (#9765)
* chore(linux): Speed up ibus-util tests (#9754)
* feat(linux): Allow loading of keyboards with arbitrary language  ️ (#9735)

## 17.0.191 alpha 2023-10-15

* chore: update readme for keyboard_info schema (#9746)

## 17.0.190 alpha 2023-10-12

* chore(web): web build streamlining (#9743)
* chore(linux): Add code coverage reports for keyman-config and keyman-system-service (#9753)

## 17.0.189 alpha 2023-10-11

* refactor(linux): Use auto cleanup (and fix some memory leaks) (#9648)
* refactor(linux): Simplify adding keyboard (#9734)

## 17.0.188 alpha 2023-10-11

* chore(android,windows): Update crowdin strings for Kannada (#9737)

## 17.0.187 alpha 2023-10-11

* chore(developer): convert Server to ES Modules (#9673)
* chore(common): convert hextobin to ES Modules (#9676)
* chore(common): convert resources/build/version to ES Modules (#9678)
* chore(common): keyman-version now generates only es module (#9680)
* chore(common): cleanup final Typescript non-ESM metadata (#9681)
* chore(ios): renew certificate (#9697)
* feat(core): match any marker (#9687)
* feat(developer): ldml fix all remaining TODOs around markers and variables (#9688)
* fix(windows): re-enable signature check (#9695)
* fix(common): fix schema fixer (#9727)
* chore(core): rename keyboardprocessor.h to keyman_core_api.h (#9723)
* chore: rename km_kbp_ to km_core_ (#9724)
* chore(developer): fixup signcode paths for server (#9730)
* fix(linux): Explicitly initialize GTK (#9706)
* chore(linux): Improve Sentry reports (#9725)
* epic(developer): refactor package-metadata for 17.0 (#9485)
* chore: clean up a few minor discrepancies in builder.inc.sh (#9731)
* fix: path for ESM increment-version, and exit code (#9738)

## 17.0.186 alpha 2023-10-04

* feat(developer): show an INFO message when warnings have failed a build (#9652)
* chore(developer): reduce duplicate words warning to hint (#9653)
* feat(developer): issue hint if package includes keyboard source files (#9658)
* chore(developer): switch on code coverage reporting for kmc (#9662)
* fix(core): clean cached ICU in core (#9668)
* feat(developer): warn if .kps includes a .js which is not touch-capable (#9667)

## 17.0.185 alpha 2023-10-03

* chore(web): Add non-printing characters to the OSK (#9547)
* feat(developer): support `store(&version) '17.0'` (#9656)
* chore(developer): add test for `checkFilenameConventions == false` or unset (#9661)
* feat(developer): ldml scan codes support (#9615)

## 17.0.184 alpha 2023-10-02

* fix(web): fixes toolbar refocus timing after a keyboard change (#9618)

## 17.0.183 alpha 2023-09-29

* feat(web): browser-KMW support for default subkeys (#9496)
* refactor(linux): Add more tests for `keymanutil.c`  ️ (#9595)

## 17.0.182 alpha 2023-09-28

* chore(web): builds that output to web/build/publish should also clean it (#9613)
* chore(linux): Dynamically choose display number (#9629)
* docs(linux): Update sample tasks.json for Linux (#9634)
* chore(common): Add Crowdin strings for Mon (#9550)
* fix(developer): only include mobile touch platform in basic project (#9549)
* fix(windows): fix the ellipsis for longer text on buttons (#9638)

## 17.0.181 alpha 2023-09-26

* chore: workaround npm/cli#3466 when bundling internal deps (#9536)
* chore(linux): Check and restart background processes (#9608)

## 17.0.180 alpha 2023-09-25

* fix(linux): Fix detection of unit tests (#9606)

## 17.0.179 alpha 2023-09-22

* chore(resources): ldml: update to keyboard3 (#9588)
* change(android,web) Use web-based popup key longpresses (#9591)

## 17.0.178 alpha 2023-09-21

* fix(linux): Correctly open files linked from help page (#9601)
* chore(linux): Fix bugs, add dependency and update documentation (#9602)

## 17.0.177 alpha 2023-09-20

* chore(linux): Add coverage action to `ibus-keyman/build.sh` (#9583)
* docs(common): Fix documentation for `builder_describe_internal_dependency` (#9582)

## 17.0.176 alpha 2023-09-19

* chore(oem/fv/android): Update Gradle to 7.4 (#9590)
* refactor(linux): Rename defines to clarify purpose  ️ (#9584)
* feat(core): drop \u1234 format (#9560)

## 17.0.175 alpha 2023-09-18

* chore(linux): Split startup process (#9570)

## 17.0.174 alpha 2023-09-16

* refactor(linux): Reformat file (#9569)

## 17.0.173 alpha 2023-09-13

* chore(common): Update to Unicode 15.1 (#9555)

## 17.0.172 alpha 2023-09-12

* fix(linux): Prevent exception if neither USER, LOGNAME nor SUDO_USER set (#9543)
* chore(linux): Exclude environment.sh from build (#9553)

## 17.0.171 alpha 2023-09-01

* chore(linux): Add `clean` target to `rules` (#9531)

## 17.0.170 alpha 2023-08-31

* fix(common): builder_parse broke on option parameters (#9489)
* docs(linux): Explicitly specify emscripten version (#9527)

## 17.0.169 alpha 2023-08-30

* fix(linux): Fix exception in km-kvk2ldml (#9523)

## 17.0.168 alpha 2023-08-29

* refactor(linux): Fix warning (#9513)

## 17.0.167 alpha 2023-08-25

* feat(core): implement mapped set mapping (#9504)
* feat(resources): add pcm keyboard (#9508)

## 17.0.166 alpha 2023-08-24

* chore(common): Update crowdin strings for French (#9505)

## 17.0.165 alpha 2023-08-23

* feat(core): add display=id (#9484)

## 17.0.164 alpha 2023-08-22

* fix(common): don't crash on illegal unicodesets (#9492)

## 17.0.163 alpha 2023-08-21

* chore(web): Update keymanweb-osk.ttf to v. 3.0 (#9469)

## 17.0.162 alpha 2023-08-18

* fix(common): marker test (#9483)
* fix(windows): allow QR share box to grow to edge of default configuration parent window (#9472)
* chore(resources):  update CLDR to post-PRI (#9482)

## 17.0.161 alpha 2023-08-16

* feat(core) actual regex (#9440)

## 17.0.160 alpha 2023-08-14

* feat(core): limit max marker to 0xD7FF (#9448)

## 17.0.159 alpha 2023-08-11

* feat(core): marker implementation (#9405)

## 17.0.158 alpha 2023-08-09

* chore(linux): Disable Mantic builds on Launchpad (#9435)
* feat(windows): Use Keyboard Activated event API call to turn off caps lock (if required) (#9353)
* chore: stats script parameters (#9431)
* feat(developer): add 'default' property for longpress keys (#9432)
* chore(developer): improve kmc sentry reporting on fatal build errors (#9441)

## 17.0.157 alpha 2023-08-08

* fix(linux): fix memory leaks and method name (#9413)

## 17.0.156 alpha 2023-08-07

* epic: kmc-kmw KeymanWeb compiler in Typescript (#8954)

## 17.0.155 alpha 2023-08-05

* chore(developer): update markers per review (#9374)

## 17.0.154 alpha 2023-08-04

* chore(common): Update crowdin GHA to trigger daily (#9410)
* fix(web): forgot to target es5 when minifying polyfilled worker (#9409)
* chore(common): Update history.md with 16.0 stable entries (#9419)

## 17.0.153 alpha 2023-08-03

* docs(windows): Update OS requirement to Windows 10 (#9381)
* fix(web): maintenance of focus when changing keyboard via Toolbar UI (#9397)
* chore(linux): Remove Kinetic from GHA (#9399)
* chore(linux): Properly treat test builds with packaging GHA (#9400)

## 17.0.152 alpha 2023-08-02

* fix(developer): more wasm uset fixes (#9382)
* docs(windows): corrected nmake cmd for certificates (#9376)
* chore: add run-name to deb-packaging (#9386)
* chore: try another variable for reporting (#9388)
* chore(linux): Remove package build on Jenkins for Keyman 17 (#9380)
* docs(linux): Add build doc for Keyman Web and Android (#9383)

## 17.0.151 alpha 2023-08-01

* feat(developer) marker steps (#9364)
* feat(common): marker processing (#9365)
* chore(linux): Don't fail on parallel builds (#9368)
* fix(developer): fix breakage from emscripten 3.1.44 (#9375)
* docs(core): Document how to build Core on Linux (#9328)

## 17.0.150 alpha 2023-07-31

* chore(linux): Update debian changelog (#9358)
* chore(linux): Fix creation of PRs after uploading to Debian (#9360)

## 17.0.149 alpha 2023-07-30

* fix(core): Better range check for Uni_IsValid() (#9346)
* chore(core): update documentation in transform logic and processor (#9352)

## 17.0.148 alpha 2023-07-27

* feat(core): merge transform/reorder processing w/ u32 (#9293)
* chore(developer): make unknown vkey a hint, not error (#9344)
* chore(linux): Update supported Ubuntu versions (#9341)

## 17.0.147 alpha 2023-07-25

* chore(linux): Update debian changelog (#9327)

## 17.0.146 alpha 2023-07-24

* chore(deps-dev): bump word-wrap from 1.2.3 to 1.2.4 (#9314)

## 17.0.145 alpha 2023-07-21

* fix(linux): Fix logging (#9310)
* fix(windows): open pdf in an external browser (#9295)
* fix(linux): Fix installation of keyboards with lang tag `mul` (#9027)
* fix(web): allows registering precached keyboards (#9304)

## 17.0.144 alpha 2023-07-20

* refactor(linux): Use better way to get username (#9313)

## 17.0.143 alpha 2023-07-19

* spec(core): minor fix to tran spec (#9292)
* docs(linux): Update man page and remove do-nothing option (#9291)
* docs(linux): Update documentation (#9277)

## 17.0.142 alpha 2023-07-17

* chore(windows): disable wix compression for debug builds (#9074)

## 17.0.141 alpha 2023-07-15

* feat(core): implement reorder (#9223)
* chore(common): add BUILDER_STR_REF (#9248)
* feat(core): load reorder from kmx+, test case (#9260)

## 17.0.140 alpha 2023-07-14

* chore(common): add engine clause to package.json (#9008)

## 17.0.139 alpha 2023-07-13

* fix(common): set variables need to serialize elementstring (#9259)
* chore(linux): Add unit tests for dconf_util.py (#9215)
* refactor(linux): Refactor image loading (#9245)

## 17.0.138 alpha 2023-07-11

* chore: Update standards data (#9193)
* chore(linux): Trigger Linux integration tests (#9221)

## 17.0.137 alpha 2023-07-10

* chore: authenticate github request when triggering test builds (#9225)
* feat(web): logs enhanced debug info for error reports (#9217)
* fix(web): silences low-priority warning that shows up in iOS kbd init (#9206)

## 17.0.136 alpha 2023-07-07

* chore(common): Update crowdin strings for Kibaku (#9214)
* feat(core): kmxplus uset implementation in core (#9197)
* chore(linux): Add support for `--no-integration` flag to build files (#9212)
* spec(core): marker spec (#9196)

## 17.0.135 alpha 2023-07-06

* chore(common): improve coverage, fix todos (#9195)
* chore(ios): minor build-script cleanup (#9201)
* chore(linux): Allow to run tests without integration tests (#9192)
* fix(web): sentry sourcemapping round 2 - yesterday's 'fix' unfortunately doesn't upload the maps (#9199)
* chore(web): normalizes sourcemap paths for cleaner Sentry reports (#9200)
* feat(linux): Allow to install keyboard for multiple languages (#9017)

## 17.0.134 alpha 2023-07-05

* fix(linux): Fix packaging GHA (#9172)
* chore(ios): standardizes iOS build scripts (#9092)
* fix(web): fixes up Web's InlinedOSK manual test page (#9175)
* fix(web): re-establishes web srcmap uploading (#9179)
* refactor(linux): Code refactoring (#9129)
* refactor(linux): Extract methods (#9168)
* fix(linux): Fix vertical alignment of labels in About dialog (#9167)
* fix(linux): Add scrollbar to About dialog (#9166)
* fix(linux): Remove tags from package description (#9165)
* docs(linux): Update readme (#9190)
* feat(developer): uset in element string (#9144)
* fix(developer): improve coverage, add todos (#9170)
* chore(developer): refactor so KmnCompiler only created once within kmc (#9171)
* fix(common): support multichar escapes in element sets (#9173)
* chore(linux): Update docker builds (#9188)

## 17.0.133 alpha 2023-07-04

* feat(common): add segmenter for element strings (#9141)
* fix(windows): adds an extra row for Change Hotkey text label (#9149)
* fix(linux): Another attempt to fix packaging GHA (#9155)
* fix(linux): Fix packaging GHA (#9164)
* fix(linux): Fix packaging GHA (#9169)

## 17.0.132 alpha 2023-06-30

* docs(mac): Update requirements.md (#9137)
* fix(linux): Fix GHA package builds for non-Jammy dists (#9123)
* fix(linux): Fix packaging GHA (#9142)

## 17.0.131 alpha 2023-06-27

* feat(developer,common,core): ldml UTF-32 literals in binary (#9084)
* fix(web): fixes Web build zip artifact construction (#9096)

## 17.0.130 alpha 2023-06-26

* chore(deps): bump semver from 7.3.8 to 7.5.2 (#9085)
* epic: conversion of Keyman Engine for Web to ES modules (#8560)

## 17.0.129 alpha 2023-06-23

* chore(common): Update crowdin strings for Spanish (es-ES and es-419) (#9040)
* chore(android/app): Address some pre-launch accessibility report warnings (#9070)
* fix(windows): support kmxplus in mcompile (#9071)
* fix(core): fix ldml transforms (#9072)
* feat(common,developer,core): add uset section (#9049)

## 17.0.128 alpha 2023-06-22

* fix(windows): testhost improve error message (#9061)
* docs(windows): add more steps for clarity (#9063)

## 17.0.127 alpha 2023-06-21

* feat(core): implementation of non-regex components of tran (#9019)
* chore: enable common ci for web (#9047)

## 17.0.126 alpha 2023-06-19

* chore(mac): move from altool to notarytool (#9010)

## 17.0.125 alpha 2023-06-17

* fix(android/samples): Cleanup Sample/Tests apk's (#9030)
* chore(common): Bump crowdin github action to 1.3.2 (#9029)

## 17.0.124 alpha 2023-06-14

* fix(windows): qr code id uses keyboard name and not package name (#9002)
* refactor(linux): Extract widgets to separate classes and files (#9007)

## 17.0.123 alpha 2023-06-13

* chore(developer): support cp1252 'ansi' keyboards (#8988)
* fix(developer): keep kmc-kmn messages within namespace (#8999)
* fix(linux): Fix display of installed keyboard version (#8948)
* fix(linux): Fix creation of markdown help files for Linux (#9001)

## 17.0.122 alpha 2023-06-12

* refactor(developer): import kmcmplib errors into kmc-kmn (#8962)

## 17.0.121 alpha 2023-06-09

* fix(linux): Fix disabling of buttons (#8946)
* chore(developer): check for nodejs deps in kmc-kmn (#8961)
* spec(core): spec/impl for transform and vars (#8695)
* feat(developer): improve tran/bksp tests, other improvements (#8967)

## 17.0.120 alpha 2023-06-07

* chore(core): look for emcc.py, not emcc (#8934)
* chore(linux): Move some files to keyman-config (#8917)
* fix(linux): Fix title of reference page and links (#8939)
* fix(android/engine): Re-enable KMManager tests (#8940)

## 17.0.119 alpha 2023-06-06

* chore(developer): ensure kmcmplib messages are all UTF-8 (#8927)
* chore(linux): Fix lintian warnings (#8931)

## 17.0.118 alpha 2023-06-05

* chore(ios): replace fv cert (#8900)
* fix(core): Fix compilation if hotdoc is installed (#8912)
* feat(linux): Rename column and add tooltip (#8918)

## 17.0.117 alpha 2023-06-04

* chore(developer): verify long lines compile correctly (#8915)

## 17.0.116 alpha 2023-06-02

* feat(linux): Add column for installation location (#8897)
* chore(developer): verify kvks files and report errors (#8892)
* refactor(developer): rearrange kmcmplib interface source (#8899)
* refactor(developer): move filename consistency check to kmc (#8907)
* chore(developer): loadFile callback error check and optimization (#8908)
* chore(common): remove url module ref from common/web/types (#8914)

## 17.0.115 alpha 2023-06-01

* refactor(developer): complete fs move out of kmcmplib (#8882)
* fix(common): tweak pack/publish support for npm 9.5.1 and node 18.16.0 (#8894)
* feat(linux): Implement Options page and option to disable Sentry error reporting (#7989)

## 17.0.114 alpha 2023-05-31

* chore(developer): replace cwrap wasm bindings (#8857)
* chore(developer): refactor kmcmplib interfaces (#8870)
* chore(developer): move keyboard repo fixtures (#8874)
* chore(linux): Move build steps to build.sh (#8864)

## 17.0.113 alpha 2023-05-26

* docs(android): Update documentation for building Android on Linux (#8860)
* fix(android): Fix typo in build-utils.sh (#8859)
* refactor(developer): compiler interface part 1 for wasm (#8850)
* chore(developer): remove SetError macro (#8856)

## 17.0.112 alpha 2023-05-25

* refactor(developer): move file write out of WriteCompiledKeyboard and CompileKeyboard merge (#8796)

## 17.0.111 alpha 2023-05-24

* fix(android/app): Pathing for publish script (#8845)
* chore(developer): add test for missing .js file to kmc-package (#8846)
* feat(linux): Add dbus system service (#8509)
* chore(linux): Add missing dependency to GHA (#8849)

## 17.0.110 alpha 2023-05-22

* fix(common): ensure child builds don't rebuild dependencies unnecessarily (#8834)

## 17.0.109 alpha 2023-05-17

* fix(windows): add wrap-symbols to Text Editor Makefile (#8819)
* chore(linux): Make postinst script comply with Debian policy (#8810)
* chore(windows): remove legacy core and flag  ️ (#8593)

## 17.0.108 alpha 2023-05-16

* feat(windows): add text editor to the support makefile (#8750)
* feat(developer): verify keyboard versions in kmc-package (#8769)
* feat(developer): verify bcp47 tags are valid and minimal in kmc-package (#8778)
* feat(developer): verify at least one language in package (#8783)
* chore(developer): verify file types of content files in package (#8792)
* chore(developer): verify that package has at least a model or keyboard (#8793)
* chore(ios): Changes required for XCode 14.3 (#8746)

## 17.0.107 alpha 2023-05-15

* chore(linux): Fix installation build step on TC (#8784)
* refactor(android/engine): Consolidate updateSelection (#8739)

## 17.0.106 alpha 2023-05-12

* chore(developer): move package formats to common/web/types (#8729)
* feat(developer): add package validation (#8740)
* feat(developer): add validation of package filenames (#8751)
* feat(developer): validate content file names in packages (#8755)
* feat(developer): validate package name in compiler (#8757)
* chore(developer): rename Compiler and related classes (#8726)
* feat(developer): uset api from wasm! (#8716)

## 17.0.105 alpha 2023-05-11

* chore(common): Update crowdin strings for Amharic (#8748)
* chore(android/app): Cleanup build-play-store-notes script (#8749)
* chore(linux): Add build.sh for keyman-config (#8733)

## 17.0.104 alpha 2023-05-10

* chore(developer): rename kmc-keyboard to kmc-ldml (#8725)
* chore(linux): bring kmp.json for unit test up to spec (#8732)
* feat(common): add `builder_run_action` shorthand function (#8742)

## 17.0.103 alpha 2023-05-09

* fix(mac): `$CONFIG` not `$CONFIGURATION` in build path (#8724)
* chore(linux): Fix another problem uploading to launchpad (#8727)

## 17.0.102 alpha 2023-05-08

* chore(linux): Build for Lunar on all platforms (#8713)
* fix(linux): Fix linux tests (#8510)
* chore(linux): Fix uploading to launchpad (#8720)

## 17.0.101 alpha 2023-05-05

* chore(common): `set -eu` in build-utils.sh (#8698)
* fix(android/app): Resize OSK when resuming app from background (#8704)

## 17.0.100 alpha 2023-05-04

* fix(developer): warning fixes in kmcmplib (#8690)
* feat(developer): add uset api in kmcmplib (#8691)

## 17.0.99 alpha 2023-05-03

* chore(linux): Fix build script (#8705)

## 17.0.98 alpha 2023-05-02

* chore(common): run common and resources tests (#8692)

## 17.0.97 alpha 2023-04-28

* docs(windows): Add verify vs build tools (#8676)
* feat(developer): groundwork for support new transform, developer side (#8686)

## 17.0.96 alpha 2023-04-27

* chore(android): Update QRGen version for Java 11 (#8673)
* refactor(android/engine): Move toggleSuggestionBanner to KMKeyboard (#8679)
* refactor(android/engine): Use isKeyboardLoaded() on a consistent basis (#8680)
* chore(developer): test message correctness in compiler modules (#8664)
* chore(developer): add eslint devDependency for kmc (#8665)
* chore(developer): kmc-keyboard should not use any node-specific modules (#8666)
* fix(common): builder parse multi-param order (#8668)
* refactor(developer): use callbacks to avoid direct fs access in kmc-package (#8674)
* refactor(android/engine): Move shouldIgnore booleans to KMKeyboard (#8682)
* chore(linux): Build for Ubuntu 23.04 Lunar as well (#8683)

## 17.0.95 alpha 2023-04-26

* refactor(android/engine): Move currentBanner to KMKeyboard (#8671)
* chore(android/engine): Remove jvm args (#8675)
* fix(developer): kmc: fix jis/abnt2 scan codes (#8670)

## 17.0.94 alpha 2023-04-25

* fix(android/engine): Update InApp input connection (#8647)
* chore(common): Move the_99 test fixture keyboard (#8660)
* chore(android): Move namespace to Gradle files (#8667)

## 17.0.93 alpha 2023-04-24

* feat(common): kmx file reader (#8641)
* feat(developer): support FollowKeyboardVersion in kmc-package (#8642)
* chore(common): Update crowdin strings for Simplified Chinese (#8653)
* fix(common): builder - pass deps flags to child scripts (#8654)
* feat(developer): use CompilerEvent for messages in all of kmc (#8651)
* chore(developer): fixup deps for kmc modules (#8652)
* feat(core): ldml repertoire test, initial ICU integration (#8441)

## 17.0.92 alpha 2023-04-21

* fix(developer): kmc-package path reference (#8634)
* chore(developer): only run keyboards-repo tests for a full-test (#8635)
* refactor(developer): move comperr.h to kmn_compiler_errors.h (#8636)
* refactor(developer): kmc-model to use `CompilerEvent` (#8637)

## 17.0.91 alpha 2023-04-20

* fix(developer): kmc-kmn would abort build on warnings (#8617)
* fix(developer): kmc no longer emits double file extension for models (#8618)
* refactor(developer): move build activities (#8622)
* feat(developer): add warning options to kmc (#8623)
* feat(developer): use new message infrastructure in kmc-kmn (#8625)
* refactor(developer): move shared test helpers into common (#8626)
* feat(developer): add CompilerMessages support to kmc-package (#8627)
* feat(developer): warn if .kps file includes a non-binary .kvk file (#8628)
* chore(web): sets kmc-model as dependency of input-processor test action (#8629)
* fix(android/engine): Fix resetContext calls when selection changes (#8611)

## 17.0.90 alpha 2023-04-19

* chore(android/engine): Cleanup old SDK calls (#8612)
* fix(developer): package editor no longer loses RTL flag for LMs (#8606)

## 17.0.89 alpha 2023-04-18

* chore(developer): tweak build of extension list in kmc (#8597)
* docs(ios): Add help page for Settings (#8546)
* fix(developer): add missing dependencies (#8605)

## 17.0.88 alpha 2023-04-17

* docs(android): Update Linux dev environment for Java 11 (#8596)

## 17.0.87 alpha 2023-04-13

* refactor(developer): DRY kmc module references (#8577)
* chore(developer): kmc-kmn should build all baseline fixtures (#8578)
* fix(developer): kmc should not validate as well as compile keyboard (#8579)
* fix(developer): improve 'does not exist' error message (#8580)
* fix(common): consolidate npm publishing (#8587)
* chore(common): add missing deps and files for npm publish (#8588)

## 17.0.86 alpha 2023-04-12

* docs(windows): link to wiki architecture faq page (#8584)

## 17.0.85 alpha 2023-04-11

* chore(developer): AddWarning and SetError cleanup (#8571)
* fix(developer): add DRY_RUN variable to build.sh (#8576)
* fix(ios): fixes xcode-triggered debug, run builds (#8575)

## 17.0.84 alpha 2023-04-10

* epic: kmcompx - kmcmpdll cross-platform compiler (#7330)
* chore(android): Update projects to use Java 11 (#8543)
* docs: clarify dependency meanings in build-utils (#8567)
* fix(mac): fix corrupt installer (#8565)

## 17.0.83 alpha 2023-04-05

* fix(windows): use right instead of left alignment for popup buttons (#8561)

## 17.0.82 alpha 2023-03-31

* feat(developer): support jis and abnt2 (#8513)
* revert: #8549 "chore(web): merge master into feature-esmodule-web-engine (A17S9 end)  " (#8553)

## 17.0.81 alpha 2023-03-30

* feat(windows): simple text editor using Edit Control (#8391)

## 17.0.80 alpha 2023-03-29

* chore(linux): Remove dependency on `sudo` (#8535)
* chore(linux): Improve robustness of test cleanup (#8536)
* fix(linux): Remove warning after pressing Close button (#8538)

## 17.0.79 alpha 2023-03-28

* chore(linux): Revert "Run and ignore autopkgtests on s390x" (#8506)
* docs(linux): Update minimum required Ubuntu version (#8526)
* docs(linux): Update common questions (#8532)

## 17.0.78 alpha 2023-03-27

* chore(linux): Remove python3-raven dependency (#8507)
* refactor(android/engine): Make KMKeyboardJSHandler a normal class (#8494)

## 17.0.77 alpha 2023-03-25

* chore(common): use mac /usr/bin/stat rather than homebrew version (#8498)

## 17.0.76 alpha 2023-03-24

* chore(linux): Run and ignore autopkgtests on s390x (#8492)
* refactor(android/engine): Consolidate dispatchKey (#8483)

## 17.0.75 alpha 2023-03-23

* chore(common): define BUILDER_CONFIGURATION env var (#8496)

## 17.0.74 alpha 2023-03-22

* chore(windows): update sentry-native to 0.6.0 (#8464)
* chore(linux): Prevent building on s390x (#8477)
* fix(linux): Display error message for corrupt .kmp file (#8479)

## 17.0.73 alpha 2023-03-21

* refactor(android/engine): Consolidate insertText (#8438)
* chore(common): prevent multiple npm ci runs in child scripts (#8484)

## 17.0.72 alpha 2023-03-20

* chore(common): die early when --ci and --debug passed to test.sh (#8465)
* chore(common): Update crowdin strings for Kibaku (#8447)

## 17.0.71 alpha 2023-03-18

* fix(core): ldml update fr-azerty for VKEY mapping (#8434)

## 17.0.70 alpha 2023-03-17

* chore(android): Document builder script steps (#8449)

## 17.0.69 alpha 2023-03-16

* chore(web): Cleanup build echo (#8446)
* fix(developer): lm compiler handle missing line no in errors (#8444)
* fix(android/app): Temporarily disable Keyman browser (#8430)
* fix(android/engine): Add builder output for configure (#8442)
* chore(linux): Update debian changelog (#8452)

## 17.0.68 alpha 2023-03-15

* chore(common): add common test build configurations (#8431)
* fix(common): fix broken common/web/types cases (#8426)
* fix(developer/compilers): locks esbuild target detection for kmc building (#8437)
* chore(developer): ldml March 2023 update to tech-preview (#8412)
* fix(core): ldml more TODO-LDML fixes (#8436)
* chore(linux): Use dependency on core in ibus-keyman/build.sh (#8423)

## 17.0.67 alpha 2023-03-14

* chore(developer): Update copyright period in License.rtf (#8425)
* chore(common): support shorthand for build.sh (#8415)
* chore(common): build script performance improvements (#8416)
* chore(common): TS updates to non-sync'd packages, feature-esmodule merge conflict prevention (#8429)

## 17.0.66 alpha 2023-03-13

* refactor(android): Use builder scripts (#7407)

## 17.0.65 alpha 2023-03-12

* refactor(core): consolidate wasm defs (#8409)

## 17.0.64 alpha 2023-03-10

* feat(common): support single target for a dependency (#8404)
* chore(core): breadth-first build (#8400)
* refactor(common): Make build-help into helper functions (#8386)

## 17.0.63 alpha 2023-03-09

* fix(web): --no-minify, not --skip-minify (#8394)
* chore(core): Remove obsolete `--target-path` option (#8376)

## 17.0.62 alpha 2023-03-08

* chore(linux): Remove debug output from packaging GHA (#8372)
* chore: merge feature-ldml to master, again (#8377)
* chore(common): cleanup build.inc.sh echo (#8344)
* chore(common): remove vswhere (#8357)
* feat(linux): Add DBus method `SendText` (#8039)
* chore(core): Create both dynamic and static libs on Linux (#8375)
* chore(common): clean up `--debug` usage in build scripts (#8382)
* feat(common): builder.inc.sh inheritable options (#8384)

## 17.0.61 alpha 2023-03-03

* fix(android/app): Another attempt at fixing OSK rotation (#8353)

## 17.0.60 alpha 2023-03-02

* fix(android/engine): Fix various OSK bugs due to refactor (#8349)
* feat(core): Build both architectures for mac and generate a 'fat' library for them (#8342)
* fix(common): fixup npm publish (#8350)
* chore(linux): More GHA debugging (#8354)

## 17.0.59 alpha 2023-03-01

* fix(common): builder: skip dependency build for clean action (#8341)
* chore(web): fixup dependencies in web build script (#8332)
* chore(linux): More debug output (#8345)

## 17.0.58 alpha 2023-02-28

* chore(common): fix child build error handling (#8327)
* chore(linux): Improve and fix packaging GHA (#8328)
* fix(linux): Fix ibus-keyman/build.sh (#8329)
* epic: LDML keyboard support (#7054)
* fix(linux): Fix debian postinst script (#8294)

## 17.0.57 alpha 2023-02-26

* chore(android/app): Revert #8291 (#8314)

## 17.0.56 alpha 2023-02-24

* fix(linux): Fix upload in packaging GHA (#8308)
* feat(common): adds warning if described output does not exist (#8304)
* chore(linux): Improve detecting presence of test files (#8313)

## 17.0.55 alpha 2023-02-23

* chore(docs): update docs due to Mac Ventura (#8277)
* chore: remove greadlink from standard script prolog (#8288)
* chore: use builtin instead of dirname for perf (#8289)
* chore(common): stats script for planning (#7861)

## 17.0.54 alpha 2023-02-22

* fix(ios): allow duplicate languages in kmp file (#8125)
* fix(linux): Fix paths in upload action of packaging GHA (#8285)
* fix(android/app): Fix OSK width when rotating (#8291)

## 17.0.53 alpha 2023-02-21

* chore(windows): Remove old Kannada setup strings (#8257)
* fix(linux): Collect consecutive backspaces (#8274)
* chore(linux): Improve deb-packaging GHA (#8281)
* chore(linux): Cleanup GHA (#8282)

## 17.0.52 alpha 2023-02-20

* fix(windows): use min-width allow to expand (#8215)
* chore(web): minor cleanup of web/build.sh (#8254)
* fix(web): `--no-minify`, not `--skip-minify` (#8264)
* chore(web): Actually skip browser-stack for non-web builds (#8266)
* chore: cleanup some of shellHelperFunctions.sh (#8260)
* chore: consolidate script helper functions (#8261)
* chore(linux): Output results of API verification on stderr (#8241)
* chore(linux): Create draft PR after updating changelog (#8242)
* fix(linux): Checkout before  running apt-install (#8247)
* fix(linux): Fix bug in deb-packaging script (#8273)
* chore(developer): update bundled node to 18.14.1 LTS (#8268)
* chore(developer): ignore Esc key when compiling (#8263)
* docs: builder.inc.sh improved documentation around dependencies (#8173)
* chore(linux): Add build.sh to ibus-keyman  ️ (#8112)

## 17.0.51 alpha 2023-02-18

* chore(common): Update Fula strings for Sprint A17S6 (#8245)

## 17.0.50 alpha 2023-02-16

* feat(common): parent-child scripts for builder (#8198)
* fix(linux): Another iteration to fix packaging GHA (#8230)
* chore(common): updates Web & Web-related dependencies (#8196)
* fix(linux): Fix syntax for skipping upload in packaging GHA (#8236)

## 17.0.49 alpha 2023-02-15

* feat(linux): Pass PR# in the event type (#8221)
* feat(linux): Implement uploading of packages in packaging GHA (#8223)
* fix(linux): Fix GHA package build (#8225)
* fix(linux): Another fix for GHA package build (#8228)

## 17.0.48 alpha 2023-02-14

* fix(developer): Select BCP47 Code dialog inconsistencies (#8179)
* chore(common): automatically apply _builder_check_color (#8197)
* docs(linux): Update packaging documentation (#8202)
* chore(common): Report status if no platform needs build (#8210)
* chore(linux): Exclude autopkg tests on s390x (#8218)
* fix(linux): Fix path to symbols file in packaging GHA (#8209)

## 17.0.47 alpha 2023-02-10

* fix(linux): Fix autopkgtests (#8201)
* fix(linux): Fix Verify-API step of GHA (#8199)

## 17.0.46 alpha 2023-02-09

* fix(windows): add remove lang id uses correct keyboard id (#8186)
* chore(common): add `builder_term` function (#8187)
* chore(common): builder internal dependencies (#8188)
* chore(linux): Update changelog from Debian (#8192)
* feat(linux): Add debian packaging GHA (#7911)

## 17.0.45 alpha 2023-02-08

* chore(ios): update certificate (#8176)
* fix(linux): Fix autopkgtests (#8181)
* chore(common): Cleanup Kannada locale (#8182)
* chore(windows): Fix Portuguese UI language name (#8184)

## 17.0.44 alpha 2023-02-07

* chore(linux): Set test-helper script executable (#8178)
* chore(linux): Update debian changelog (#8156)

## 17.0.43 alpha 2023-02-06

* chore(linux): Build with meson instead of autotools  ️ (#8111)

## 17.0.42 alpha 2023-02-03

* chore(deps): bump http-cache-semantics from 4.1.0 to 4.1.1 (#8144)
* feat(developer): check for duplicated language codes in package editor and compiler (#8151)
* chore: Fail TC build if triggering Jenkins build fails (#8152)
* chore(linux): Fix lintian warnings (#8154)
* chore(core): Update meson version (#7882)
* refactor(android/engine): Consolidate Keyboard picker intent (#8163)

## 17.0.41 alpha 2023-02-02

* chore(web): main web build & test script rework to our common script format + nomenclature (#7474)
* change(web): conversion to standard filesystem layout:  source -> src, intermediate + release -> build (#7513)
* change(web): conversion to standard filesystem layout:  testing -> src/test/manual, unit_tests -> src/test/auto (#7515)
* change(web): conversion to standard filesystem layout:  tools -> src/tools (#7556)
* chore(web): updates build product references, extracts CI test-build scripts (#7831)
* feat(common): TS-based sourcemap remapping tool (#7894)
* feat(web): implements shell scripting for CI release-build configurations (#8001)

## 17.0.40 alpha 2023-02-01

* chore(developer): kmcmpdll debug src path (#8127)
* chore(common): Merge end of beta from Sprint A17S5 (#8135)
* refactor(android): Change com.tavultesoft.kmea package to com.keyman.engine (#7981)
* refactor(android/engine): Split KMInAppKeyboardWebViewClient to new class (#7983)
* refactor(android/engine): Refactor KMSystemKeyboardWebViewClient (#7993)

## 17.0.39 alpha 2023-01-31

* feat(windows): add additional registry keys to report (#8126)

## 17.0.38 alpha 2023-01-30

* chore(deps): bump ua-parser-js from 0.7.31 to 0.7.33 (#8109)
* fix(linux): Fix CI build (#8121)

## 17.0.37 alpha 2023-01-27

* chore(linux): Fix warnings (#8103)
* chore(linux): Remove legacy projects (#8102)
* Specify the path to the Debian package in the Vcs-Git header (#8104)

## 17.0.36 alpha 2023-01-26

* chore(common): Add crowdin strings for Kannada (#8074)

## 17.0.35 alpha 2023-01-25

* chore(common): revise tag variable name to VERSION_GIT_TAG (#8063)
* chore(common): Add crowdin strings for Czech (#8070)
* feat(linux): Improve uninstallation (#8011)

## 17.0.34 alpha 2023-01-24

* chore: merge beta to alpha, end of A17S4 (#8065)

## 17.0.33 alpha 2023-01-23

* chore: Xcode 14.2 update (#8015)

## 17.0.32 alpha 2023-01-20

* fix(ios): use mobile mode for keyboard download pages (#8042)
* docs(common/resources): Update configure step in Docker readme (#8034)
* fix(linux): JSON File missing after installation (#8040)

## 17.0.31 alpha 2023-01-19

* chore: git tag with release@semver (#8035)
* fix(linux): Properly set context after changing IP (#8026)
* chore(linux): use faster zero-length string check (#8037)
* chore(linux): log failures to `km_kbp_context_clear(context)` (#8036)
* chore(linux): Update recommended extension (#8038)

## 17.0.30 alpha 2023-01-17

* chore(linux): Fix vertical alignment of label (#8016)
* fix(common): update sentry release identifiers to support semver (#8031)

## 17.0.29 alpha 2023-01-16

* fix(linux): Fix crash (un-)installing shared keyboard (#8020)
* chore(linux): Don't report KeyboardInterrupt to Sentry (#8021)
* chore(linux): Update sample settings (#8018)
* feat(linux): Enhance tab completion in km-package-install (#8005)

## 17.0.28 alpha 2023-01-12

* feat(linux): Display error messages in the UI (#8006)
* bug(linux): Empty keyboard after failed installation (#8008)

## 17.0.27 alpha 2023-01-11

* chore(linux): Refactor completion script (#8002)

## 17.0.26 alpha 2023-01-10

* feat(linux): Add Back button to "Download Keyman Keyboards" dialog (#7994)
* feat(linux): List fonts in the uninstall confirmation dialog (#7995)

## 17.0.25 alpha 2023-01-09

* chore(linux): Remove unnecessary variable (#7988)

## 17.0.24 alpha 2023-01-06

* chore(common/resources): Add Docker readme (#7980)

## 17.0.23 alpha 2023-01-03

* fix(linux): add IBUS_HAS_PREFILTER ifdef to linux/ibus-keyman/tests (#7958)
* docs(linux): document fullbuild using docker (#7960)

## 17.0.22 alpha 2023-01-02

* chore: merge beta to master (A17S2) (#7948)
* chore(linux): Remove unused JENKINS parameter (#7920)

## 17.0.21 alpha 2022-12-21

* fix(windows): shutdown fix masked modal result (#7933)

## 17.0.20 alpha 2022-12-19

* chore(android/samples): Remove Sentry dependencies (#7901)
* chore: Revert "chore(common): Fix shellcheck warnings" (#7925)

## 17.0.19 alpha 2022-12-17

* fix(linux): Fix dependency of ibus-keyman (#7918)

## 17.0.18 alpha 2022-12-16

* chore(common): Fix shellcheck warnings (#7910)

## 17.0.17 alpha 2022-12-15

* chore: merge beta to master (#7885)
* chore(android): Update Gradle dependencies; targetSDKVersion to 33 (#7897)

## 17.0.16 alpha 2022-12-11

* refactor(linux): Update ibusimcontext.c (#7813)

## 17.0.15 alpha 2022-12-08

* chore(linux): Update vscode settings for Linux (#7876)

## 17.0.14 alpha 2022-12-07

* docs(linux): Move Linux documentation (#7868)
* chore(linux): Remove building for Bionic (#7873)
* chore(linux): Remove some shellcheck warnings (#7874)

## 17.0.13 alpha 2022-11-29

* fix(windows): lower case filenames for projects (#7837)
* refactor(linux): Refactor setting keyboard options (#7804)

## 17.0.12 alpha 2022-11-25

* chore(common): Merge beta from Sprint B16S2 to master (#7815)

## 17.0.11 alpha 2022-11-24

* refactor(linux): Use consts instead of strings (#7803)

## 17.0.10 alpha 2022-11-21

* fix: automatically merge changes to history back to master (#7777)
* chore: cleanup history (#7776)

## 17.0.9 alpha 2022-11-18

* chore: github issue template config.yml (#7751)
* chore(windows): tweak sentry client for cross-module messages (#7752)
* chore(android): Update Target SDK Version to 31 (#7761)

## 17.0.8 alpha 2022-11-17

* fix(android): bump version_code major calculation (#7736)

## 17.0.7 alpha 2022-11-13

* chore: fix TIER.md for master (#7704)

## 17.0.6 alpha 2022-11-12

* chore: merge beta to master B16S1 (#7693)
* chore(deps): bump minimatch from 3.0.4 to 3.1.2 (#7675)

## 17.0.5 alpha 2022-11-11

* chore(linux): Update debian changelog (#7681)

## 17.0.4 alpha 2022-11-10

* chore(deps): bump socket.io-parser from 4.0.4 to 4.0.5 (#7672)

## 17.0.3 alpha 2022-11-09

* fix(windows): Cleanup CEF more correctly in shutdown (#7661)

## 17.0.2 alpha 2022-11-01

* fix(android): Handle empty Play Store release notes (#7581)

## 17.0.1 alpha 2022-10-31

* fix(ios): iPad was not recognised as tablet device (#7563)
* feat(windows): configuration UI polish (#7206)
* chore: move to 17.0-alpha (#7577)
* chore: Move to 17.0 alpha

## 16.0.145 stable 2024-02-01

* chore(linux): Add support for loong64 architecture (#10108)
* chore(linux): Update debian changelog (#10122)
* fix(web): Fix null error with legacy keyboards (#10177)
* chore(ios): FV certificate key (#10229)
* fix(oem/fv): Add fv_hulquminum_combine and fv_kwadacha_tsekene (#10285)
* chore(developer): upgrade ngrok to v3 (#10360)
* chore(linux): Build with webkitgtk 4.1 instead of 4.0 (#10574)

## 16.0.144 stable 2023-11-30

* chore(linux): Update debian changelog (#10046)
* fix(android/app) Disable in-app Browser (#10077)

## 16.0.143 stable 2023-11-22

* chore(android): Update targetSdkVersion from 31 to 33 (#9823)
* chore(linux): Exclude environment.sh from build (#9814)
* chore(mac): move from altool to notarytool (#9896)
* fix(web): fixes doc-kbd display of default layer when it's not defined first (#9892)
* fix(ios): fv: replace Zip framework to prevent crash on startup (#9958)

## 16.0.142 stable 2023-10-20

* chore(linux): Update debian changelog (#9326)
* chore(linux): Remove obsolete dist from uploading to launchpad (#9340)
* chore(linux): Stop building Kinetic on Jenkins (#9354)
* chore(linux): Work around build failure on Debian mips64el (#9342)
* chore(linux): Update debian changelog (#9359)
* chore(android): Update targetSDKVersion to 33 (#9494)
* chore(linux): Remove workaround for mips64el (#9519)
* chore(linux): Workaround: Don't run Wayland tests (#9530)
* chore(linux): Add `clean` target to `rules` (#9532)
* fix(linux): Prevent exception if neither USER, LOGNAME nor SUDO_USER set (#9544)
* chore(linux): Ignore tests that fail on ba-jammy-64-ta (#9556)
* chore(ios): renew certificate (#9811)
* fix(mac): move keyboard menu items to main Input Menu from submenu (#9810)

## 16.0.141 stable 2023-07-25

* chore(mac): fix corrupt installer (#9332)

## 16.0.140 stable 2023-07-21

* chore(linux): Update debian changelog (#8451)
* chore(linux): Prevent building on s390x (#8476)
* fix(linux): Display error message for corrupt .kmp file (#8480)
* chore(linux): Run and ignore autopkgtests on s390x (#8491)
* chore(linux): Revert "Run and ignore autopkgtests on s390x" (#8505)
* chore(ios): Xcode 14 upgrade (#8487)
* fix(developer): package editor no longer loses RTL flag for LMs (#8607)
* chore(common): use mac /usr/bin/stat rather than homebrew version (#8925)
* chore(ios): replace fv cert (#8923)
* fix(core): Fix compilation if hotdoc is installed  (#8929)
* chore(linux): Move some files to keyman-config  (#8930)
* fix(core): Fix compiling with GCC 13 (#8932)

## 16.0.139 stable 2023-03-16

* chore: Fail TC build if triggering Jenkins build fails (#8141)
* chore(linux): Fix lintian warnings (#8153)
* chore(linux): Update debian changelog (#8155)
* chore(ios): update certificate (#8177)
* cherrypick(windows): Fix Portuguese UI language name (#8190)
* fix(linux): Fix autopkgtests (#8180)
* chore(linux): Update changelog from Debian (#8191)
* cherrypick(common): Cleanup Kannada locale (#8189)
* fix(windows): add remove lang id uses correct kbd (#8195)
* fix(linux): Fix autopkgtests (#8200)
* chore(linux): Exclude autopkg tests on s390x (#8217)
* chore(common): Update Fula strings for Sprint A17S6 (#8246)
* chore(windows): Remove old Kannada setup strings (#8258)
* fix(linux): Fix debian postinst script (#8295)
* fix(developer): lm compiler handle missing line no in errors (#8445)

## 16.0.138 stable 2023-02-01

* chore: add new developer build trigger (#8139)

## 16.0.137 stable 2023-02-01

* chore(common): Release 16.0

## 16.0.136 beta 2023-01-31

* chore(common): Update crowdin strings for Shuwa (Latin) (#8097)
* chore(common): Check in crowdin for Russian (#8072)
* chore(common): Check in crowdin for Ukrainian (#8073)
* chore(common): Add crowdin for Swedish (#8071)
* chore(common): Add crowdin strings for Czech (#8095)

## 16.0.135 beta 2023-01-30

* chore(linux): Include packaging path in Vcs-Git header (#8110)

## 16.0.134 beta 2023-01-25

* chore(common): Add crowdin strings for Kannada (#7970)

## 16.0.133 beta 2023-01-19

* fix(linux): Properly set context after changing IP (#8025)

## 16.0.132 beta 2023-01-18

* docs(windows): update screenshots and documentation for Keyman for Windows config (#8014)

## 16.0.131 beta 2023-01-16

* chore(linux): Don't report KeyboardInterrupt to Sentry (#8022)
* fix(linux): Fix crash (un-)installing shared keyboard (#8019)

## 16.0.130 beta 2023-01-09

* fix(windows): kmshell -ikl install language and enable keyboard (#7856)

## 16.0.129 beta 2023-01-06

* fix(android/engine): Add utility for localized strings (#7976)

## 16.0.128 beta 2022-12-22

* fix(developer): force ES3 code generation for LMs (#7927)

## 16.0.127 beta 2022-12-21

* fix(oem/fv/android): Fix keyboard version title (again) (#7941)
* fix(oem/fv/android): Add view permission of online help (#7909)
* fix(windows): modifer event is always serialized (#7935)

## 16.0.126 beta 2022-12-21

* fix(windows): shutdown fix masked modal result (#7932)

## 16.0.125 beta 2022-12-19

* fix(windows): no languages warning source changed (#7900)

## 16.0.124 beta 2022-12-18

* fix(oem/fv/android): Cleanup version title (#7908)

## 16.0.123 beta 2022-12-17

* fix(linux): Fix dependency of ibus-keyman (#7917)

## 16.0.122 beta 2022-12-14

* fix(windows): modify disable keyboard display (#7898)
* fix(developer): invalid charmap cell selection when updating to empty search result (#7887)

## 16.0.121 beta 2022-12-11

* chore(linux): Fix `make install` (#7886)

## 16.0.120 beta 2022-12-07

* chore(linux): Improve package build (#7875)

## 16.0.119 beta 2022-12-05

* chore: Change platform advocates (#7867)

## 16.0.118 beta 2022-12-04

* chore(linux): Update debian changelog (#7860)

## 16.0.117 beta 2022-12-01

* fix(developer): OnlineUpdate crash on shutdown (#7830)
* fix(developer): EncodeURL was not handling spaces (#7820)

## 16.0.116 beta 2022-11-30

* fix(android/app): Toggle keyboard update notifications for landscape and tablets (#7832)
* fix(android/engine): Append languageID for keyboard updates (#7807)
* fix(android/engine): Use FLAG_IMMUTABLE for PendingIntent for Android S+ (#7844)

## 16.0.115 beta 2022-11-29

* fix(windows): Cleanup CEF more correctly in shutdown (#7825)
* fix(windows): lower case filenames for projects (#7836)
* chore(linux): Update debian changelog (#7835)
* chore(linux): Fix upload-to-debian script (#7833)

## 16.0.114 beta 2022-11-28

* fix(mac): Caps processing was not consistent with core (#7795)
* fix(web): locks page scroll when scrolling lang menu (#7790)

## 16.0.113 beta 2022-11-26

* docs(android/app): Add note of haptic feedback to whatsnew (#7817)
* fix(developer): remove missing files from MRU list (#7821)

## 16.0.112 beta 2022-11-25

* fix: test builds should always run latest changes (#7806)
* fix(web): unit test breakages (#7797)

## 16.0.111 beta 2022-11-24

* fix(android/engine): Make it easier to test keyboard updates (#7781)

## 16.0.110 beta 2022-11-23

* fix(linux): Revert "fix(linux): Fix a warning" (#7783)
* fix: automatically merge changes to history back to master (#7780)
* chore(common): Update whatsnew for 16.0 (#7789)
* fix(web): inferred key text + spacebar caption scaling (#7741)
* feat(web): re-implementation of introductory globe-key help bubble (#7612)
* fix(android/engine): Fix logic error for updateKMP (#7799)
* fix(linux): Allow to build without patched ibus version (#7792)
* chore(linux): Update autotools configuration (#7793)
* docs(ios): Update help documentation about Install and Allow Full Access (#7791)
* fix(android/engine): Update versions of matching package and keyboard ID (#7740)

## 16.0.109 beta 2022-11-22

* fix(windows): arrow keys need to select next/prev item in options tab (#7756)
* fix(windows): buttons should be in list in Options tab (#7757)
* fix(android/engine): Add null check on kmp filename (#7787)
* fix(ios): make text size of in-app editor consistent with slider (#7782)
* fix(android/engine): Check if kmp.json is null (#7788)
* fix(mac): Enable right-alt/option key mapping (#7762)
* feat(linux): Add symbols file (#7742)

## 16.0.108 beta 2022-11-21

* fix(windows): backout ms button style for add lang pop up (#7765)
* fix(android/app): Only check Install Referrer API with Play Store installs (#7766)
* fix(windows): Fix text selectable configuration (#7758)
* feat(common/models): directional quote insensitivity (#7767)
* chore(windows): remove unused options from locales and UI (#7760)
* fix(developer): cast to prevent ERangeError for failed http reqs in CEF (#7775)
* chore(android): Update Target SDK Version to 31 (#7768)

## 16.0.107 beta 2022-11-18

* fix(windows): Download Keyboard blue footer (#7755)
* fix(windows): tweak elevate icon in Config dialog (#7754)
* fix(web): documentation keyboard style application + layout (#7694)
* fix(windows): fix checkbox and hint help button styles (#7764)
* chore(common): Update crowin strings for Fulfulde (#7715)
* fix(linux): Fix installation if shared kbd already installed (#7730)
* fix(linux): Fix a warning (#7746)
* chore(common): Update crowdin strings for Simplified Chinese (#7713)

## 16.0.106 beta 2022-11-17

* chore(windows): set platform core env explicitly (#7689)
* fix(android): bump version_code major calculation (#7735)
* fix(android/engine): Remove duplicate file suffix when installing .kmp (#7720)
* fix(linux): Ignore dbus exception when testing for fcitx (#7732)
* fix(linux): Fix lintian warnings (#7733)
* feat(linux): Add optional bcp47 parameter to km-package-install (#7731)

## 16.0.105 beta 2022-11-16

* chore(windows): add unit test for SetupCoreEnvironment (#7714)
* docs(linux): Add example for keyman:// URL format (#7726)
* fix(linux): set DEB_VERSION (#7727)
* fix(linux): Don't crash if `und` is specified without `fonipa` (#7728)

## 16.0.104 beta 2022-11-15

* chore(android/engine): Add more logging for keyboard restart (#7719)

## 16.0.103 beta 2022-11-14

* fix(windows): click in 1 pixel high box in Language Switch window caused crash (#7699)
* chore(windows): add sentry traces for unsolved crashes (#7703)
* fix(windows): handle timeout in downloads (#7709)
* chore(windows): Add safecall for internal CustomisationStorage API interface (#7707)
* chore(windows): include debug info in keyman build (#7710)
* fix(windows): check if permissions for thread to access file (#7207)
* refactor(linux): Use consts instead of strings (#7686)

## 16.0.102 beta 2022-11-12

* fix(windows): make sure IdStackWindows finalizes after Vcl.Forms (#7692)
* fix(android/app): Append tier to app name (#7674)
* chore(android/app): Remove debugging info on build.gradle (#7700)

## 16.0.101 beta 2022-11-11

* fix(windows): Improve shutdown robustness (#7677)
* fix(windows): base layout support in Keyman Core (#7667)
* fix(developer): URL parameters should be UTF-8 (#7631)
* fix(android/engine): Use IME package name if query permission denied (#7668)
* fix(linux): Fix keyboard icon in system tray (#7678)
* chore(linux): Update debian changelog  (#7682)
* chore(linux): Update Debian standards version (#7683)

## 16.0.100 beta 2022-11-10

* chore(linux): Update whatsnew (#7660)
* chore: Update history-and-versioning to match history (#7669)
* chore(linux): Update debian changelog (#7492)
* chore(android/engine): Don't show Toast errors on stable tier (#7676)
* fix(web): Add additional checks on className.indexOf (#7662)

## 16.0.99 beta 2022-11-09

* fix(web): Add polyfill for Array.includes() (#7646)
* fix(web): Add polyfill for Array.findIndex() (#7652)
* fix(developer): handle edge cases for app sources (#7654)
* fix(developer): handle shutdown cleanup more cleanly (#7655)
* fix(developer): handle crash reports without Keyman Engine installed (#7657)
* fix(developer): handle unpaired surrogate (#7659)
* fix(core): `save()` should be on internal kmx action queue (#7644)
* fix(core): emit keystroke was writing to wrong queue (#7650)

## 16.0.98 beta 2022-11-08

* fix(android/engine): Check temporary kmp file is valid (#7620)
* fix(developer): set keyman.osk immediately after switching device type (#7624)
* fix(developer): update monaco editor to 0.15.6 (#7626)
* chore(developer): tidy up compile hints (#7633)
* fix(developer): handle EEncodingError loading text file (#7635)
* fix(developer): prevent exception in charmap when unable to focus (#7640)
* fix(developer): disable Keyboard|Compile if no project loaded (#7642)
* fix(web): Handle undefined keyId in multi-tap (#7648)
* fix(developer): kmlmc has runtime dependency on keyman-version (#7651)
* fix(web): avoid reset for touch system keyboard (#7638)
* fix(developer): improve support for inferred key cap text (#7617)

## 16.0.97 beta 2022-11-04

* fix(android/app): Don't show "Get Started" after setting Keyman as default system keyboard (#7587)
* fix(windows): include debug info in kmshell build (#7600)
* fix(developer): tweak captions for consistency (#7607)
* fix(mac): replace white background keyman system menu icon with transparent one (#7610)

## 16.0.96 beta 2022-11-03

* fix(developer): Make Details and Build tabs scrollable (#7593)
* fix(developer): support gif preview images in Package Editor (#7594)
* fix(developer): handle high-bit-set HKL correctly (#7601)
* fix(android/app): Add check if bundle to PackageActivity is null (#7603)
* fix(common): always define dry_run variable (#7604)
* feat(web): Custom layer targets for Shift double-tap (#7608)
* fix(android/engine): Handle parsing empty JSONArray as JSONObject (#7611)

## 16.0.95 beta 2022-11-02

* fix(developer): Use US base layout for debugger (#7531)
* fix(developer): ensure kmlmp handles paths (#7580)
* fix(developer): prevent repeated `begin` statements (#7583)
* fix(developer): remove ancient samples (#7586)
* fix(developer): generate a default icon for basic keyboard projects (#7584)
* fix(developer): avoid reformatting unchanged system stores (#7585)
* fix(developer): publish `@keymanapp/keyman-version` to npm (#7595)

## 16.0.94 beta 2022-10-31

* fix(ios): iPad was not recognised as tablet device (#7563)
* feat(windows): configuration UI polish (#7206)

## 16.0.93 beta 2022-11-01

* chore: move to 16.0 beta

## 16.0.92 alpha 2022-10-31

* fix(android/engine): Add FLAG_ACTIVITY_NEW_TASK flag for resource update (#7572)
* change(common/models): prevent integration test flakiness due to prediction-search timeouts (#7571)
* chore: make git pre-commit hook executable (#7573)
* fix(web): bulk renderer update (#7516)

## 16.0.91 alpha 2022-10-28

* fix(developer): handle LControl being set by Windows when AltGr pressed (#7530)
* chore(windows): convert `wm_keyman_keyevent` and `wm_keyman_modifierevent` to private `WM_USER` messages (#7546)
* chore(common): Update help references from 15.0 to 16.0 (#7558)
* fix(core): remove `KMN_API` from callback spec (#7520)
* fix(developer): exclude `nocaps` from layer id modifier components (#7553)
* fix(web): model language ids now case-insensitive (#7557)
* fix(android/engine): Dismiss subkeys when hiding keyboard (#7555)

## 16.0.90 alpha 2022-10-27

* chore(common): update configure-repo.sh to include pre-commit hook (#7545)
* chore(common): Add crowdin strings for Dutch (#7498)

## 16.0.89 alpha 2022-10-26

* fix(core): Add missing include files (#7504)
* fix(linux): Improve setting context (#7084)
* feat(windows): Change "None" to "No Hotkey" matching new config (#7539)
* feat(web): keyboard layout spec now allows numeric width, pad, sp (#7486)
* chore(common): update actions to node16 (#7525)
* chore(web): KMW dev-tools reorganization, cleanup, script updates (#7463)
* fix(web): allow `isChiral()` to accept string name parameter of keyboard (#7544)
* fix(web): ensures keyboard does not change underneath OSK keystroke (#7543)

## 16.0.88 alpha 2022-10-25

* fix(common): ignore deps when formatting help (#7514)
* refactor(linux): Comment unused methods (#7501)
* chore(linux): Rename internal method to `commit_string` (#7502)
* chore(linux): call `reset_context` instead of `...focus_in` (#7503)
* chore(common): Update crowdin strings for Amharic (#7522)
* chore(android): Update help whatsnew for 16.0 (#7507)
* chore(common): Update existing crowdin strings for 16.0 (#7524)

## 16.0.87 alpha 2022-10-24

* fix(web): touch-platform language menu initial scrolling + index scrolling (#7488)

## 16.0.86 alpha 2022-10-21

* fix(developer): Handle invalid graphics in icon editor (#7431)
* chore(common): move to new GitHub issue form templates (#7470)
* change(web): removes unused config, help buttons from OSK (#7484)
* fix(windows): ensure all modifier events go to serialized queue (#7449)
* chore(common): automatically run internal dependencies on builder (#7464)
* fix(mac): Prompt for required permissions at start time (#7354)

## 16.0.85 alpha 2022-10-20

* chore(android/engine): Remove help bubble tool tip (#7473)
* fix(linux): Fix upload script (#7494)
* docs(linux): Update sample vscode settings (#7491)
* fix(android/engine): Dismiss key preview and subkeys on globe action (#7472)
* refactor(linux): Cleanup whitespace (#7500)
* chore(linux): Improve debug output (#7499)

## 16.0.84 alpha 2022-10-19

* fix(developer): Handle hints and warnings cleanly (#7372)
* fix(developer): Ctrl key to select key was conflicting with shortcuts (#7430)
* modify(android/engine): Stop logging if keyboard index not found (#7415)
* chore(android/engine): Update Keyman references in android-host.js (#7466)

## 16.0.83 alpha 2022-10-18

* docs(linux): Update readmes (#7468)

## 16.0.82 alpha 2022-10-17

* change(web): now utilizes 'inputMode="none"' on supported touch devices (#7343)
* chore(web): drops alignInputs, hideInputs (#7360)
* fix(web): better element inputmode management (#7395)
* chore(common): `--debug` standard flag for builder (#7462)
* fix(ios):  Allow Full Access enabled to fix ios 16  invisible keyboard (#7459)

## 16.0.81 alpha 2022-10-16

* fix(developer): manual attachment for kmw in server (#7457)

## 16.0.80 alpha 2022-10-14

* feat(common): builder script dependency and legacy script report (#7453)
* chore(linux): Adjust Linux specific files in `.gitignore` (#7451)

## 16.0.79 alpha 2022-10-13

* chore(common/models, web): more script updates (#7392)
* feat(common): builder dependency support (#7438)
* chore(common): further tweaks to builder scripts (#7439)

## 16.0.78 alpha 2022-10-12

* fix(linux): Fix reordering of output (#7079)
* fix(linux): Fix make install (#7434)
* chore(linux): Add Node.js to the docker container (#7435)

## 16.0.77 alpha 2022-10-10

* fix(web): possible error on change of context to a contextEditable (#7359)
* chore(web): unit test polishing pass (#7381)

## 16.0.76 alpha 2022-10-06

* chore(linux): Cleanup unnecessary surrogate check (#7396)

## 16.0.75 alpha 2022-10-05

* chore(common): builder script maintenance (#7405)

## 16.0.74 alpha 2022-10-04

* chore(android/engine): Display Toast notifications on Sentry errors (#7390)
* fix(android/engine): Dismiss subkeys on multi-touch (#7388)

## 16.0.73 alpha 2022-10-03

* fix(developer): handle invalid kps xml when loading project view (#7371)

## 16.0.72 alpha 2022-09-30

* fix(web): Only unhighlight suggestion if there's a pending one (#7383)
* refactor(common): Split builder utilities to separate script (#7365)

## 16.0.71 alpha 2022-09-29

* fix(developer): generate valid js when using unquoted digits in stores (#7369)
* fix(developer): ensure backslash in paths passed to kmcomp (#7370)
* fix(web): Update spacebar caption when refreshing layout (#7348)
* fix(common/web): mock deadkey handling after rules manipulating context (#7345)

## 16.0.70 alpha 2022-09-26

* fix(android): Add language name when installing default lexical-model (#7347)
* chore(common): Add 15.0 stable entries to HISTORY.md  (#7350)

## 16.0.69 alpha 2022-09-21

* fix(android/engine): Check listview for setting keyboard selection (#7325)
* refactor(common/web): reworks low-level module build scripts  ️ (#7319)
* refactor(common): `builder` script tweaks - "has action" vs "start action"  ️ (#7333)
* feat(common): builder trap-functionality testing  ️ (#7334)
* chore(common): builder extra params support (#7339)

## 16.0.68 alpha 2022-09-20

* chore(web): update `got` dependency for regression tests (#7315)
* chore(developer): update multer dependency for server (#7314)
* chore(developer): update node-windows-trayicon to update node-gyp to 9.1.0 (#7321)

## 16.0.67 alpha 2022-09-19

* fix(common/models): blocks full-text "corrections"  ️ (#7241)
* change(web): better fat-finger key weighting  ️ (#7242)
* fix(web): fixes unintended auto-acceptance of suggestion after reverting (#7305)
* feat(common/models): wordbreaker customization (#7279)
* fix(common/models): fixes reference dropped by git merge (#7313)
* test(android): Add final keyboard to test K_ENTER rule (#7303)
* chore(common): Update to Unicode 15.0 (#7302)
* chore(common): update auto labeler configuration (#7316)
* chore(common): make scripts executable and add pre-commit test (#7317)
* chore(deps): bump nanoid and mocha (#7307)
* chore(common): remove unused ncc dependency (#7318)

## 16.0.66 alpha 2022-09-17

* chore: improve auto labeling (#7288)

## 16.0.65 alpha 2022-09-16

* chore(linux): Remove unused IBusLookupTable (#7296)

## 16.0.64 alpha 2022-09-15

* fix(android/engine): Switch keyboard if uninstalling current one (#7291)
* fix(common/models): fixes quote-adjacent pred-text suggestions (#7205)
* fix(common/models): max prediction wait check (#7290)

## 16.0.63 alpha 2022-09-14

* chore(linux): Update debian changelog (#7281)
* fix(linux): Fix ignored error (#7284)

## 16.0.62 alpha 2022-09-13

* fix(developer): hide key-sizes when in desktop layout in touch layout editor (#7225)
* fix(developer): show more useful error if out of space during Setup (#7267)

## 16.0.61 alpha 2022-09-12

* docs(windows): add steps for using testhost debugging (#7263)
* fix(developer): compiler mismatch on currentLine (#7190)
* fix(developer): suppress repeated warnings about unreachable code (#7219)
* chore:  try disabling concurrency for browserstack tests (#7258)
* chore(web): disable browserstack on non-web-specific builds (#7260)

## 16.0.60 alpha 2022-09-10

* fix(web): enhanced timer for prediction algorithm (#7037)
* chore(core): fixup km_kbp_event docs (#7253)
* fix(windows) Update unit tests for tsf bkspace (#7254)
* fix(android): Standardize language ID in language picker menu (#7239)

## 16.0.59 alpha 2022-09-09

* chore: use keyman.com instead of keyman-staging.com (#7233)
* feat(core): add `km_kbp_event` API endpoint (#7223)
* fix(windows): Delete both code units when deleting surrogate pairs in TSF-aware apps (#7243)

## 16.0.58 alpha 2022-09-08

* fix(common/models): reconnects unit tests for worker-internal submodules (#7215)
* feat(common/models): extra Unicode-based wordbreaker unit tests (#7217)

## 16.0.57 alpha 2022-09-07

* fix(android/engine): Fix Backspace key to delete without errant subkeys (#7156)

## 16.0.56 alpha 2022-09-05

* chore(linux): Fix ibus-keyman.postinst script (#7192)

## 16.0.55 alpha 2022-09-03

* chore(android): Reduce Toast notification noise (#7178)

## 16.0.54 alpha 2022-08-30

* chore(core): rename json.hpp to jsonpp.hpp (#6993)
* chore(developer): remove unused dependencies from KeymanWeb compiler (#7000)
* chore: update core/ label (#7138)
* chore(linux): Update debian changelog (#7145)
* fix(linux): Allow downgrades for installing build deps (#7147)
* chore(core): emcc off path for linux (#7149)

## 16.0.53 alpha 2022-08-29

* feat(linux): Dockerfile for linux builder (#7133)

## 16.0.52 alpha 2022-08-26

* fix(android/engine): Lower the max height for landscape orientation (#7119)
* fix(linux): Remove wrong `ok_for_single_backspace` method (#7123)

## 16.0.51 alpha 2022-08-24

* fix(android): verify browser before starting activity (#7001)
* chore: Change platform advocates per discussion (#7096)
* fix(windows): Add invalidate context action to non-updatable parse (#7089)
* feat(common): add parameter variable support to `builder_` functions (#7103)

## 16.0.50 alpha 2022-08-23

* chore(core): Remove obsolete python keyboardprocessor (#7094)

## 16.0.49 alpha 2022-08-22

* fix: remove saving and restoring context kbd options (#7077)
* chore(deps): bump @actions/core from 1.8.2 to 1.9.1 (#7087)

## 16.0.48 alpha 2022-08-16

* fix(web): button, float init timer cleanup (#7036)

## 16.0.47 alpha 2022-08-15

* chore(core): refactor kmx_file.h to common (#7062)
* fix(developer): stack overflow when compiling non-web keyboard (#7031)
* fix(developer): prevent crash attempting to compile ansi keyboard (#7033)
* chore(linux): Refactor `jenkins.sh` (#7060)
* fix(developer): compiler emitting garbage for readonly groups (#7013)

## 16.0.46 alpha 2022-08-12

* docs(core): cleanup in keyboardprocessor.h (#7065)

## 16.0.45 alpha 2022-08-09

* feat(linux): Replace deprecated distutils (#7051)
* chore(linux): Adjust Linux source package to restructured code (#7056)

## 16.0.44 alpha 2022-08-08

* chore(linux): Update debian changelog (#7041)

## 16.0.43 alpha 2022-08-05

* chore(web): centralizes web-based modules' CI unit test configurations (#7024)
* refactor(web): converts test_utils funcs to Promise use (#7027)
* chore(core): get wasm core building again (#7023)
* chore(common): build-utils.sh minor tweaks (#7018)
* change(web): rotation polish - OSK reload no longer needed  ️ (#6787)
* change(web): updates mobile screen dimension detection code  ️ (#6979)

## 16.0.42 alpha 2022-08-01

* feat(developer): add languageUsesCasing to Model Editor (#7008)
* fix(developer): increase maximum file size for server (#7003)
* fix(developer): rebuilding a model would not refresh it in server (#7015)
* chore(common): GitHub issue template tweaks (#7022)

## 16.0.41 alpha 2022-07-29

* chore(common): builder script `:target` and `--option` support (#6986)
* chore(linux): Add code comment (#7009)

## 16.0.40 alpha 2022-07-28

* chore(linux): Add script to upload to debian (#6997)

## 16.0.39 alpha 2022-07-27

* chore(linux): Update debian changelog (#6996)

## 16.0.38 alpha 2022-07-26

* feat(developer): Rework Touch Layout Editor to support flick and multitap (#6884)
* fix(developer): improve error message for multi-part key ids on <15.0 (#6989)
* fix(android): rework longpress movement trigger (#6984)
* chore(linux): Update changelog (#6988)

## 16.0.37 alpha 2022-07-22

* fix(linux): Catch PermissionError exception (#6968)
* chore(linux): Update Debian changelog  (#6973)
* chore(linux): Add support for Ubuntu 22.10 "Kinetic Kudu" (#6975)
* fix(developer): reduce timeouts if server shut down (#6943)
* fix(linux): Implement refresh after keyboard installation (#6956)

## 16.0.36 alpha 2022-07-20

* chore(linux): Code cleanup (#6957)
* fix(linux): Another attempt at fixing postinst script (#6960)
* fix(linux): Fix uninstallation when using fcitx5 (#6963)

## 16.0.35 alpha 2022-07-19

* fix(common): Fix `delete` (#6965)

## 16.0.34 alpha 2022-07-15

* fix(developer): set contextDevice in web debugger  ️ (#6906)
* fix(web): postkeystroke processing should ignore key-event source (#6901)
* fix(web): layer-setting ops should not trigger for hardware keystroke processing  ️ (#6902)
* fix(developer): kmdecomp virtual character key output (#6940)
* fix(developer): crash on exit when checking for updates (#6941)
* fix(developer): crash when switching back a tab (#6942)
* chore(web): remove invalid warning msg (#6950)

## 16.0.33 alpha 2022-07-14

* feat(developer): Option to skip embed of compiler version (#6920)
* feat(developer): compiler regression support (build kmcomp-$version.zip during release build) (#6918)
* chore(ios): updates core internals for the in-app browser (#6838)

## 16.0.32 alpha 2022-07-13

* fix(ios): ignore CFBundleShortVersionString (#6934)

## 16.0.31 alpha 2022-07-12

* fix(developer): QR Code for Package Editor had wrong path (#6926)

## 16.0.30 alpha 2022-07-11

* fix(web): maps touch-layout chiral alt, ctrl to non-chiral when non-chiral keyboard is active (#6808)

## 16.0.29 alpha 2022-07-08

* fix(web): ncaps rules not matching on touch (#6911)

## 16.0.28 alpha 2022-07-08

* chore(android): disable sendError for js errors (#6905)
* fix(developer): add no-ops for symbol builds for components (#6908)

## 16.0.27 alpha 2022-07-07

* fix(web): improve `console.error()` reporting (#6890)

## 16.0.26 alpha 2022-07-06

* fix(developer): Show full version number (#6866)
* chore(developer): rename developer/js to developer/kmlmc (#6630)
* feat(developer): .keyman-touch-layout schema for 15.0 (#6856)
* feat(developer): add flick and multitap to touch layout schema and doc (#6877)
* fix(linux): Fix post-install script (#6894)

## 16.0.25 alpha 2022-07-05

* chore(web): update readme (#6891)

## 16.0.24 alpha 2022-07-02

* fix(common): trigger builds correctly for stable builds (#6847)
* fix(windows): show full version number (#6867)

## 16.0.23 alpha 2022-06-29

* fix(web): clears repeating bksp on keyboard reload  ️ (#6786)
* fix(web): null-guard for refreshLayout (#6854)

## 16.0.22 alpha 2022-06-28

* fix(ios): eliminates conditional height for banner image (#6855)

## 16.0.21 alpha 2022-06-27

* chore(ios): modernizes app-help's internals (#6836)
* fix(linux): Deal with non-existing files (#6840)
* fix(linux): Catch KeyboardInterrupt (#6841)

## 16.0.20 alpha 2022-06-26

* chore(web): shorten check/web/file-size check message (#6780)

## 16.0.19 alpha 2022-06-24

* change(web): drops old IE-related handling code (#6557)
* chore(deps): bump got from 11.8.3 to 11.8.5 (#6821)
* chore(ios): log script actions and fixup export settings to avoid version being clobbered (#6835)

## 16.0.18 alpha 2022-06-23

* chore(core): move common/core/desktop/ to core/ (#6629)

## 16.0.17 alpha 2022-06-22

* chore(web): updates error-reporting lib uploader (#6818)
* fix(web): fixes doc-keyboard generation (#6793)

## 16.0.16 alpha 2022-06-21

* fix(web): key tip constraint logic requires .bottom CSS (#6784)
* fix(ios): overzealous error about missing banner image (#6807)
* chore(linux): Update Debian changelog (#6810)

## 16.0.15 alpha 2022-06-18

* fix(web): crash on custom modifier keys (#6790)
* chore: add trigger definitions for stable-15.0 and fixup force param for increment-version.sh (#6776)

## 16.0.14 alpha 2022-06-17

* chore(web): add 'index.html' to parent links in test pages (#6791)

## 16.0.13 alpha 2022-06-15

* chore: beta to master merge A16S4 (#6771)

## 16.0.12 alpha 2022-06-14

* chore(web): replace lerna with npm workspaces and ts projects (#6525)
* fix(common): update increment-version for new tsc, node versions (#6762)

## 16.0.11 alpha 2022-05-27

* chore: A16S2 beta to master (#6670)
* chore(ios): FV certificate hash (#6674)

## 16.0.10 alpha 2022-05-15

* chore: beta to master A16S1 (#6614)

## 16.0.9 alpha 2022-04-29

* chore(common): merge beta to master B15S7 (Part 2) (#6579)

## 16.0.8 alpha 2022-04-17

* chore: merge beta to master B15S7 (#6537)

## 16.0.7 alpha 2022-04-07

* chore: beta to master B15S6 (#6490)
* fix(ios): use CFBundleShortVersionString for version (#6496)

## 16.0.6 alpha 2022-04-05

* chore(deps): bump ansi-regex from 3.0.0 to 3.0.1 in /web/testing/regression-tests (#6470)
* chore(deps-dev): bump minimist from 1.2.5 to 1.2.6 in /web/testing/regression-tests (#6471)
* chore(deps): bump minimist from 1.2.5 to 1.2.6 in /developer/server (#6472)

## 16.0.5 alpha 2022-04-04

* chore(common): beta to master B15S6 (#6467)

## 16.0.4 alpha 2022-03-22

* chore: B15S4 beta->alpha (#6402)

## 16.0.3 alpha 2022-03-16

* chore: merge beta changes to alpha B15S4 (#6376)
* chore(common/core/desktop): Improve meson build file  ️ (#6183)

## 16.0.2 alpha 2022-03-03

* chore(deps-dev): bump karma from 6.3.14 to 6.3.16 in /web/testing/regression-tests (#6322)

## 16.0.1 alpha 2022-03-02

* chore(common): move to 16.0 (#6318)

## 15.0.270 stable 2022-09-12

* chore(linux): Add support for Ubuntu 22.10 "Kinetic Kudu" (#7143)
* chore(linux): Update debian changelog (#7144)
* fix(android/engine): Cleanup list of subkeys when dismissing window (#7176)
* chore(linux): Fix ibus-keyman.postinst script (#7208)
* fix(windows): Delete both code units when deleting surrogate pairs in TSF-aware apps (#7256)
* fix(developer): compiler mismatch on currentLine (#7191)
* fix(developer): suppress repeated warnings about unreachable code (#7265)

## 15.0.269 stable 2022-08-29

* chore(linux): Update debian changelog (#7040)
* feat(linux): Replace deprecated distutils  (#7052)
* fix(developer): compiler emitting garbage for readonly groups (#7014)
* chore: Change platform advocates per discussion (#7114)
* fix(windows): remove saving and restoring context kbd options (#7107)
* fix(windows): Add invalidate context action to non-updatable parse (#7108)
* fix(android/engine):  Lower the max height for landscape orientation (#7128)

## 15.0.268 stable 2022-08-04

* chore(linux): Update debian changelog (#6995)
* fix(developer): increase maximum file size for server (#7004)
* fix(developer): rebuilding a model would not refresh it in server (#7016)
* chore(linux): Add script to upload to debian (#7017)
* fix(developer): stack overflow when compiling non-web keyboard (#7034)
* fix(developer): prevent crash attempting to compile ansi keyboard (#7035)

## 15.0.267 stable 2022-07-26

* fix(developer): QR Code for Package Editor had wrong path (#6927)
* fix(ios): ignore CFBundleShortVersionString (#6935)
* fix(web): context-only rule effects, set(&layer) from physical keystrokes  ️ (#6949)
* chore(web): remove invalid warning msg (#6951)
* fix(common): Fix `delete`  (#6966)
* fix(linux): Another attempt at fixing postinst script  (#6961)
* fix(linux): Fix uninstallation when using fcitx5  (#6964)
* fix(linux): Catch PermissionError exception  (#6969)
* chore(linux): Update Debian changelog (#6972)
* fix(developer): kmdecomp virtual character key output (#6945)
* fix(developer): crash on exit when checking for updates (#6946)
* fix(developer): crash when switching back a tab (#6947)
* fix(developer): reduce timeouts if server shut down (#6948)
* fix(android): rework longpress movement trigger (#6992)
* chore(linux): Update changelog (#6987)
* fix(developer): improve error message for multi-part key ids on <15.0 (#6990)

## 15.0.266 stable 2022-07-08

* fix(linux): Fix post-install script  (#6895)
* fix(web): improve `console.error()` reporting (#6904)
* fix(web): ncaps rules not matching on touch (#6913)

## 15.0.265 stable 2022-07-04

* fix(common): trigger builds correctly for stable builds (#6879)
* fix(web): post-keystroke processing after use of pred. text selection (#6886)

## 15.0.264 stable 2022-07-01

* fix(ios): eliminates conditional height for banner image (#6857)
* fix(web): clears repeating bksp on keyboard reload  ️ (#6863)
* fix(web): better stability during transient OSK load state (#6861)
* fix(developer): readonly groups should never emit output (#6873)
* fix(web): Maintain separate Caps Lock states for touch and physical (#6874)

## 15.0.263 stable 2022-06-24

* chore(ios): add manageAppVersionAndBuildNumber=false (#6837)

## 15.0.262 stable 2022-06-23

* fix(web): key preview styling, positioning issues (#6795)
* chore(linux): Update Debian changelog (#6809)
* fix(linux): Deal with non-existing files (#6812)
* fix(linux): Catch KeyboardInterrupt (#6814)
* fix(ios): blocks known non-error "error" report (#6816)
* fix(web): fixes doc-keyboard generation (#6817)
* chore(web): updates error-reporting lib uploader (pseudo- ) (#6819)

## 15.0.261 stable 2022-06-18

* fix(web): crash on custom modifier keys (#6789)

## 15.0.260 stable 2022-06-15

* chore(common): Release version 15.0

## 15.0.259 beta 2022-06-10

* fix(android/engine): Disable haptic feedback on hardware keystrokes (#6671)
* fix(windows): Install TIP crash with inconsistent bcp47tag (#6727)
* change(web): adjusts minimum distance needed for quick-display of subkeys (#6714)
* change(web): browser test timeout tweaks (#6718)

## 15.0.258 beta 2022-06-06

* chore: add 14.0 entries to beta HISTORY.md (#6705)
* chore(web): switch on additional BrowserStack reporting (#6707)
* fix(web): fixes OSK-interaction issues in Float, Toolbar UI modules (#6689)

## 15.0.257 beta 2022-06-03

* fix(developer): kmcomp package compiler race condition (#6696)
* fix(developer): install keyboard call should quote kmshell.exe path (#6691)

## 15.0.256 beta 2022-06-01

* chore(android,ios,mac): Update crowdin strings for Spanish (Latin America) (#6650)

## 15.0.255 beta 2022-05-31

* fix(web): osk shrinkage from rounding (#6658)

## 15.0.254 beta 2022-05-30

* fix(ios): missed i18n site (#6667)
* fix(ios): keyboard height on app load (#6672)
* feat(ios): adds back and forward navigation buttons for in-app help (#6654)
* change(ios): modernization of the Keyman app main screen's layout logic (#6666)

## 15.0.253 beta 2022-05-27

* chore(ios): FV certificate hash (#6673)

## 15.0.252 beta 2022-05-25

* fix(ios): fixes OSK mispositioning bug in iOS 15 (#6631)

## 15.0.251 beta 2022-05-23

* fix(ios): stops leaking memory on system keyboard rotation (#6552)
* fix(android/engine): Fix sticky long-press keys (#6637)

## 15.0.250 beta 2022-05-20

* feat(android/app): Provide haptic feedback when typing (#6626)
* fix(ios): disables transparent nav bar for ios 15 (#6633)

## 15.0.249 beta 2022-05-19

* chore(common): improve PR reporting of changes (#6624)

## 15.0.248 beta 2022-05-17

* fix(windows): use GlobalGetAtomName to access global list (#6580)

## 15.0.247 beta 2022-05-16

* chore(common/resources): Update langtags.json (#6594)
* fix(android/engine): Refresh OSK when changing spacebar text (#6620)

## 15.0.246 beta 2022-05-11

* chore(ios): Set transparent background on "more" icons (#6598)
* chore(common): Add crowdin strings for Polish (#6593)

## 15.0.245 beta 2022-05-10

* chore(common): Add crowdin strings for Italian (#6592)

## 15.0.244 beta 2022-05-09

* fix(windows): Don't reset defaults on Keyman upgrade (#6491)

## 15.0.243 beta 2022-05-03

* chore(web): report on file size (#6584)
* chore(android/engine): Ignore DownloadManager if ID not found (#6581)
* fix(common/models): prevents preemptive tokenization of potential contractions (#6574)
* fix(web): fixes fat-finger distrib when subkey selected (#6526)

## 15.0.242 beta 2022-04-29

* docs(windows): Update OS and whatsnew in help (#6560)
* fix(android/engine): Check suggestion banner after setting keyboard (#6559)
* chore(common): Update Crowdin strings for Fula (#6562)

## 15.0.241 beta 2022-04-27

* change(web): touch alias optimization followup (#6566)
* fix(common/core/web): Fix forEach loop in SentryManager (#6569)
* fix(linux): Fix shared keyboard (un-)installation in Bionic (#6565)

## 15.0.240 beta 2022-04-26

* feat(web): adds .getKeyboard().IsRTL, .HasLoaded (#6567)
* fix(android): Don't show welcome.htm on keyboard package update (#6555)
* fix(android/engine): Localize some Toast notifications (#6561)

## 15.0.239 beta 2022-04-22

* fix(web): optimizes, enhances caret placement within text on mobile devices (#6551)

## 15.0.238 beta 2022-04-21

* chore(linux): Fix failure in Debian reproducibility testing (#6548)
* chore(linux): Update Linux dependencies (#6549)

## 15.0.237 beta 2022-04-20

* feat(linux): WIP - implement capslock handling with Wayland  ️ (#6213)

## 15.0.236 beta 2022-04-19

* fix(android): key tip sticky with multitap (#6494)
* fix(web): blocks modifier key fat-fingering (#6473)

## 15.0.235 beta 2022-04-18

* chore(android,windows): Update crowdin strings for bwr-NG (#6532)

## 15.0.234 beta 2022-04-14

* chore(common): Add Crowdin strings for Portuguese (pt-PT) (#6509)
* chore(linux): Fix l10n language mappings (#6529)
* chore(linux): Update Debian changelog (#6530)

## 15.0.233 beta 2022-04-13

* chore(android/app): Add description for referrer response error (#6524)
* fix(linux): test fileVersion during package install (#6522)

## 15.0.232 beta 2022-04-11

* fix(linux): Remove tab completion warning (#6482)

## 15.0.231 beta 2022-04-09

* test(common/resources): Add Noto font to web_context_tests keyboard (#6488)

## 15.0.230 beta 2022-04-08

* fix(android/engine): Show keyboard after rotating (#6498)
* fix(windows): don't log before checking nil when installing keyboard (#6505)
* chore: Revert "feat(web): prediction casing follows current layer" (#6510)
* chore(linux): Update changelog file (#6500)
* fix(windows): framework for switching of pipeline (#6392)

## 15.0.229 beta 2022-04-07

* fix(iOS): check version during package install (#6479)
* feat(ios): testflight builds for PRs (#6486)
* chore(ios): Document minimum iOS version 12.1 (#6492)
* fix(developer): handle exception on unpaired surrogate (#6487)
* chore(mac): Update whatsnew for 15.0 (#6493)
* fix(ios): use CFBundleShortVersionString for version (#6495)
* fix(android/app): Simplify sharing intent (#6481)

## 15.0.228 beta 2022-04-05

* feat(web): prediction casing follows current layer (#6459)
* fix(mac): check package fileVersion during install (#6415)

## 15.0.227 beta 2022-04-04

* fix(developer): improves More Help link for icon tool (#6464)
* chore(android): Document minimum required version of Chrome (#6468)
* chore(common): Update in-app help versions to 15.0 (#6469)
* fix(web): rely on caller for unmatched keys (#6457)
* feat(developer): hint on unreachable code (#6440)
* feat(developer): check filename case consistency when compiling keyboard (#6445)
* feat(developer): add Clear All button to icon editor (#6465)
* fix(developer): icon editor font color now follows fgcolor (#6466)
* fix(developer): validate kmconvert parameters (#6451)
* feat(developer): check for duplicate store/group names (#6463)
* docs(linux): Update testing section in readme (#6460)

## 15.0.226 beta 2022-04-01

* fix(web): track stateKeys changes with layer changes (#6453)

## 15.0.225 beta 2022-04-01

* fix(ios): reset context when osk is resized (#6428)
* feat(developer): keep Virtual Key Identifier dialog open on Insert (#6449)
* fix(oem/fv/ios): maintain consistent keyboard state between Keyman Engine and FV app (#6446)

## 15.0.224 beta 2022-03-29

* fix(android/engine): Clear WebView cache on package install (#6438)
* feat(oem/fv/ios): Dictionary Support (#6197)

## 15.0.223 beta 2022-03-27

* fix(mac): invalid keyboard breaks configuration (#6421)
* fix(windows): make long menus scroll in Configuration (#6424)
* fix(web): ios popup positioning and style (#6383)

## 15.0.222 beta 2022-03-25

* fix(windows): put correct HK_ALT flag for modifier (#6425)

## 15.0.221 beta 2022-03-24

* fix(developer): structure sizes for kmcomp x64 (#6408)
* fix(common): report more log detail in increment-version (#6418)

## 15.0.220 beta 2022-03-23

* fix(web): adjust touch alias element positioning (#6406)
* fix(android/engine): Display longpress keys during a Move gesture (#6138)

## 15.0.219 beta 2022-03-22

* chore(common): Update Hausa strings (#6399)

## 15.0.218 beta 2022-03-21

* chore(android/engine): Remove visual gaps in keyboard picker menu (#6370)

## 15.0.217 beta 2022-03-19

* fix(web): bounding rect offset was incorrect (#6357)

## 15.0.216 beta 2022-03-18

* feat(linux): Allow to run ibus tests on Wayland  ️ (#6202)

## 15.0.215 beta 2022-03-17

* fix(web): crash on focus before init (#6393)

## 15.0.214 beta 2022-03-16

* feat(web): `&newLayer` and `&oldLayer` (#6366)

## 15.0.213 beta 2022-03-15

* feat(common): zip and index keyboard artifacts (#6367)
* fix(android/engine): Activate menu on globe longpress before release (#6356)
* fix(web): subkey touch position after scroll (#6340)
* fix(developer): warn on ncaps usage inconsistency (#6347)
* fix(web): caps state tracking for touch (#6351)
* chore(windows): Update license date (#6374)
* chore(android): Separate CI step to generate Play Store notes (#6375)
* chore(linux): Update changelog and copyright (#6372)
* fix(oem/fv/android): Consolidate dictionary and keyboard settings (#6369)
* fix(linux): Don't start ibus-daemon multiple times (#6283)

## 15.0.212 beta 2022-03-11

* chore(linux): Fix lintian warnings (#6360)
* fix(common): allow custom keyboard builds (#6358)

## 15.0.211 beta 2022-03-10

* fix(oem/fv/android): Check keyboard exists before registering a model (#6346)
* chore(common): cleanup sh global color variables (#6353)

## 15.0.210 beta 2022-03-10

* feat(common): build test keyboards via script (#6352)

## 15.0.209 beta 2022-03-08

* fix(windows): add null pointer checks imsample keyboard (#6187)
* chore(linux): Debug why Beta build didn't get triggered (#6337)
* fix(developer): project should not steal focus on load (#6325)
* chore(android/engine): Remove bold styling in lists (#6341)
* fix(linux): Fix trigger script (#6342)
* chore: increment to 15.0.209 (#6343)

## 15.0.206 beta 2022-03-02

* change(web): tightens call signature for banner selection (#4966)
* fix(windows): cleanup edge cases in k32_load (#5197)
* feat(ios): refinement of spacebar-captioning PR (#5368)

## 15.0.205 alpha 2022-03-02

* fix(developer): move ngrok to Server/bin/ (#6304)
* fix(linux): Gracefully handle keyboard download failure (#6285)
* fix(linux): Gracefully handle makedirs failures (#6287)
* chore(common): tweak CODEOWNERS (#6305)
* fix(developer): rename Code to ID in Touch Layout Editor (#6309)
* feat(developer): kmcomp -h and -help (#6312)
* chore(developer): remove Package for Distribution action (#6313)
* fix(web): error in floating osk view when no keyboard active (#6316)
* chore(developer): disable upload for server except localhost (#6300)
* fix(ios): tweak simulator builds to support M1 (#6317)
* fix(developer): handle paste event in touch layout editor (#6295)
* fix(developer): Improve active tab visibility (#6314)
* fix(developer): font for Package and Model Editors (#6311)
* fix(web): stop repeated CSS injection (#6310)
* feat(developer): support commas in frequency in wordlists (#6315)
* chore: release to beta

## 15.0.204 alpha 2022-03-01

* feat(android): Android web-based test harness (#6250)
* fix(developer): server font support (#6298)
* chore(windows): Remove core icon from system tray (#6302)

## 15.0.203 alpha 2022-02-26

* chore(common): remove rust (for now!) (#6291)

## 15.0.202 alpha 2022-02-24

* feat(common): echo in macos.sh setup script (#6246)
* chore(common): Add CODEOWNERS file (#6262)
* chore(oem/fv): Add keyboard version and languages (#6226)
* fix(common/core/desktop): Add num and scroll lock mask to VKeyToChar (#6211)
* fix(linux): Properly compare versions in km-package-install (#6278)

## 15.0.201 alpha 2022-02-21

* fix(developer): npm publish with correct tier (#6259)
* fix(linux): Fix stray `[-vv]` in man pages (#6241)

## 15.0.200 alpha 2022-02-20

* chore(common): Update French strings (#6263)

## 15.0.199 alpha 2022-02-18

* fix(web): Non-integer OSK sizes (#6248)

## 15.0.198 alpha 2022-02-13

* fix(developer): compile package fileVersion from keyboard FileVersion (#6145)
* chore(android/app): Clarify help on switching keyboards (#6234)

## 15.0.197 alpha 2022-02-12

* chore(deps): bump follow-redirects from 1.7.0 to 1.14.8 in /web/testing/regression-tests (#6247)
* chore(deps-dev): bump karma from 6.3.4 to 6.3.14 in /web/testing/regression-tests (#6236)
* fix(windows): create Keyman/Diag folder in redirected profile (#6225)
* fix(developer): support KM_KBP_IT_CAPSLOCK (#6230)

## 15.0.196 alpha 2022-02-11

* fix(windows): add flag to track core process event (#6219)

## 15.0.195 alpha 2022-02-09

* chore(android): Upgrade gradle-wrapper version to 6.8 (#6223)

## 15.0.194 alpha 2022-02-08

* chore(android): Update Play publishing plugin to 3.5.0 (#6217)
* feat(android/engine): Single tap globe switch to previous system IME (#6206)
* feat(linux): Try to start ibus if not running (#6109)

## 15.0.193 alpha 2022-02-07

* chore(ios): certificate update again (#6208)

## 15.0.192 alpha 2022-02-04

* chore(oem/firstvoices): Add fv_haisla keyboard (#6181)
* chore(linux): Improve dependencies in Linux package (#6198)
* chore(linux): Remove obsolete Ubuntu 21.04 Hirsute (#6199)

## 15.0.191 alpha 2022-02-03

* fix(developer): remove keymanengine.msm (#6143)

## 15.0.190 alpha 2022-02-02

* fix(linux): Don't crash when installing keyboards in Wasta (#6159)

## 15.0.189 alpha 2022-02-01

* fix(linux): Don't crash if we lack permissions (#6179)
* chore(linux): stop releasing KMFL together with Keyman (#6184)

## 15.0.188 alpha 2022-01-28

* fix(mac): Check for CODE_SETSYSTEMSTORE (#6169)
* feat(developer): Keyman Developer Server (#6073)

## 15.0.187 alpha 2022-01-27

* fix(developer): handle Core failure to load (#6141)
* feat(windows): Keyman Core Integration - Support for IMX DLLs (#5936)
* chore(windows): testhost readme (#6164)
* chore(ios): certificate update (#6165)
* feat(linux): add support for Wayland (#6135)

## 15.0.186 alpha 2022-01-26

* feat(linux): Replace hirsute with impish (#6161)

## 15.0.185 alpha 2022-01-25

* feat(windows): testhost project for debugging keyman32 (#6154)
* feat(windows): handle key output in testhost (#6157)
* chore(deps): bump node-fetch from 2.6.1 to 2.6.7 in /resources/build/version (#6156)
* fix(developer): prevent crash on build dblclicked file (#6151)
* chore(linux): Add comment for clarification of code behaviour (#6152)

## 15.0.184 alpha 2022-01-24

* fix: only auto-merge if `auto:` prefix in title (#6146)

## 15.0.183 alpha 2022-01-21

* chore(linux): Update changelogs for 14.0.284  (#6132)
* chore(linux): Revert workaround for Python bug (#6133)

## 15.0.182 alpha 2022-01-21

* chore(linux): Add metadata file for packaging (#6127)
* fix(web): crash in setTitleFromKeyboard when no keyboard active (#6129)

## 15.0.181 alpha 2022-01-19

* fix(linux): Add workaround for Python bug (#6124)
* docs(linux): Add missing dependency (#6118)

## 15.0.180 alpha 2022-01-18

* feat(web): Caps Layer and double-tap gesture (#5989)
* fix(web): Use regex to determine display layer and functional layers (#6100)

## 15.0.179 alpha 2022-01-17

* feat(web): Start of Sentence support - part 1 (#5963)
* feat(developer): support for Caps Lock layer (#5988)
* fix(web): call postKeystroke on banner touch (#6004)
* chore(ios): project scripts should use /usr/bin/env bash (#6114)
* chore: update docs (#6115)
* chore(common): Check in crowdin strings for Kibaku (#6048)
* fix(mac): externalize strings for package info window localization (#6088)
* fix(developer): repatch #6074 (#6111)
* fix(linux): Fix lintian errors (#6106)

## 15.0.178 alpha 2022-01-14

* chore(common): Check in crowdin strings for Waha (#6050)

## 15.0.177 alpha 2022-01-13

* chore(common): Check in crowdin strings for Marghi (#6049)

## 15.0.176 alpha 2022-01-12

* fix(windows): add x64 build fix invalid memory access (#6057)
* feat(windows): method set context correctly truncates input buffer (#6080)
* chore(common): Check in crowdin strings for Kanuri (#6047)
* fix(linux): Update minimum sentry-sdk version (#6093)
* docs: Enhance Linux build setup doc (#6094)

## 15.0.175 alpha 2022-01-11

* feat(linux): Don't create Sentry events for errors (#6070)
* fix(linux): Fix attribute error (#6086)
* fix(developer): bundle.sh must succeed (#6091)

## 15.0.174 alpha 2022-01-10

* chore(android/samples): Add -no-daemon flag to KMSample2 build script (#6082)
* fix(linux): Fix installation of shared packages (#6015)

## 15.0.173 alpha 2022-01-05

* chore(common): Update crowdin strings for Amharic (#6040)
* fix(linux): fix release version number for Sentry reporting (#6068)

## 15.0.172 alpha 2021-12-23

* fix(developer): work around devDependencies bug in npm (#6074)
* chore(windows): nmake makefile cleanup (#6065)

## 15.0.171 alpha 2021-12-17

* feat(developer): remember test page preferences on reload (#6033)
* feat(developer): tidy up package list in web test view (#6034)
* feat(developer): live reload of web debugger (#6035)
* feat(developer): cache web debug objects across sessions (#6036)
* fix(developer): project_mru.xml path (#6061)
* chore(developer): add browse4folder to components (#6062)
* docs(windows): Add note on split user/admin accounts (#6059)
* fix(mac): increase OSK character size by 50% (#6006)

## 15.0.170 alpha 2021-12-16

* docs(linux): Document how to disable error reporting (#6051)
* fix(linux): fix release version number for Sentry reporting (#6052)
* fix(linux): Fix package description (#6054)

## 15.0.169 alpha 2021-12-15

* fix(mac): display Unicode package name correctly instead of '????' (#6016)
* fix(linux): Improve ibus-keyman tests (#6044)

## 15.0.168 alpha 2021-12-14

* feat(linux): Add support for Ubuntu 22.04 Jammy (#6037)

## 15.0.167 alpha 2021-12-13

* fix(windows): remove unused variable (#6039)

## 15.0.166 alpha 2021-12-11

* chore(windows): version.rc use macros instead of mkver (#6019)
* chore(windows): move from Borland make to nmake (#6020)
* chore(windows): use findstr instead of Borland grep (#6021)
* chore(windows): replace mkver with mkver.sh (#6022)
* chore(windows): build without delphi (#6030)

## 15.0.165 alpha 2021-12-08

* fix(web): font size was not consistently set (#5906)

## 15.0.164 alpha 2021-12-07

* refactor(common/core): Use defines in all xstring tests (#6002)

## 15.0.163 alpha 2021-12-06

* chore(common): buf now uses decxstr so remove comment (#6012)

## 15.0.162 alpha 2021-12-05

* chore(linux): Update changelogs for 14.0.283    (#6008)

## 15.0.161 alpha 2021-12-04

* chore(linux): Allow to specify debian revision    (#5999)
* chore(linux): Remove lintian warning    (#6000)

## 15.0.160 alpha 2021-12-03

* fix(developer): building kmw keyboard had wrong message callback signature (#5972)
* chore(common): add more description to Buf method (#6003)
* chore(linux): add non-surrounding-text feature to tests (#5968)

## 15.0.159 alpha 2021-12-02

* fix(developer): debugger crash with empty group (#5995)
* chore(android): Document switching to other system keyboard (#5991)
* fix(android/engine): Fix font paths (#5987)

## 15.0.158 alpha 2021-12-01

* feat(android/engine): Add other IME's to the Keyboard Picker menu (#5973)
* chore(android): Specify build tools version on other projects (#5979)

## 15.0.157 alpha 2021-11-29

* fix(web): remove obsolete popupBaseTarget (#5949)
* fix(web): improve keyboard switch performance (#5958)
* fix(web): support saving focus for custom OSK interactions (#5947)
* fix(developer): render OSK nicely on touch devices (#5923)
* chore(developer): move kcframe into its own folder (#5971)
* fix(web): restore `dragEnd` function (#5977)
* feat(common/core/desktop): Allow preserved key support (#5850)
* chore(windows): Consolidate documentation for Windows devboxes (#5481)
* chore(developer): tweak language metadata message (#5978)
* fix(linux): Fix ibus-keyman integration tests (#5966)

## 15.0.156 alpha 2021-11-26

* fix(windows): fix loadkeyboardoptions core memory error (#5959)

## 15.0.155 alpha 2021-11-24

* fix(web): reset scroll anchor on touchend (#5919)
* fix(web): shorten setFocusTimer() (#5946)
* fix(web): clear longpress timeout if user does a flick up (#5952)

## 15.0.154 alpha 2021-11-22

* fix(web): simplify hide transition (#5910)
* fix(web): inline osk keytip position (#5938)
* feat(mac): i18n, support localization of Keyman for Mac (#5869)
* chore(linux): Remove unused test methods (#5954)
* chore(linux): Run ibus-keyman tests as part of the build (#5889)

## 15.0.153 alpha 2021-11-20

* fix(web): resolve unhandled exception in promise (#5902)

## 15.0.152 alpha 2021-11-19

* fix(common/core):  Update decxstr to check all characters while decrementing (#5842)

## 15.0.151 alpha 2021-11-18

* fix(developer): use correct tike icon (#5925)
* fix(android/engine): Support for U_xxxx_yyyy subkeys (#5913)
* fix(mac): use xcframework to support m1 (#5933)
* fix(developer): move web osk beneath text area (#5937)

## 15.0.150 alpha 2021-11-17

* fix(web): remove canvas use for iOS compatibility (#5915)

## 15.0.149 alpha 2021-11-17

* chore(developer): Update jszip version (#5770)
* chore(common): Check in crowdin strings for Fulfulde Nigeria (#5841)

## 15.0.148 alpha 2021-11-16

* chore(common): Check in crowdin strings for Mandara (Wandala) (#5857)
* chore(linux): Update method comments (#5883)

## 15.0.147 alpha 2021-11-15

* chore(android,linux,windows): Check in crowdin strings for Bura-Pabir (#5839)
* chore: add keyman-local.com to iis https setup (#5892)
* fix(web): avoid error if timerid not valid (#5908)

## 15.0.146 alpha 2021-11-14

* feat(developer): support for U_xxxx_yyyy (#5894)

## 15.0.145 alpha 2021-11-13

* fix(developer): crash exporting OSK on European keyboard (#5893)
* feat(developer): validate .kps files during compile (#5895)

## 15.0.144 alpha 2021-11-12

* fix(android/engine): Truncate language name in list (#5878)
* fix(android/app): Check KMP file exists before attempting to extract (#5849)
* fix(web): Check parent element is defined before assigning (#5874)
* fix(android/app): Revert test code (#5899)

## 15.0.143 alpha 2021-11-11

* fix(developer): handle shortcut keys in debugger (#5776)

## 15.0.142 alpha 2021-11-08

* chore(linux): Cleanup (#5886)
* chore(linux): cleanup defunct tests (#5885)
* chore(linux): fix failing ibus-keyman tests (#5884)

## 15.0.141 alpha 2021-11-05

* chore(linux): Add integration tests for ibus-keyman (#5881)

## 15.0.140 alpha 2021-11-03

* feat(android): Allow uninstall of sil_euro_latin keyboard (#5838)
* chore(common/core): Compile test keyboards to .kmp (#5864)

## 15.0.139 alpha 2021-11-02

* chore(common/core): refactor kmx unit tests (#5862)
* chore(linux): Extract keycode_to_vk[] to separate header file (#5863)

## 15.0.138 alpha 2021-10-29

* chore(linux): Set correct version number on ibus component (#5859)
* chore(common/core): generate kmp.json (#5860)

## 15.0.137 alpha 2021-10-28

* chore(common): Don't check for conventional commit for fixups (#5858)

## 15.0.136 alpha 2021-10-23

* chore(android/engine): Address globe key TODO (#5836)

## 15.0.135 alpha 2021-10-20

* docs(windows): add note to caps-lock-stores test (#5837)

## 15.0.134 alpha 2021-10-19

* docs(windows): add install apps from anywhere instructions (#5829)

## 15.0.133 alpha 2021-10-16

* chore(common): Check in crowdin strings for Hausa (#5768)

## 15.0.132 alpha 2021-10-15

* feat(windows): manual keyboard caps tests (#5808)
* feat(windows): Change caps to work with common core processor (#5803)

## 15.0.131 alpha 2021-10-13

* chore(windows): Add obj cod and pdb to gitignore (#5826)

## 15.0.130 alpha 2021-10-11

* fix(common/core): incxstr checks all character while incrementing (#5712)

## 15.0.129 alpha 2021-10-08

* chore: Create CODE_OF_CONDUCT.md (#5819)
* chore: Add CONTRIBUTING documentation (#5821)
* chore: Add link to SIL (#5823)

## 15.0.128 alpha 2021-10-07

* fix(ios): move sentry settings responsibility to build agent (#5805)

## 15.0.127 alpha 2021-10-06

* chore(web): disable registerstub test (#5800)

## 15.0.126 alpha 2021-10-05

* chore: sentry.io dsn  ‍ ️ (#5787)

## 15.0.125 alpha 2021-10-05

* fix(android/engine): Remove unnecessary permissions from Manifest (#5752)
* feat(developer): touch layout testing (#5723)
* fix(web): popup positioning (#5742)
* chore(linux): Update changelog files for 14.0.282  (#5794)

## 15.0.124 alpha 2021-10-04

* fix(web): support variable stores with predictive text (#5749)
* fix(windows): handle edge cases using default language (#5709)
* fix(linux): Don't crash with non-keyboard package file (#5755)
* fix(linux): Don't crash displaying keyboard details (#5758)

## 15.0.123 alpha 2021-10-01

* fix(common): Fix cherry-pick labeling (#5782)
* chore(ios,android,windows): Update crowdin strings for Amharic (#5722)

## 15.0.122 alpha 2021-09-30

* fix(developer): move ampersand to shift+7 on touch (#5746)
* feat(windows): Keyman Core integration (#5443)
* chore(windows): Keyman Core UI and Settings (#5769)
* fix(linux): Fix debian package script (#5771)
* docs(mac): Add note about installing with homebrew (#5767)

## 15.0.121 alpha 2021-09-29

* fix(developer): remove empty touch rows on save (#5720)
* chore(common): Check in crowdin strings for Shuwa Latin (#5734)
* chore(windows): fix broken links in help (#5765)

## 15.0.120 alpha 2021-09-28

* fix: help.keyman.com script file cleanup (#5751)
* chore(common): Enhance cherry-pick labeling (#5759)

## 15.0.119 alpha 2021-09-24

* chore(common): support forked repos when triggering test builds (#5738)

## 15.0.118 alpha 2021-09-23

* chore(deps): bump ansi-regex from 5.0.0 to 5.0.1 in /resources/build/version (#5741)
* refactor(web): active element management in relation to OSK display control  ️ (#5644)
* refactor(web): osk activation and visibility modeling  ️ (#5661)
* chore(android/app): Remove runConfigruations.xml file (#5743)
* feat(web): new OSK type - the inlined OSK (#5665)
* chore(web): inline osk test page (#5728)
* feat(web): enables mouse interactivity for the predictive banner (#5739)
* fix(mac): add support for M1 processor (#5701)

## 15.0.117 alpha 2021-09-22

* fix(developer): run kmlmc from Keyman source path (#5727)

## 15.0.116 alpha 2021-09-21

* fix(common/core): insure list pntr is incremented (#5669)
* chore(common): Check in crowdin strings for Simplified Chinese (#5681)
* fix(web): Null check for calculating globe key position (#5724)

## 15.0.115 alpha 2021-09-20

* chore(common/core): cleanup version headers (#5719)
* refactor(web): OSK modularization - specialized OSKView classes  ️ (#5633)
* feat(developer): track changes to option values better in debugger (#5696)
* fix(developer): support chiral modifiers in debugger (#5697)

## 15.0.114 alpha 2021-09-17

* feat(developer): Debugger platform option (#5640)
* feat(developer): Debugger keyboard options (#5647)
* fix(developer): add version info to Core (#5711)
* fix(common/core/web): layer reset on physical keystroke after OSK interaction (#5641)
* fix(ios): prevents installation of packages without JS (#5698)

## 15.0.113 alpha 2021-09-16

* fix(ios): version-tagging for errors logged from the system keyboard (#5693)
* fix(web): spacebar caption when functional layer differs from display layer (#5688)
* refactor(web): OSK modularization - view components, layout cleanup  ️ (#5619)
* refactor(web): OSK modularization - common-path code -> OSKView  ️ (#5620)
* fix(common/core/web): Remove empty rows in OSK (#5699)

## 15.0.112 alpha 2021-09-14

* chore(common): Update to Unicode 14.0 (#5686)
* chore(common): Add GitHub action to upload sources to crowdin (#5687)
* fix(windows): fallback to filename if `&name` not set (#5684)

## 15.0.111 alpha 2021-09-13

* fix(developer): include Keyman Core in Keyman Developer (#5649)
* fix(developer): ensure file modified after import from layout (#5676)
* chore(android/engine): Fix dictionary selector and back arrow styling (#5667)
* fix(web,android/app): Select numeric layer when entering a numeric field (#5664)

## 15.0.110 alpha 2021-09-10

* fix(android/engine): Fix localization on Android M (#5670)

## 15.0.109 alpha 2021-09-06

* fix(android/engine): Fix backspace on Android 5.0 (#5660)

## 15.0.108 alpha 2021-09-01

* fix(linux): Use first keyboard language if none given (#5655)

## 15.0.107 alpha 2021-08-31

* fix(linux): Fix launch of km-config (#5631)

## 15.0.106 alpha 2021-08-30

* fix(linux): Remove Linux workspace (#5654)

## 15.0.105 alpha 2021-08-27

* feat(android/app): Add navigation buttons to Info Activity (#5622)
* fix(android): Fix Chrome version ranges for Keyman engine functionality (#5629)

## 15.0.104 alpha 2021-08-26

* chore(linux): Update changelog files (#5637)
* fix(web): Check keyboard before marking layout calibrated (#5630)

## 15.0.103 alpha 2021-08-25

* chore(linux): Improve debian package script (#5626)
* chore(linux): Fix uploading to Sentry (#5623)

## 15.0.102 alpha 2021-08-24

* fix(android/engine): UX improvement when 1 keyboard installed (#5570)
* chore(windows): sentrytool param file support (#4172)
* fix(developer): F6 in debugger and cef focus (#5597)
* chore(common): add report-history.sh (#5527)
* chore(common): more on report-history (#5603)
* chore: fixup HISTORY.md (#5604)
* chore(android/app): Add in-app help for adjusting keyboard height (#5621)

## 15.0.101 alpha 2021-08-23

* refactor(web): overhaul of OSK key layout calcs + styling, merges desktop & touch logic (#5462)
* fix(android): keyboard's black bar bug (#5521)
* refactor(web): prep work for mouse-based use of touch events (#5506)
* refactor(web): mouse-based use of touch handlers (#5530)

## 15.0.100 alpha 2021-08-22

* fix(linux): Fix uploading to launchpad for legacy projects (#5612)

## 15.0.99 alpha 2021-08-21

* feat(android/app): Add menu to adjust keyboard height (#5606)

## 15.0.98 alpha 2021-08-20

* fix(windows): Ignore Access Denied error creating task (#4365)
* fix(web): Have util.wait check useAlerts option (#5538)
* fix(developer): debugger cleanup (#5588)
* chore(developer): debug deadkey 1-based values (#5592)
* chore(developer): refactor forcekeyboard in debug (#5593)
* fix(developer): get Test Mode working again (#5594)
* chore(developer): hide debug events panel (#5595)
* fix(common/core/web): behavior with unmatched final group (#5553)
* fix(ios): iOS 13 and 14 only - stuck settings toggles (#5548)
* fix(linux): Fix lost context after pressing K_SHIFT (#5601)

## 15.0.97 alpha 2021-08-19

* fix(linux): Fix uploading to launchpad (#5607)

## 15.0.96 alpha 2021-08-19

* docs(linux): Fix readme for ibus-keyman (#5565)
* chore(common/core): Add uninstall option to build script (#5564)
* feat(web): setup build artifacts for manual tests (#5582)
* feat(ios): adds option for Simulator-compatible testing artifact (#5557)
* refactor(web): OSK layout hierarchy encapsulation (#5451)
* refactor(web): reworks VisualKeyboard layout spec design (#5459)
* refactor(web): overhaul of OSK key layout calcs + styling, merges desktop & touch logic (#5462)
* fix(android): keyboard's black bar bug (#5521)
* fix(android): removes duplicated line (#5587)
* fix(linux): Check for valid kmp file (#5583)
* fix(linux): Handle corrupt icon file (#5585)
* fix(windows): handle restoring modal dialogs consistently (#5586)
* chore(android): Remove runConfigurations.xml files (#5572)
* fix(windows): Welcome should always show in front (#4657)

## 15.0.95 alpha 2021-08-06

* fix(windows): wrap text in keyboard installation dialog (#5559)
* fix(windows): tsysinfo format grid correctly (#5556)
* feat(android): Add API for checking Chrome version (#5520)
* fix(android/app): Fix cleanup when progress dialog cancelled (#5541)
* fix(common/core/desktop): split smp for context chars (#5562)
* feat(developer): debugger uses Keyman Core (#5513)
* fix(common/models): keep/suggestion diacritic sensitivity when de-duping (#5480)
* fix(web): Blank spacebar text when displayName is empty string (#5555)
* chore(linux): Consolidate Debian source packages (#5536)
* chore(common/core): Linux side of caps-lock stores (#5497)

## 15.0.94 alpha 2021-08-05

* chore(ios,windows): Update crowdin strings for Azerbaijani (#5486)
* chore(android): Update sentry-android-gradle-plugin to 2.1.0 (#5546)
* fix(web): Have util.wait check useAlerts option (#5538)
* fix(android): gracefully handle errors in KMW keyboards (#5423)
* chore(linux): Remove compiler warning in ibus-keyman (#5549)

## 15.0.93 alpha 2021-08-04

* fix(linux): Improve uninstallation (#5505)
* refactor(linux): Reformat keycode_to_vk table (#5508)
* refactor(linux): Cleanup import of os.path (#5529)
* chore(common/core): Allow shell script for kmcomp (#5498)
* refactor(common/core): Move comment to where it belongs (#5518)

## 15.0.92 alpha 2021-08-03

* fix(android/app): Check Play Store release notes less than 500 chars (#5535)

## 15.0.91 alpha 2021-08-02

* chore(common): stable history update (#5515)
* chore(android): log fontpath error (#5516)
* chore(windows): fixup cef interfaces for CEF 90 (#5514)
* chore(common/core): refactor backspace handling (#5512)
* chore(windows): split keyman64 header to keymanengine (#5522)
* fix(ios/engine): engine migration always precedes installs (#5484)
* chore(common): Add C++ formatting rules (#5523)
* fix(developer): compiler use and match behavior for Web should be same as Core (#5525)
* fix(web): stuck key highlighting from touchpoint movement (#5490)

## 15.0.90 alpha 2021-07-28

* chore(windows): patches for Delphi 10.4 (#5496)
* chore(common/core): additional debug tests (#5482)
* chore(android): Update globe key help and whatsnew (#5495)
* chore(common/core): handle deletion of markers in actions (#5489)
* chore(linux): Remove Groovy and Impish builds (#5502)

## 15.0.89 alpha 2021-07-27

* feat(web): Update addKeyboards to return Promise (#5389)
* fix(common/core/web): error from early fat-finger termination due to OS interruptions (#5479)
* fix(common/core/web): OSK state-key management (#5456)
* feat(android/engine): Improve globe key experience (#5437)

## 15.0.88 alpha 2021-07-23

* feat(common/core): debug action index (#5470)
* refactor(web): osk inner-frame abstraction (help text vs std OSK) (#5430)
* refactor(web): delayed OSK initialization (#5412)
* chore(common/core): Implement capsAlwaysOff system store (#5432)

## 15.0.87 alpha 2021-07-22

* fix(ios): keyboard swapping (#5475)
* refactor(linux): Refactor processing actions in separate methods (#5452)

## 15.0.86 alpha 2021-07-20

* chore(android): improve error reporting for kmw (#5468)

## 15.0.85 alpha 2021-07-19

* feat(common/core/desktop): kmx debugger basic infrastructure (#5425)
* feat(common/core/desktop): add debug events (#5448)
* chore(developer): fixup devtime paths (#5449)
* refactor(web): partial encapsulation of the desktop title bar and resize bar (#5393)
* refactor(web): osk move & resize handler encapsulation (#5409)

## 15.0.84 alpha 2021-07-09

* fix(web): Copy the keyboard stubs for registration (#5438)
* fix(android/engine): register lexical model in switchToNextKeyboard (#5439)

## 15.0.83 alpha 2021-07-08

* chore(windows): remove backup build step (#5434)
* refactor(common/core/desktop): Fix file and class names (#5445)
* refactor(web): longpresses, groundwork for additional gestures (#5387)
* fix(android/engine): Display dictionary help link (#5427)
* refactor(common/core/desktop): Rename Load method (#5444)

## 15.0.82 alpha 2021-07-07

* fix(common/models/types): fixes test script config (#5441)

## 15.0.81 alpha 2021-07-06

* fix(android/engine): Fix font file path (#5424)
* chore(common): npm audit fixes (#5419)
* chore(web): updates regression test-deps to same versions as web (#5420)
* chore(windows): make build paths consistent (#5405)

## 15.0.80 alpha 2021-07-02

* feat(web): spacebar text controls (#5348)
* feat(android): Spacebar text controls (#5349)
* feat(ios): add spacebar text controls (#5365)
* fix(linux): Don't crash with corrupt keyboard (#5414)

## 15.0.79 alpha 2021-07-01

* docs: start consolidation of build config docs (#5324)
* fix(developer): builder and editor commands were ignored (#5391)
* chore(developer): remove obsolete InitClasses (#5403)
* change(web): fat-finger-performance unit-test threshold (#5404)
* fix(linux): Fix auto-generated help (#5399)
* fix(linux): Improve bitmap conversion (#5401)

## 15.0.78 alpha 2021-06-30

* fix(web): console errors when using SHIFT w help-text 'osk' (#5392)

## 15.0.77 alpha 2021-06-29

* fix(windows): #5336 appcontext get to handle small buffer (#5383)
* fix(common): get handle caller buffer size small than internal Fixes … (#5390)

## 15.0.76 alpha 2021-06-28

* chore(android): Add help on Settings option to change spacebar caption (#5375)
* feat(android): hide textarea for perf (#5376)
* chore: keyboard_info.source.json 1.0.6 (#5379)
* refactor(android): setKeyboard and setKeymanLanguage (#5338)
* feat(web): Merge keyboard stub with error stub (#5340)
* feat(linux): Add reference index page (#5370)
* fix(linux): Fix restarting ibus when running with sudo (#5371)
* refactor(linux): Centralize paths (#5372)
* fix(linux): Save QR code in temporary directory (#5373)

## 15.0.75 alpha 2021-06-25

* fix(windows): osk scaling mismatch on horz axis (#5341)
* fix(linux): Don't add duplicate entries when reinstalling keyboard (#5363)
* fix(linux): fix tab completion for command line tools (#5362)
* feat(web): Update addLanguageKeyboards to return a Promise (#5260)
* feat(web): Add option useAlerts to control alerts() (#5302)

## 15.0.74 alpha 2021-06-24

* fix(android): fixes application of nextlayer for subkeys with customized layer setting (#5350)
* feat(common/core/web): engine correction-prep optimizations (#5319)
* feat(linux): Add support for impish (Ubuntu 21.10) (#5334)
* fix(linux): Add libglib2.0-bin to keyman Depends (#5360)

## 15.0.73 alpha 2021-06-23

* chore(windows): add engine test framework (#5337)
* feat(linux): Auto-generate reference help (#5326)
* fix(linux): Improve error handling if `lang_tags_map.py` is missing (#5345)

## 15.0.72 alpha 2021-06-22

* fix(mac): .kmx max file version is now 0x0E00 (14.0) (#5329)
* fix(mac): kmp.inf is windows-1252 (#5328)
* fix(windows): restore endpoints for interface stability (#5254)
* chore(linux): Build also on riscv64 to facilitate migration on Ubuntu (#5323)
* refactor(web): OSK key source-code reorganization (#5291)
* refactor(web): osk key highlighting/preview behavior (#5292)
* change(ios): setText now uses JSON serialization (#5321)

## 15.0.71 alpha 2021-06-18

* feat(common): keyboard_processor wasm build (#5233)
* chore(linux): Update changelogs to match Debian (#5304)

## 15.0.70 alpha 2021-06-17

* fix(web): touch-move cancellation (#5290)
* fix(developer): kmconvert help match reality (#5298)

## 15.0.69 alpha 2021-06-16

* refactor(web): OSK layout calculations - simplification (#5279)
* refactor(web): VisualKeyboard - key event model (#5280)
* refactor(web): OSK keytip abstraction (#5287)
* refactor(web): encapsulation of browser-based subkey array popup (#5288)
* chore(common): bash version for run-required-test-builds.sh (#5267)
* fix(linux): Adjust version of dependency for ibus-keyman (#5281)

## 15.0.68 alpha 2021-06-15

* chore(common): Add YouTube links to Keyman 14 features (#5276)
* chore(common): Check in crowdin strings for Spanish (Latin America) (#5269)
* refactor(web): polishes management of OSK keys (#5257)
* refactor(web): Visual Keyboard disentanglement - pass 1 (#5259)
* refactor(web): VisualKeyboard height styling consistency (#5278)
* fix(common/models): predictive-text engine use of NFD input (#5273)
* chore(linux): Fix warnings (#5282)

## 15.0.67 alpha 2021-06-11

* feat(linux): Fix Linux packaging on i386 (#5250)

## 15.0.66 alpha 2021-06-11

* feat(android): silent install for referred kbd (#5240)
* fix(android): rotation is not updating keyboard (#5247)

## 15.0.65 alpha 2021-06-10

* docs(ios): minor readme tweak regarding prerequisites (#5253)
* fix(common/core/web): optimizes transform construction (#5248)

## 15.0.64 alpha 2021-06-09

* feat(android): Automatically install keyboard through Play Store (#5230)
* fix(oem/fv/ios): app encryption flag for app store uploads (15.0) (#5244)
* refactor(common/core/web, web): target-agnostic key events (#5181)
* fix(linux): Fix packaging on ppa (#5249)

## 15.0.63 alpha 2021-06-08

* feat(common): Rust infrastructure (#5162)
* fix(web): Always pass kill_browserstack (#5234)
* chore(common): iis setup script for dev boxes (#5206)
* fix(windows): support Esc key in Download Keyboard dialog (#5207)
* fix(android/engine): Adjust default OSK landscape size (#5201)

## 15.0.62 alpha 2021-06-04

* fix(developer): support spaces in regression tests (#5217)

## 15.0.61 alpha 2021-06-03

* fix(linux): Exclude s390x from package builds for ibus-keyman (#5213)
* feat(linux): Integrate with fcitx5 (#5215)

## 15.0.60 alpha 2021-06-01

* fix(windows): sentry.dll version handling (#5187)
* fix(windows): start keymanx64 with ShellExecute (#5202)
* fix(windows): fix size of splash (#5203)

## 15.0.59 alpha 2021-05-31

* fix(common): build script help param (#5175)
* feat(ios): resource file validation before use (#5178)
* fix(linux): Migrate from /usr/lib/ibus to /usr/libexec (#5183)
* fix(linux): Don't crash on invalid metadata (#5185)
* fix(android/samples): Set gradlew executable for Tests (#5198)
* fix(windows): avoid cached hotkey state (#5190)
* chore(windows): minor maintenance (#5192)
* fix(common/core/desktop): Fix failing tests on armhf (#5169)

## 15.0.58 alpha 2021-05-28

* chore(android): Add -clean flag to build script (#5145)
* feat(web): Add script to deploy KeymanWeb release to s.keyman.com (#5150)
* feat(ios): partial conversion of internal logs to use of Sentry (#5153)
* change(ios): workspace reorganization + cleanup (#5167)
* fix(linux): Fix Makefile and instructions for building (#5170)

## 15.0.57 alpha 2021-05-28

* chore(ios): dependencies - update sentry-cocoa to 6.2.1 (#5120)
* change(ios): minimum iOS version -> 12.1 (#5168)
* chore(ios): renames problematic app selectors (#5173)

## 15.0.56 alpha 2021-05-27

* fix(ios): link, but NOT embed KeymanEngine.xcframework in app exs (#5164)
* feat(common): offline help css (#5157)
* change(ios): help always offline (#5158)

## 15.0.55 alpha 2021-05-27

* fix(ios): artifact upload preparation (#5160)

## 15.0.54 alpha 2021-05-26

* chore(windows): Update Sentry to 0.4.9 (#5144)
* chore(web): Add CI script to kill BrowserStack tunnel (#5136)
* docs(linux): Update readme (#5147)
* chore(linux): Update changelogs (#5146)
* fix(ios): re-enable SKIP_INSTALL (#5155)

## 15.0.53 alpha 2021-05-25

* feat(android/app): Auto-generate Play Store release notes (#5132)
* fix(web): active element selection in multi-touch scenarios (#5134)
* chore(windows): cleanup old .dof, .bdsproj and .cfg files (#5131)
* chore(ios): shift to use of XCFrameworks (#5107)
* chore(ios): imports iOS artifact-upload script for CI use (#5140)
* refactor(ios/engine): "should reload keyboard" now a property of the keyboard (#4847)
* docs(windows): Initial commit - README.md (#5119)
* chore(windows): move to vc++ 2019 (#5143)

## 15.0.52 alpha 2021-05-24

* chore(windows): Chromium 89.0.18 (#5128)
* chore(android/samples,oem/fv/android) Update dependencies (#5127)
* feat(web): basic promise wrapper on cloud api call (#5121)

## 15.0.51 alpha 2021-05-21

* chore(windows): reorganise tests (#5084)
* feat(developer): Add 'full copyright' field to templates (#5085)
* fix(developer): TframeTextEditor.SetText was not synchronous (#5096)
* chore(android): Update Gradle and other dependencies (#5098)
* chore(web): add test page for playing with CKEditor (#5089)
* chore(ios): update fv cert ref (#5123)
* chore(mac): Add help links for installing Keyman and keyboards (#5108)
* chore(android): Update targetSDKVersion to 30 (#5126)

## 15.0.50 alpha 2021-05-20

* chore(common): Check in crowdin strings for Amharic (#5102)
* chore(common): resolve audit issues in Web-related repo NPM packages (#5092)
* refactor(linux): Refactor `install_kmp.py` (#4932)
* chore(linux): Swap order of dependency (#5113)
* chore(linux): Remove unnecessary dependency on kmflcomp (#5115)

## 15.0.49 alpha 2021-05-19

* refactor(linux): Remove obsolete SCIM kmfl imengine (#5093)
* fix(common/core/desktop): Fix warnings when compiling for armhf (#5099)
* fix(common/core/desktop): Don't segfault on invalid .kmx file (#5101)

## 15.0.48 alpha 2021-05-17

* chore(common): Update stable history for 14.0.274 (#5087)
* fix(web): position popups correctly in landscape mode on Android and during Chrome emulation (#5075)

## 15.0.47 alpha 2021-05-14

* fix(developer): kmconvert commandline and deploy (#5082)

## 15.0.46 alpha 2021-05-13

* chore(windows): remove unused keymanx64 parameter (#5062)
* fix(android/engine): Check lexical-model file exists before using (#5071)
* chore(web/resources): bump lodash from 4.17.19 to 4.17.21 in /web/testing/regression-tests (#5037)

## 15.0.45 alpha 2021-05-12

* fix(windows): refactor controller windows (#5060)
* chore(windows): remove KMC_CHANGEUISTATE (#5061)
* fix(linux): Swap order of dependencies for Debian package (#5064)
* fix(linux): Fix Caps Lock (#5054)

## 15.0.44 alpha 2021-05-11

* chore(windows): FixupMissingFile needed current component code (#5057)
* chore(windows): remove unused utilrun unit (#5056)
* fix(developer): two small issues in kmdecomp (#5031)
* fix(common/core): Add values for Caps and Nocaps modifiers (#5053)

## 15.0.43 alpha 2021-05-10

* chore(mac): update requirements to include Big Sur (#5030)
* feat(android/app): Add telemetry for launching WebBrowserActivity (#5046)
* chore(android/app): Always use offline help (#5047)
* fix(windows): avoid disabling Keyman when speech recognition starts (#5000)
* chore(windows): add telemetry to trace crash on exit (#5007)
* fix(windows): make keymanx64 responsible for its own lifecycle (#5002)

## 15.0.42 alpha 2021-05-07

* chore(common): Check in crowdin files for Azerbaijani (#4992)

## 15.0.41 alpha 2021-05-06

* chore(linux): Add support for Ubuntu 21.04 (hirsute) (#5033)

## 15.0.40 alpha 2021-05-02

* chore(common): update stable history for 14.0.273 (#5003)

## 15.0.39 alpha 2021-04-30

* chore: support for xcode 12 (#4995)

## 15.0.38 alpha 2021-04-29

* fix(android/engine): Fix toHex() for null string (#4991)

## 15.0.37 alpha 2021-04-28

* fix(windows): handle errors starting keymanx64 (#4989)

## 15.0.36 alpha 2021-04-26

* docs(ios): tweaks prereqs in readme (#4977)
* fix(linux): Fix crash with incomplete metadata (#4908) (#4971)
* chore(android/engine): Don't use localized string for Sentry errors (#4978)
* fix(web): Make banner initialization more robust (#4961)
* chore: disable findTouchAliasElement logging (#4981)
* chore(common): update stable history for 14.0.272 (#4975)
* chore: Add cherry-pick label for cherry-pick PRs (#4973)

## 15.0.35 alpha 2021-04-23

* chore(ios): prep for CI transition to Xcode 12, build script tweak (#4967)
* chore(linux): Fix triggering of Jenkins builds for stable branch (#4969)
* fix(linux): Don't crash if kmp file vanishes (fixes #4907) (#4970)

## 15.0.34 alpha 2021-04-22

* No changes made

## 15.0.33 alpha 2021-04-22

* fix(oem/fv/android): Migrate keyboard list from 12.0 to 14.0 (#4951)
* fix(web): publish restorePosition() function (#4946)
* fix(web): fixes subkey lookup for fat-finger processing (#4954)
* feat(linux): Improve Sentry crash reporting (#4914)
* fix(web, ios): better SMP, emoji handling with frequent keyboard swaps (#4938)

## 15.0.32 alpha 2021-04-21

* fix(windows): handle invalid package names during install (#4887)
* fix(windows): crash when installing TIP in some rare situations (#4890)
* chore(windows): disable profile repair (#4899)
* fix(windows): access violation closing text editor (#4920)
* fix(windows): help contents broken from tray menu (#4922)
* fix(developer): avoid crash if .kpj.user file is malformed (#4918)
* fix(developer): chiral mismatch warning is disruptive (#4934)
* fix(windows): avoid error if keyman32.dll renamed (#4940)
* change(web): adds error + console logs usable for Sentry reporting targeting #4797 (#4821)

## 15.0.31 alpha 2021-04-20

* fix(android/engine): Don't load woff fonts on Android N (#4905)
* fix(android/engine): Change getList() to return an empty list instead of null (#4926)
* fix(android/engine): Don't set lexical model list to null (#4927)
* fix(linux): don't crash on legacy non-Unicode files (#4906)
* fix(linux): Don't crash on network problem (fixes #4911) (#4912)
* chore(linux): Remove experimental scripts (#4913)

## 15.0.30 alpha 2021-04-19

* chore(android/engine): Rename "Pulaar" to "Pulaar-Fulfulde" (#4865)
* fix(developer): Reduce non-canonical BCP 47 tag warning in PackageInfo to "Info" (#4863)
* chore(common): Update crowdin for French (#4895)
* chore(android,windows): Check in crowdin for Indonesian (#4829)

## 15.0.29 alpha 2021-04-05

* chore(android/engine): Rename "Fula" to "Pulaar" (#4859)

## 15.0.28 alpha 2021-04-02

* fix(linux): Fix crash if `<kbd>.json` doesn't contain description (#4851)

## 15.0.27 alpha 2021-04-01

* docs(android): Update installing-keyboards.md (#4837)
* chore(deps): bump y18n from 4.0.0 to 4.0.1 in /web/testing/regression-tests (#4817)
* fix(android): ensure keyboard is always set after pageLoaded (#4840)
* chore(deps): bump y18n from 4.0.0 to 4.0.1 in /resources/build/version (#4818)
* fix(developer): buffer size for range expansions (#4831)
* chore(common): Check in crowdin for Fulah (#4846)
* docs(android): Update image for enabling-system-keyboards (#4844)
* chore: history from 14.0.271 (#4849)

## 15.0.26 alpha 2021-03-31

* fix(android/engine): Add KMString wrapper for formatting Strings (#4813)

## 15.0.25 alpha 2021-03-30

* fix(developer): requote font names (#4814)

## 15.0.24 alpha 2021-03-29

* fix(ios): ensures JS keyboard set after page load (#4808)
* fix(developer): open containing folder was not opening correct folder (#4776)
* fix(linux): Fix crash if query doesn't contain bcp47 tag   (#4800) (#4811)

## 15.0.23 alpha 2021-03-26

* fix(ios): fixes sys-kbd setup help link for iOS 9 and 10 (#4774)
* fix(android): Fix NullPointerException in package installation (#4790)

## 15.0.22 alpha 2021-03-25

* fix(ios/samples): samples should use package-oriented API (#4771)
* fix(android/engine): Sanitize embedded KMW Sentry error (#4782)

## 15.0.21 alpha 2021-03-23

* chore: beta to alpha A15S1 (#4759)

## 15.0.20 alpha 2021-03-22

* chore: B14S8 beta to alpha (#4739)
* chore: merge beta to master (A15S1) (#4745)

## 15.0.19 alpha 2021-03-09

* chore: beta to alpha merge, B14S7 (#4624)

## 15.0.18 alpha 2021-02-25

* fix(linux): Set help-keyman.com.sh executable (#4530)

## 15.0.17 alpha 2021-02-22

* chore: merge B14S6 beta to alpha (#4503)

## 15.0.16 alpha 2021-02-11

* chore: B14S6 beta to master merge (#4473)
* chore: manual version increment (#4478)

## 15.0.15 alpha 2021-02-11

* chore: merge B14S5 beta to master (#4432)

## 15.0.14 alpha 2021-02-02

* docs(linux): Improve packaging doc (#4389)

## 15.0.13 alpha 2021-02-01

* chore(linux): Don't report on Sentry when running unit tests (#4374)
* chore(linux): Pass second tag parameter to Jenkins build (#4376)

## 15.0.12 alpha 2021-01-28

* chore(linux): Improve launchpad.sh script (#4355)

## 15.0.11 alpha 2021-01-27

* chore: Enhance PR labeling based on PR title (#4226)

## 15.0.10 alpha 2021-01-27

* fix(ios): renew distribution certificate (#4342)

## 15.0.9 alpha 2021-01-25

* chore(windows): help for 14.0, part 1 (#4109)
* chore: merge B14S4 beta to master (#4320)

## 15.0.8 alpha 2021-01-18

* chore(linux): Allow to push to the `keyman-beta` ppa (#4271)

## 15.0.7 alpha 2021-01-12

* chore(linux): Update debian metadata (#4223)

## 15.0.6 alpha 2021-01-07

* fix(linux): only pass tag if we just created one (#4220)

## 15.0.5 alpha 2021-01-05

* chore(linux): pass tag to Jenkins build (#4160) (#4215)

## 15.0.4 alpha 2020-12-23

* feat(common/models): naive backspace workaround, naive multi-char Transform mitigation (#4206)
* fix(web): supplies missing (optional) argument to prevent warnings (#4208)
* fix(ios): moves Settings notification UI code to main thread (#4210)

## 15.0.3 alpha 2020-12-21

* fix(web): Solving kmwosk color inconsistency (#4154)

## 15.0.2 alpha 2020-12-16

* fix(common): increment version needs to check base (#4156)

## 15.0.1 alpha 2020-12-14

* chore: prepare 15.0 alpha (#4129)

## 14.0.292 stable 2022-05-24

* fix(windows): sentry debuglogging for DoInstall (#6648)

## 14.0.291 stable 2022-05-07

* fix(windows): don't log before checking nil when installing keyboard (#6506)
* fix(windows): put correct HK_ALT flag for modifier (#6587)

## 14.0.290 stable 2022-03-31

* chore(oem/fv/android): Update dependencies (#6452)

## 14.0.289 stable 2022-03-30

* chore(linux): Remove versions that are no longer supported (#6388)
* chore(windows): move crash metadata to extra (#6398)
* fix(android/engine): Test fileVersion during package install (#6423)
* fix(mac): invalid keyboard breaks configuration (#6420)
* chore(android):   Update Play publishing plugin to 3.5.0 and other plugins (#6224)
* fix(web): track load error detail (#6443)

## 14.0.288 stable 2022-03-15

* chore(linux): Update changelogs for 14.0.287 (#6373)

## 14.0.287 stable 2022-03-04

* chore(ios): certificate update (#6210)
* fix(windows): create Keyman/Diag folder in redirected profile (#6222)
* fix(web): stop masking error (#6332)

## 14.0.286 stable 2022-01-27

* chore(linux): Update changelogs for 14.0.284 (#6131)
* chore(linux): Revert workaround for Python bug (#6134)
* fix: only auto-merge if `auto:` prefix in title (#6148)
* fix(developer): work around devDependencies bug in npm (#6142)
* chore(ios): certificate update (#6166)
* fix(mac): Check for CODE_SETSYSTEMSTORE (#6170)

## 14.0.285 stable 2022-01-20

* fix(linux): Fix lintian errors  (#6107)
* fix(linux): Add workaround for Python bug  (#6125)
* fix(web): Use regex to determine display layer and functional layers (#6123)

## 14.0.284 stable 2022-01-11

* chore(android,oem/fv/android): Update targetSDKVersion to 30 (#5934)
* fix(web): popupBaseKey null check (#5948)
* fix(linux): Add test targets to Makefile (#5975)
* fix(android/engine): Fix font paths (#5990)
* chore(linux): Remove lintian warning (#5993)
* chore(linux): Allow to specify debian revision (#5998)
* chore(linux): Update changelogs for 14.0.283  (#6007)
* fix(linux): fix release version number for Sentry reporting  (#6053)
* chore(common): Check in crowdin strings for Spanish (Latin America) (#6060)
* fix(linux): fix release version number for Sentry reporting  (#6069)
* chore(android/samples): Add -no-daemon flag to KMSample2 build script (#6083)
* fix(linux): Fix attribute error  (#6087)

## 14.0.283 stable 2021-11-17

* chore(windows): fix broken links in help (#5766)
* chore(linux): copy Keyman for Linux 15 reference to 14 (#5764)
* fix(linux): Fix debian package script (#5772)
* chore(common): Enhance cherry-pick labeling (#5773)
* fix(common): Fix cherry-pick labeling () (#5783)
* fix(linux): Don't crash displaying keyboard details (#5757)
* chore(linux): Update changelog files for 14.0.282 (#5793)
* fix(linux): Don't crash with non-keyboard package file (#5754)
* chore: sentry.io dsn  ‍ ️ (#5788)
* fix(windows): handle edge cases using default language (#5775)
* fix(ios): move sentry settings responsibility to build agent  ‍ ️ (#5806)
* fix(web): remove canvas use for iOS compatibility (#5916)

## 14.0.282 stable 2021-09-27

* fix(mac): add support for M1 processor (#5737)
* chore(mac): Add help links for installing Keyman and keyboards (#5748)
* fix: help.keyman.com script file cleanup (#5750)

## 14.0.281 stable 2021-09-17

* fix(common/models): keep/suggestion diacritic sensitivity when de-duping (#5552)
* feat(ios): adds option for Simulator-compatible testing artifact (#5569)
* chore(ios,windows): Update crowdin strings for Azerbaijani (#5487)
* fix(oem/fv/ios): build configuration for 14.0's test-oriented simulator artifact (#5596)
* fix(linux): Handle corrupt icon file (#5589)
* fix(linux): Check for valid kmp file (#5590)
* chore: fixup stable HISTORY.md (#5605)
* chore(linux): Improve debian package script (#5624)
* chore(linux): Fix uploading to Sentry (#5625)
* chore(linux): Update changelog files (#5636)
* fix(ios): iOS 13 and 14 only - stuck settings toggles (#5611)
* fix(web): Fix layers for embedded longpress keys (#5651)
* fix(linux): Use first keyboard language if none given (#5657)
* fix(android/engine): Fix backspace on Android 5.0 (#5674)
* chore(common): report-history (#5678)
* fix(developer): ensure file modified after import from layout (#5677)
* test(windows): investigate TIP crash (#5679)
* fix(common/core/web): Remove empty rows in OSK (#5703)
* fix(windows): fallback to filename if `&name` not set (#5685)

## 14.0.280 stable 2021-08-02

* fix(android): displayName for keyboard was not optional (#5492)
* chore(linux): Remove Groovy builds (#5503)
* chore(linux): Fix dependency versions (#5504)
* fix(common/core/web): error from early fat-finger termination due to OS interruptions (#5491)
* chore(android): log fontpath error (#5517)
* fix(ios/engine): engine migration always precedes installs (#5501)
* fix(common/core/web): OSK state-key management (#5494)
* fix(linux): fix crash trying to display QR code (#5528)
* fix(android): avoid error with empty font data (#5534)

## 14.0.279 stable 2021-07-22

* fix(ios): keyboard swapping (#5476)

## 14.0.278 stable 2021-07-20

* change(common/core/web): fat-finger-performance unit-test threshold (#5417)
* fix(linux): Improve bitmap conversion (#5410)
* fix(linux): Save QR code in temporary directory (#5411)
* fix(linux): Fix restarting ibus when running with `sudo` (#5413)
* fix(linux): Don't crash with corrupt keyboard (#5421)
* fix(web): fixes osk resize-popping effect (#5428)
* fix(android/engine): register lexical model in switchToNextKeyboard (#5447)
* feat(web): spacebar text controls (#5406)
* feat(android): Spacebar text controls (#5407)
* feat(ios): add spacebar text controls (#5408)
* chore(android): improve error reporting for kmw (#5469)

## 14.0.277 stable 2021-06-29

* chore(common): bash version for run-required-test-builds.sh (#5268)
* chore(common): Add YouTube links to Keyman 14 features (#5285)
* fix(common/models): predictive-text engine use of NFD input (#5286)
* fix(developer): kmconvert help match reality (#5299)
* chore(linux): Update changelogs to match Debian (#5303)
* fix(mac): .kmx max file version is now 0x0E00 (14.0) (#5331)
* fix(mac): kmp.inf is windows-1252 (#5330)
* fix(windows): improve keymanx64 start stability (#5222)
* chore(linux): Build also on riscv64 to facilitate migration on Ubuntu (#5322)
* fix(android): fixes application of nextlayer for subkeys with customized layer setting (#5351)
* feat(common/core/web): engine correction-prep optimizations (#5352)
* fix(linux): Add libglib2.0-bin to keyman Depends (#5359)
* fix(windows): osk scaling mismatch on horz axis (#5342)
* fix(linux): Don't add duplicate entries when reinstalling keyboard (#5369)
* chore: keyboard_info.source.json 1.0.6 (#5380)
* feat(android): hide textarea for perf (#5377)
* change(ios): setText now uses JSON serialization (#5333)

## 14.0.276 stable 2021-06-11

* fix(linux): Exclude s390x from package builds for ibus-keyman (#5220)
* fix(web): Always pass kill_browserstack (#5235)
* feat(android): Automatically install keyboard through Play Store (#5231)
* fix(oem/fv/ios): app encryption flag for app store uploads (14.0) (#5243)
* fix(common/core/web): optimizes transform construction (#5255)
* fix(android): rotation is not updating keyboard (#5262)
* feat(android): silent install for referred kbd (#5266)
* fix(windows): disable new hotkey modifier check (#5270)

## 14.0.275 stable 2021-06-04

* fix(linux): Swap order of dependencies for Debian package (#5070)
* fix(android/engine): Check lexical-model file exists before using (#5080)
* fix(developer): kmconvert commandline and deploy (#5083)
* fix(web): position popups correctly in landscape mode on Android and during Chrome emulation (#5090)
* fix(common/core/desktop): Fix warnings when compiling for armhf (#5109)
* fix(common/core/desktop): Don't segfault on invalid .kmx file (#5110)
* chore(common): Check in crowdin strings for Amharic (#5117)
* chore(linux): Swap order of dependency (#5114)
* fix(developer): TframeTextEditor.SetText was not synchronous (#5097)
* chore(ios): update fv cert ref (#5124)
* chore(ios): works around wrong-workspace Carthage 0.38 lookup issue (#5135)
* fix(web): multi touch handling (#5142)
* chore(ios): artifact prep script now in-repo (#5141)
* chore(web): Add CI script to kill BrowserStack tunnel (#5152)
* fix(ios): artifact upload preparation (#5161)
* chore(android): Add -clean flag to build script (#5177)
* fix(windows): avoid cached hotkey state (#5189)
* fix(android/samples): Set gradlew executable for Tests (#5199)
* fix(common/core/desktop): Fix failing tests on armhf (#5204)
* feat(web): Add script to deploy KeymanWeb release to s.keyman.com (#5179)
* chore(ios): renames problematic app selectors (#5174)
* fix(developer): support spaces in regression tests (#5218)

## 14.0.274 stable 2021-05-11

* fix(android/engine): Fix toHex() for null string (#4997)
* chore: support for xcode 12 (#4996)
* chore(linux): Add support for Ubuntu 21.04 (hirsute) (#5035)
* chore(ios): prep for CI transition to Xcode 12, build script tweak (#5048)
* chore(windows): add telemetry to trace crash on exit (#5008)
* fix(windows): avoid disabling Keyman when speech recognition starts (#5038)
* chore(android/app): Always use offline help (#5051)
* chore(common): Check in crowdin files for Azerbaijani (#5039)
* feat(android/app): Add telemetry for launching WebBrowserActivity (#5050)
* fix(linux): Enable caps lock support (#5058)

## 14.0.273 stable 2021-04-26

* chore(linux): Fix triggering of Jenkins builds for stable branch (#4968)
* fix(linux): Don't crash if kmp file vanishes (fixes #4907) (#4972)
* fix(linux): Fix crash with incomplete metadata (#4908) (#4979)
* chore(android/engine): Don't use localized string for Sentry errors (#4980)
* chore: disable findTouchAliasElement logging (#4982)
* fix(web): Make banner initialization more robust (#4983)
* chore: Add cherry-pick label for cherry-pick PRs (#4984)

## 14.0.272 stable 2021-04-23

* chore(linux): Debug triggering Jenkins build (#4852)
* chore(android/engine): Rename "Fula" to "Pulaar" (#4860)
* chore(android/engine): Rename "Pulaar" to "Pulaar-Fulfulde" (#4891)
* fix(developer): Reduce non-canonical BCP 47 tag warning in PackageInfo to Info (#4892)
* chore(common): Update crowdin for French (#4903)
* chore(android,windows): Check in crowdin for Indonesian (#4904)
* fix(linux): don't crash on legacy non-Unicode files (#4878)
* fix(android/engine): Don't load woff fonts on Android N (#4924)
* fix(linux): Don't crash on network problem (fixes #4911) (#4931)
* fix(android/engine): Change getList() to return an empty list instead of null (#4928)
* fix(windows): handle invalid package names during install (#4888)
* fix(windows): crash when installing TIP in some rare situations (#4901)
* fix(windows): access violation closing text editor (#4921)
* fix(windows): help contents broken from tray menu (#4923)
* fix(developer): avoid crash if .kpj.user file is malformed (#4919)
* fix(developer): chiral mismatch warning is disruptive (#4935)
* chore(windows): disable profile repair (#4900)
* fix(windows): avoid error if keyman32.dll renamed (#4941)
* change(web): adds error + console logs usable for Sentry reporting targeting (#4929)
* fix(web): fixes subkey lookup for fat-finger processing (#4955)
* fix(oem/fv/android): Migrate keyboard list from 12.0 to 14.0 (#4952)
* fix(web): publish restorePosition() function (#4957)
* fix(web, ios): better SMP, emoji handling with frequent keyboard swaps (#4958)
* feat(linux): Improve Sentry crash reporting (#4960)

## 14.0.271 stable 2021-04-01

* fix(android/engine): Sanitize embedded KMW Sentry error (#4786)
* fix(ios/samples): samples should use package-oriented API (#4772)
* fix(ios): fixes sys-kbd setup help link for iOS 9 and 10 (#4775)
* fix(android): Fix NullPointerException in package installation (#4796)
* fix(ios): ensures JS keyboard set after page load (#4809)
* fix(developer): open containing folder was not opening correct folder (#4777)
* fix(linux): Fix crash if query doesn't contain bcp47 tag (#4800) (#4801)
* fix(android/engine): Add KMString wrapper for formatting Strings (#4820)
* fix(developer): requote font names (#4815)
* fix(android): ensure keyboard is always set after pageLoaded (#4841)
* fix(linux): Fix crash if `<kbd>.json` doesn't contain description (#4835)
* fix(developer): buffer size for range expansions (#4832)
* chore(common): Check in crowdin strings for Fulah (#4822)
* docs(android): Update installing-keyboards and enabling-system-keyboards (#4842)

## 14.0.270 stable 2021-03-23

* chore: stable tier (#4763)

## 14.0.269 beta 2021-03-23

* chore: history: (#4760)
* fix: support new branches in increment-version (#4761)

## 14.0.268 beta 2021-03-23

* fix(windows): Change TLangSwitchRefreshWatcher ownership from thread to form (#4752)
* fix(common/models): prediction threshold when count is low, unintentional aliasing (#4754)

## 14.0.267 beta 2021-03-22

* docs(ios): Help for 14.0 features (#4741)
* chore: increment-version.sh for stable (#4742)

## 14.0.266 beta 2021-03-19

* chore(android/app): Migrate launcher icon to adaptive (#4720)
* fix(common/core/web): predictive banner activation logic (#4713)
* fix(web): allows refresh of currently-loaded model (#4728)
* fix(ios): kbd search now caches publishing data for result (#4729)
* fix(linux): Fix switching to keyboard in middle of line (#4678) (#4721)
* fix(windows): ensure profiles are reinstalled during setup (#4727)
* fix(windows): Avoid double start during setup (#4726)

## 14.0.265 beta 2021-03-18

* fix(common/core/web): disambiguation of keys sharing same base key ID (#4703)
* fix(windows): Download keyboard dialog TLS protocol support (#4714)
* fix(web): first-pass workaround for popup-key corrections (#4704)
* fix(developer): use var not let in definitions (#4718)
* chore(windows): finalize help (#4717)

## 14.0.264 beta 2021-03-17

* fix(android/app): Load system keyboard before checking overrides (#4696)
* fix(android): Update styling and set text to black (#4662)

## 14.0.263 beta 2021-03-16

* fix(ios): keyboard search did not alert for lack of net (#4693)
* chore(android/engine): Send KMW console errors to Sentry (#4692)
* fix(windows): start Keyman after setup only if not running (#4681)
* fix(developer): &CasedKeys and &MnemonicLayout are not compatible together (#4690)
* fix(android/engine): Fix crash when download file fails to copy to cache (#4697)
* fix(developer): validate keyboard_info should give info on non-canonical bcp47 (#4689)
* chore(developer): upgrade bcp 47 canonicalisation to warning (#4699)
* fix(developer): support named character codes at end of line (#4691)

## 14.0.262 beta 2021-03-16

* fix(android): Update styling and set text to black (#4662)
* fix(mac): prevent duplicate keyboards in list (#4674)

## 14.0.261 beta 2021-03-15

* fix(windows): Trigger language sync after changes (#4663)
* chore(linux): Ignore buildtools in keyman-config tarball (#4675)

## 14.0.260 beta 2021-03-15

* chore(windows): fully disable auto start task (#4658)
* fix(windows): keep online update in focus (#4661)
* fix(windows): setup must save install state for restarts (#4649)
* fix(ios): removal of default keyboard respected when updating app (#4651)
* feat(ios): auto-bundles the most recent version of default resources (#4656)
* fix(mac): register configuration window on load (#4667)
* chore(ios): Set Localizable.strings to UTF-8 (#4670)
* fix(android/app): Fix crash when clicking QR code (#4664)
* fix(android): fixes errors within lists used for UI (#4666)
* chore(common): Update crowdin for km (#4671)
* chore(android/app,ios/app): Update crowdin for de (#4672)
* fix(mac): release window resources on close (#4669)
* fix(mac): show local help in app (#4673)
* chore(windows): Update whatsnew.md (#4665)

## 14.0.259 beta 2021-03-12

* chore(developer): add B11 ISO code for ABNT2 keyboard (#4654)
* fix(web): better check for missing MutationObserver type (#4646)
* fix(android/engine): Reset in-app context when selection changes (#4636)
* chore(linux): Move signature files as well (#4653)

## 14.0.258 beta 2021-03-11

* fix(windows): context help links (#4626)
* fix(windows): hotkeys correctly ignore right modifier keys (#4628)
* fix(windows): Handle disabled profiles and invalid language ids (#4635)
* fix(web): event handling for TouchAliasElement's blinking caret (#4638)
* fix(common/models): prevents "undefined" reversion display string (#4648)
* docs(common): Update npm-packages readme (#4644)

## 14.0.257 beta 2021-03-10

* fix(common/core/web): Add keyboard check for scriptObject (#4640)
* docs(common): Update readme (#4642)

## 14.0.256 beta 2021-03-10

* chore(linux): Update readme (#4633)

## 14.0.255 beta 2021-03-09

* chore(common): Update readme (#4631)

## 14.0.254 beta 2021-03-09

* fix(web): use language code correctly in toolbar (#4620)
* chore(linux): Fix some lintian warnings for Debian package (#4621)
* docs(ios): adds supported l10ns to help/about/whatsnew (#4625)
* fix(windows): represerve keys on setfocus (#4622)
* fix(windows): show version selector for Keyman in installer (#4623)
* fix(windows): ensure valid base layout on install (#4627)

## 14.0.253 beta 2021-03-05

* fix(android/app): Fix welcome.htm responsiveness (#4531)
* feat(developer): &CasedKeys system store (#4586)
* fix(android): Localize Toast notifications (#4588)
* fix(windows): PreservedKeyMap::MapUSCharToVK line order bug (#4595)
* fix(developer): tidy up expansions tests (#4592)
* fix(ios): adds i18n for some error alerts (#4577)
* fix(windows): incxstr could run over buffer with malformed data (#4596)
* chore(android/app): Update whatsnew with available display languages (#4610)
* chore(linux): Improve Sentry environment setting (#4589)
* fix(developer): Expand filenames before load (#4606)

## 14.0.252 beta 2021-03-04

* feat(ios): enables de, fr, and km localizations (#4585)
* fix(developer): improve CEF location search stability (#4571)
* fix(developer): Support all fonts in Keyboard Fonts dialog (#4574)
* feat(developer): Add different Open Containing Folder buttons (#4576)
* feat(developer): Range expansions (#4584)
* fix(web): fixes lack of respect for underlying-key display settings (#4572)
* fix(common/models): fixes application of suggestions immediately after a backspace (#4587)
* fix(linux): Improve version number (#4582)
* chore(linux): Don't report to Sentry in dev environment (#4581)

## 14.0.251 beta 2021-03-03

* fix(developer): Improve stability of named code constants (#4547)
* fix(developer): schema conformance for model package compiler (#4548)
* fix(developer): touch layout osk import handling of multiple modifiers (#4552)
* fix(developer): require language tag when compiling keyboard package (#4563)
* fix(developer): Avoid blank keys when importing KMX to KVKS (#4564)
* feat(developer): isRTL support for lexical model editor (#4559)
* fix(developer): track modified state in wordlist editor better (#4562)
* chore(ios): better visual feedback for keyboard search during poor internet connectivity (#4573)
* chore(common): Update crowdin files for `de` (#4578)
* chore(common/core/desktop): write debug output to console (#4569)

## 14.0.250 beta 2021-03-02

* fix(common/resources): Fix help.keyman.com path for CI (#4565)

## 14.0.249 beta 2021-03-01

* fix(web): mnemonic keystrokes w FF keymapping (#4540)
* chore(ios/app): Adjust help titles for installing custom dictionaries (#4550)

## 14.0.248 beta 2021-02-26

* fix(common/models): predictions after context reset / caret shift (#4411)
* change(oem/fv/ios): FV keyboards now package-based (#4471)
* fix(windows): Handle Caps Lock event correctly from TIP (#4536)
* fix(developer): run even if sentry unavailable (#4537)
* fix(developer): UTF-8 messages in LM compiler (#4539)
* feat(common/models): mid-context suggestions & reversions, fix(common/models): correction-search SMP issues (#4427)
* fix(ios): package installer completion requires welcome dismissal (#4543)
* fix(windows): Refresh settings on 64-bit apps (#4378)
* fix(windows): prevent re-registration of TIPs on 14.0 upgrade (#4535)

## 14.0.247 beta 2021-02-25

* fix(web): keyboard documentation patch-up (#4512)
* fix(web): removes package namespacing from kbd's CSS class (#4516)

## 14.0.246 beta 2021-02-24

* chore(common): allow forced version increment (#4522)

## 14.0.245 beta 2021-02-24

* fix(common/core/web): core key-processing now always returns RuleBehavior type. (#4508)
* fix(common/resources): Set help-keyman.com.sh executable (#4510)
* fix(developer/compilers): fixes error when "constructor" is in wordlist (#4504)
* fix(web): hides touch-alias caret when keystroke causes focus change (#4514)

## 14.0.244 beta 2021-02-22

* fix(common/models): merges identical suggestions after casing (#4502)
* fix(web): macOS 11 agent string parsing (#4497)
* fix(ios): app logging messages were transient, never stored (#4500)
* chore(ios): web-side sentry enablement try-catch (#4492)

## 14.0.243 beta 2021-02-12

* change(ios/app): Generate offline help from markdown (#4470)
* fix(windows): tsysinfox64 not signed (#4486)
* chore(android/app): Update help formatting and images (#4485)
* fix(android/engine): Display welcome.htm help within the app (#4477)
* fix(ios): tutorial's link to "Add a Keyboard" links directly to keyboard search (#4491)
* feat(linux): Improve output of km-package-list-installed (#4481)

## 14.0.242 beta 2021-02-11

* fix(android/engine): Display online keyboard help (#4462)
* fix(windows): Track modifier changes in UWP apps (#4468)
* fix(common/resources): Fix help.keyman.com path for commit (#4469)
* fix(common): create GitHub comments serially (#4472)
* fix(linux): Fix dependencies on packages (#4464)

## 14.0.241 beta 2021-02-10

* fix(common/resources): Just use master branch for help.keyman.com (#4459)
* fix(windows): hotkeys offset in config list (#4454)
* chore(ios): Settings case-statement cleanup (#4443)
* fix(linux): Also use staging site for beta versions (#4455)

## 14.0.240 beta 2021-02-09

* fix(windows): When uninstalling, exit Keyman (#4383)
* fix(common/models): predictions after context reset / caret shift (#4411)
* modify(common): Refactor help-keyman-com.sh script for uploading help files (#4433)
* fix(linux): improve BCP 47 canonicalization (#4439)

## 14.0.239 beta 2021-02-08

* fix(developer): debug information with unicode identifiers (#4408)
* fix(developer): Compiler check for if and nul at start of context (#4410)
* feat(developer): improve BCP 47 canonicalization (#4425)
* chore(linux): Check in markdown help for Linux (#4414)

## 14.0.238 beta 2021-02-08

* fix(android/engine): Remove WRITE_EXTERNAL_STORAGE from manifest (#4434)
* chore(common): Check in crowdin files for French (#4420)

## 14.0.237 beta 2021-02-06

* fix(linux): Fix packaging (#4428)

## 14.0.236 beta 2021-02-04

* chore(windows/resources): Fix typo about Community Forum link (#4412)
* change(android/app): Separate displaying welcome.htm from keyboard installation (#4413)
* fix(linux): Improve fix for #3399 (#4418)

## 14.0.235 beta 2021-02-03

* fix(windows): Ensure UAC window comes to foreground (#4384)
* fix(windows): setup should ignore cert revocation (#4392)
* fix(developer): don't check both setup.exe and setup-redist.exe (#4398)
* fix(windows): Show balloon when Keyman is already running (#4386)
* fix(developer): Generate platforms correctly from template (#4399)
* fix(developer): Import Keyboard support for Targets (#4400)
* fix(ios): fixes unit test mocking, test init (#4394)
* fix(ios): fixes app crash on network/install error during resource updates (#4395)
* fix(ios): fixes package-install/update event concurrency management (#4396)
* fix(common/models): bksp workaround now works beyond first word (#4401)
* chore(windows): tests should be case-sensitive (#4405)
* fix(windows): crash deleting wordlist (#4406)
* fix(windows): Use forward slashes in wordlist paths (#4407)
* modify(android/app): Change build-help.sh to generate offline help from Markdown files (#4397)
* fix(linux): Don't crash if uninstalling last keyboard (#4402)

## 14.0.234 beta 2021-02-02

* fix(windows): remove Show Keyboard Usage hotkey (#4379)
* fix(windows): avoid invalid language codes in Add Language dialog (#4381)

## 14.0.233 beta 2021-02-01

* fix(windows): SMP-aware deletion in TSF-aware apps (#4360)
* fix(common): tweak surrogate pair deletions (#4361)
* fix(windows): Enter and Spacebar handling in Configuration (#4349)
* fix(web): removes stylesheets from unloaded keyboards (#4371)
* fix(android, ios): eliminates OSK layout flashing from predictive text banner display (#4370)
* fix(windows): crash for Sinhala mitigation (#4380)
* chore(linux): Pass second tag parameter to Jenkins build (#4388)

## 14.0.232 beta 2021-01-29

* chore(linux): Don't report on Sentry when running unit tests (#4356)
* fix(windows): Ignore Access Denied error creating task (#4365)
* chore(windows/resources): Fix more titles and TODOs in help (#4367)

## 14.0.231 beta 2021-01-28

* chore(common): Enhance PR labeling based on PR title (#4357)
* change(web): set -eu for web scripts (#4353)
* fix(ios): accidental duplicated line from merge (#4366)

## 14.0.230 beta 2021-01-28

* fix(android/app): Wrap preference screen titles (#4326)
* fix(ios): better handling of scoped vs non-scoped package URLs (#4327)
* fix(web): bulk renderer using video stream capture (#4316)
* change(android/engine): Allow swipe to dismiss update notifications (#4329)
* fix(windows): improve support for strings.xml (#4323)
* fix(windows): invalid character in text content opening help (#4330)
* fix(windows): align title to top in Install Keyboard (#4331)
* chore(windows/resources): Cherry-pick 14.0 help from #4109 (#4335)
* fix(ios): renew distribution certificate (#4344)
* fix(ios): reloads keyboard after package updates (#4347)
* fix(ios): fixes accidental logo / cancel button overlap during package installation (#4332)
* fix(ios): resolves rough edges with installation view transitions (#4338)
* fix(web/ui): propagates UI module build failures (#4352)
* chore(linux): Fix typo (#4341)
* chore(windows/resources): Address more TODO links for help (#4351)
* fix(windows): setup strings comment for language (#4362)
* fix(windows): Splash button sizes (#4348)
* chore: manual version increment (#4363)

## 14.0.228 beta 2021-01-22

* fix(web): uses CSS line-height to vertically center oversized OSK keycaps (#4255)
* fix(web): OSK loading efficiency (#4279)
* fix(web): default popup key selection, space highlight after popup (#4306)
* fix(web): language menu key highlighting (#4308)
* fix(web): dynamic font downscaling for OSK keys (#4270)
* chore(windows/resources): Cleanup Notes and Tips in help (#4307)
* fix(developer): Debug character grid performance (#4237)
* chore(android/samples): Remove old sample keyboard loaded code (#4315)

## 14.0.227 beta 2021-01-21

* fix(web): dynamic font downscaling for OSK keys (#4270)
* fix(web): toolbar and loading optimisations (#4304)
* chore(web): updates MTNT for pred-text testing page to 0.1.5 (casing) (#4305)
* fix(android/samples): Add dependency on androidx.preference (#4310)

## 14.0.226 beta 2021-01-20

* fix(windows, common/core/desktop): context mismatch with if and dk (#4276)
* chore(common): Check in crowdin files for de (German) (#4295)
* feat(android/app): Add option to change display language (#4261)
* fix(developer/compilers): fixes developer build breakage from #4291 (#4299)
* fix(android/app): Handle keyman protocol from external browser (#4292)

## 14.0.225 beta 2021-01-19

* fix(android/samples): Fix Sentry dependencies (#4267)
* fix(web): better font-wait null guards (#4286)
* fix(web): uses CSS line-height to vertically center oversized OSK keycaps (#4255)
* fix(developer): disable TSentryClient on WINE (#4274)
* fix(web): no key previews for special keys reliant on keyboard-specific OSK font (#4282)
* fix(web): default kbd ui name is now generic (#4293)
* fix(developer/compilers): fixes dependency versioning on alpha, beta tiers (#4291)

## 14.0.224 beta 2021-01-18

* fix(android): Popup misalignments and compatibility with WeChat, Telegram (#4254)
* chore(web): sample pages now wait on init's promise for keyboard loading (#4253)

## 14.0.223 beta 2021-01-15

* fix(windows): typo in font helper string (#4251)
* chore(android/app): Add Obolo language from crowdin (#4256)
* fix(web): unexpected errors in OSK / banner position calculations (#4238)
* chore(linux): Improve version number for debian package (#4258)

## 14.0.222 beta 2021-01-14

* chore(common): Check in crowdin files for km (Khmer) (#4228)
* fix(web): OSK key preview positioning (#4241)
* fix(web): disables predictive text on Opera mini (#4243)

## 14.0.221 beta 2021-01-13

* fix(windows): fix menu popup position (#4175)
* fix(windows): Update mitigation for Keyman 14 and Windows 10 19597 (#4180)
* fix(developer): Allow unhandled keys to go through to debugger memo (#4209)

## 14.0.220 beta 2021-01-13

* fix(android/engine): Remove usage of WRITE_EXTERNAL_STORAGE permission (#4170)
* fix(web): osk size & position after focus changes (#4232)
* chore(linux): Update debian metadata based on Debian repos (#4233)

## 14.0.219 beta 2021-01-12

* chore: bugfix cherrypick (#4208, #4210) (#4230)
* fix(common/core/web): mnemonic modifier key-up handling (#4231)

## 14.0.218 beta 2021-01-08

* chore(linux): pass tag to Jenkins build (#4160) (#4224)

## 14.0.217 beta 2021-01-05

* chore(linux): Allow to push to the `keyman-beta` ppa (#4214)

## 14.0.216 beta 2020-12-23

* fix(developer): make touch layout editor source view fonts consistent (#4198)
* fix(developer): Respect tab editor option (#4200)
* fix(web): Solving kmwosk color inconsistency (#4187)

## 14.0.215 beta 2020-12-22

* fix(windows): create task fails (#4181)
* fix(windows): prevent modifier key from navigating in download dialog (#4182)
* fix(windows): keyboard help missing (#4177)
* fix(windows): Proxy Configuration window size (#4159)
* chore(common): update copyright year in various locations (#4197)
* fix(windows): Settings refresh management (#4164)
* fix(windows): Improve refresh performance (#4165)
* fix(windows): exception handling list error (#4166)
* fix(windows): OSK toolbar sync (#4167)
* fix(windows): Show full version with tag in Setup (#4169)
* fix(windows): Improve refresh reliablilty (#4171)
* fix(developer): debug.log created unexpectedly (#4189)
* chore(windows): Remove silent exception in task cleanup (#4191)
* fix(windows/config): keyboard icons missing (#4193)
* fix(developer): Map symbols for *LTREnter* and *LTRBkSp* (#4190)
* fix(developer): Encoding for .model.ts files (#4199)
* feat(common): annotate PRs with build links (#4202)

## 14.0.214 beta 2020-12-20

* fix(windows): Text editor font bugs (#4149)
* fix(windows): Warn if we reach maximum transient languages (#4157)

## 14.0.213 beta 2020-12-18

* fix(windows): sentrytool should fail build on exception and access violation when rewriting executables (#4158)

## 14.0.212 beta 2020-12-17

* fix(windows): uninstall language button z-index (#4145)
* fix(windows): exit button position on setup (#4150)
* fix(windows): Online Update dialog layout was messy (#4152)

## 14.0.211 beta 2020-12-16

* chore(windows): remove obsolete importkeyboard app (#4138)
* fix(windows): glitch in keyboard menu (#4139)
* fix(windows): menu scroll positions need reset at popup (#4140)
* fix(windows): remove obsolete option 'Switch to OSK/Help' (#4141)
* fix(windows): Canonicalize BCP 47 tag on keyboard download (#4144)
* fix(windows): help window centred on load (#4147)
* fix(windows): Text editor font bugs (#4149)
* chore(common): Update history for 14.0 beta release (#4148)
* fix(common): increment version needs to check base (#4155)

## 14.0.210 beta 2020-12-15

* fix(windows): beta uses wrong server (#4142)

## 14.0.209 beta 2020-12-14

* chore(common): fix trigger for beta branches (#4135)

## 14.0.208 beta 2020-12-14

* chore: re-trigger beta (#4133)

## 14.0.207 beta 2020-12-14

* fix: build trigger definitions (#4131)

## 14.0.206 beta 2020-12-14

* feat(ios/app): adds error reporting toggle (#4106)
* chore(android): rework versionCode system (#4128)

## 14.0.205 alpha 2020-12-14

* fix(web): sporadic blank keyboard on Android (#4117)
* chore(common/models): predictive-text "semi-fill" for iOS 9 use (#4118)

## 14.0.204 alpha 2020-12-11

* chore(windows): apply eberhard's suggestions to docs (#4105)
* fix(ios): prevents crash from failed legacy-resource-wrapping attempt (#4100)
* chore(deps): bump ini from 1.3.5 to 1.3.7 in /web/testing/regression-tests (#4108)

## 14.0.203 alpha 2020-12-10

* fix(web/ui): Add null check for calculations when canceling touch (#4098)

## 14.0.202 alpha 2020-12-09

* fix(ios): fixes nav bar issues when using "Install From File" (#4099)
* fix(web/ui): Fix check on indexOf (#4103)
* fix(ios): autosets + autodisplays keyboard after a package install (#4101)

## 14.0.201 alpha 2020-12-08

* chore(ios): pbxproj file compat for Xcode 12 & Simulator (#4094)
* refactor(ios/engine): changes "install from file" to better match Apple guidelines (#4089)
* fix(ios/engine): requests security for imported files (#4095)
* fix(android/app) Change UX to ensure package installation finishes (#4088)
* chore(windows): help titles and missing files (#4091)

## 14.0.200 alpha 2020-12-07

* fix(web/ui): Add check for suggestion.tag (#4085)

## 14.0.199 alpha 2020-12-07

* chore(windows): also build web help (#4086)

## 14.0.198 alpha 2020-12-07

* fix(windows): help deployment (#4083)

## 14.0.197 alpha 2020-12-04

* fix(windows): bootstrap installer not signed (#4067)
* fix(common/core/web): Sanitize embedded KMW Sentry events (#4071)
* fix(windows): keyman: link with = sign is mishandled (#4069)
* chore(android/app,oem/fv/android): Revert #4025 (#4076)
* chore(oem/fv/windows): update oem firstvoices product name (#4052)
* feat(windows): convert Keyman for Windows help to Markdown (#4074)
* chore(windows): remove docbook and libxslt (#4075)
* fix(android/engine): Remove in-app keyboard Sentry log about fallback keyboard (#4078)
* fix(web/engine): findNearestKey erroneously returned key child (#4077)
* fix(common/core/web): predictive context reset (#4072)
* fix(ios/engine): deletion for selected text at the context start (#4080)

## 14.0.196 alpha 2020-12-03

* fix(common): improve increment-version robustness (#4062)
* fix(ios/engine): preserves early context-setting effects (#4070)
* feat(linux): Rename onboard package (#4059)

## 14.0.195 alpha 2020-12-02

* chore(windows): remove obsolete newhelp folder (#4048)
* feat(ios/engine): keyboard-menu scroll indicator now flashes when opened (#4043)
* fix(ios): better meta viewport consistency (#4045)
* fix(windows): Rename to Keyman in help files (#4049)
* chore(common/resources): Fix dest paths for crowdin strings (#3995)
* fix(windows): Rename to Keyman (#4050)
* fix(ios/engine): adds check for keyboard load success, auto-reset on load failure (#4054)

## 14.0.194 alpha 2020-12-01

* fix(android/app,oem/fv/android): Sanitize Sentry navigation breadcrumbs (#4025)
* fix(windows): Task creation and deletion cleanup (#4033)
* fix(windows): crash with package online update (#4034)
* fix(windows): Keep language associations when updating keyboard (#4035)
* fix(windows): Handle network errors when downloading keyboards (#4036)
* chore(windows): disable Sentry 'Started' event (#4037)
* fix(ios): package-internal links should be considered internal (#4022)

## 14.0.193 alpha 2020-11-30

* chore: refresh github templates (#3999)

## 14.0.192 alpha 2020-11-28

* chore(linux/resources): Cleanup formatting in  history.md (#3983)

## 14.0.191 alpha 2020-11-27

* fix(mac): improve robustness of altool call (#3959)
* fix(mac): Handle duplicate filenames for packages (#3961)
* chore(common/resources): Document keeping scopes in sync (#3974)
* fix(common/models): proper RTL quote ordering (#3897)
* chore(mac): englishspanish test keyboard (#3976)
* chore(mac/resources): Catch up mac history for 13.0 releases (#3977)
* fix(android/engine,android/app) Notify when invalid keyboard package fails to install (#3964)
* chore(windows): cleanup unused variable (#3978)

## 14.0.190 alpha 2020-11-26

* fix(oem/fv/android): Check keyboard selected  before allowing setup (#3923)
* chore(common): fixup missing history (#3957)

## 14.0.189 alpha 2020-11-25

* fix(mac): re-sign files post plist update (#3932)
* fix(mac/engine): wrong variable type (#3933)
* feat(mac): Add support for "ISO" keyboard layouts (#3924)
* fix(mac): turn on legacy mode for Java apps (#3944)
* feat(developer): add new touch layout special tags (#3878)
* fix(web): default attachment behavior (#3948)
* fix(mac): arrow keys now reset context (#3946)
* feat(mac): user-controllable legacy app list (#3949)
* chore(mac): fix iso section key code (#3952)
* chore(common/resources): Part 3 of additional HISTORY.md cleanup (#3947)

## 14.0.188 alpha 2020-11-24

* chore(mac): Update README.md (#3880)
* fix(windows): sentry range check error (#3921)
* chore(common/resources): Clean up commit types in HISTORY.md (#3926)
* fix(android/engine): adds null guard to refreshLayouts call (#3927)
* fix(ios/engine): adds null guard to refreshLayouts call (#3927)
* fix(ios/engine): banner inconsistency (#3925)
* chore(web): adds LTREnter, LTRBkSp, and associated layout mapping (#3937)
* fix(web): adds null guards for two Sentry errors (#3941)
* chore(common/resources): additional cleanup to HISTORY.md (#3942)

## 14.0.187 alpha 2020-11-23

* chore(mac): use new keyboard install page (#3908)
* fix(ios/engine): allows some language-code incomplete matches during package installation (#3884)
* feat(ios/engine): launches external links outside the app (#3889)
* chore(linux): Fix launchpad build (closes #3875) (#3913)
* fix(linux): Don't fail installation if restarting ibus fails (#3915)

## 14.0.186 alpha 2020-11-20

* feat(mac): Sentry support (#3886)
* chore(mac): rewrite plists after build (#3891)
* fix(mac): add icon for Keyman.app (#3892)
* fix(mac): codesign resilience (#3893)
* fix(mac): support page and copyright (#3904)
* fix(oem/fv/android): Update Sentry library in FV app (#3905)
* fix(android/app): Install keyboard packages w/o welcome.htm (#3874)
* fix(web/engine): fixes OSK rotation (#3909)
* feat(common/core/web): integrated suggestion casing tests (#3887)
* fix(ios/engine): changes image-banner display logic (#3911)

## 14.0.185 alpha 2020-11-19

* feat(web/engine): allows touch aliasing away from blank keys (#3858)
* chore(linux): Additionally build packages for Ubuntu 20.10 (Groovy) (#3876)
* fix(linux): Remove version.sh and get tier/version from .md files (#3686)
* fix(android/samples): Remove use of version.gradle in Sample projects (#3899)
* feat(android/engine): Add embedded KeymanWeb engine crash reporting to Sentry (#3825)

## 14.0.184 alpha 2020-11-18

* feat(developer/compilers): compiler-side groundwork for applyCasing (#3770)
* feat(common/models): use of applyCasing for suggestions (#3824)
* feat(common/models): casing for suggestions with partial replacement (#3845)
* fix(common/core/web): Add environment to web Sentry reports (#3888)
* fix(android/engine): Update in-app TextView context on pageLoaded (#3867)

## 14.0.183 alpha 2020-11-17

* feat(windows): major version upgrades (#3866)
* feat(developer): support major version upgrades (#3868)
* docs(windows): Update Delphi version requirement note (#3871)
* feat(web/engine): updates osk font, adds layout codes for new glyphs (#3851)
* fix(common/core/web): meta key handling (#3847)
* feat(common/core/web): input processor unit tests (#3836)

## 14.0.182 alpha 2020-11-16

* fix(web/engine): blocks key previews for blank/hidden keys (#3857)
* fix(common/core/web): Fixes engine-level context tests, adds notany cases (#3860)

## 14.0.181 alpha 2020-11-13

* fix(common/core/web): fixes no-output logic check, arrow keys (#3848)
* fix(common/core/web): adds missing null-check (#3859)

## 14.0.180 alpha 2020-11-12

* fix(windows): makefile format error (#3854)

## 14.0.179 alpha 2020-11-12

* fix(windows): Import OSK wrong for European layouts (#3830)
* fix(windows): SizeOfImage header was wrong for dbg (#3833)
* chore(windows): symbol server support (#3834)
* fix(windows): sporadic 8087 control word corruption (#3842)

## 14.0.178 alpha 2020-11-10

* fix(developer): Remove IE dependency from Developer Setup (#3839)

## 14.0.177 alpha 2020-11-07

* fix(developer): support for notany() and context() (#3816)
* fix(web): support for notany() and context() (#3817)
* fix(developer): debug window inherits editor font (#3829)

## 14.0.176 alpha 2020-11-05

* fix(developer): remove obsolete NRSIAllTags (#3819)
* fix(developer): Incorrect script:language map (#3818)
* fix(android/app): Remove network check on "Get Started" menu (#3823)

## 14.0.175 alpha 2020-11-04

* feat(android/engine): Add check for associated model on ModelPickerActivity (#3808)

## 14.0.174 alpha 2020-11-03

* chore(common/resources): improve build README.md (#3812)

## 14.0.173 alpha 2020-10-30

* fix(windows): Cleanup setup.inf processing and CompareVersions function (#3790)
* fix(windows): setup now allows choice of source (#3794)
* fix(windows): show bootstrap progress in setup (#3792)
* fix(windows): cleanup setup action list (#3793)
* chore(android/engine): Remove use of lexical-model catalog (#3803)
* fix(windows): disabling/enabling a profile could have wrong association (#3799)
* fix(windows): keyboard menu could get out of sync (#3800)
* fix(windows): crash installing package with a race (#3805)
* fix(windows): Handle failure on task creation (#3804)

## 14.0.172 alpha 2020-10-29

* fix(windows): Start Keyman on Demand - keyman32 (#3772)
* fix(windows): Start Keyman on Demand - tasks (#3773)
* fix(windows): remove msctf free from DllMain (#3779)
* fix(windows): error reading kmp.inf in setup (#3781)
* chore(web): Update keymanweb-osk.ttf to v. 2.100 (#3782)
* fix(android/engine): Fix issues when re-installing lexical-models (#3731)

## 14.0.171 alpha 2020-10-28

* fix(android/browser): Fix slow input in embedded browser (WebViews) (#3768)
* chore(common/models): fixes context tracking with accepted suggestions (#3767)
* chore(windows): small cleanups (#3774)

## 14.0.170 alpha 2020-10-27

* fix(windows): Remove double refresh (#3754)
* fix(windows): Keyman Configuration changes apply instantly (#3753)
* fix(windows): Make help button work (#3760)
* fix(windows): use new windows url for online update check (#3761)
* fix(windows): Community button had wrong link (#3764)
* fix(windows): Crash in Keep in Touch external link (#3763)
* fix(windows): Fix multiple issues with UI locales (#3766)
* fix(android/engine): Only get keyboard version for cloud/ (#3740)

## 14.0.169 alpha 2020-10-26

* feat(common/core/web): simplify corrective distribution (#3726)
* fix(windows): restore Setup after minimize (#3739)
* fix(windows): buffer overrun in debug function (#3745)
* fix(windows): download error dialog could be blank (#3747)
* feat: Keyman Settings Manager base classes (#2456)
* feat(windows): kmconfig console app (#3732)
* feat(windows): kmconfig GUI (in kmshell) (#3733)
* feat(windows): Apps for Controlling Browser TSF integration (#3734)

## 14.0.168 alpha 2020-10-24

* chore(android): Update dependencies (#3738)

## 14.0.167 alpha 2020-10-23

* fix(android/browser): Improve how embedded browser handles input (#3722)

## 14.0.166 alpha 2020-10-22

* fix(web): disable U+25CC for diacritics (#3039)
* refactor(common/models): Common tokenization and wordbreaking functions (#3706)
* fix(windows): setup status showed only 'removing older versions' (#3735)
* fix(windows): improve tsysinfo upload messages (#3727)
* chore(windows): update msgping to Winsdk 10 (#3728)
* fix(android/engine): Improve KMManager robustness (#3721)
* feat(common/core/web): fat-finger ignores inputs that beep (#3701)

## 14.0.165 alpha 2020-10-21

* fix(web): K_SPACE handling for embedded mode, hardware keystrokes (#3707)

## 14.0.164 alpha 2020-10-20

* feat(common/models): context tracking of accepted Suggestions (#3663)
* feat(common/models): context reversion modeling (#3685)
* fix(windows): RefreshKeyboards loses some profiles (#3714)
* fix(windows): icons missing in Configuration (#3717)
* fix(windows): tweak scrolling in keyboard menu (#3719)
* fix(developer): crash creating basic project (#3716)

## 14.0.163 alpha 2020-10-19

* fix(web): Remove base key from popup keys (#3718)

## 14.0.162 alpha 2020-10-18

* fix(web): Let embedded device handle K_TAB or K_ENTER (#3664)

## 14.0.161 alpha 2020-10-16

* fix(common/core/web): fixes revert event bug (#3709)

## 14.0.160 alpha 2020-10-14

* feat(common/models): disables "keep" when word is not suggestion otherwise (#3700)
* feat(common/core/web): selective wordbreak swallowing after accepting suggestions (#3702)
* refactor(common/models): extract Outcome type (#3705)

## 14.0.159 alpha 2020-10-12

* fix(web): unit test script failure on compilation failures (#3597)

## 14.0.158 alpha 2020-10-09

* feat(common/models): 'revert' now uses model's punctuation (#3647)
* fix(windows): launch configuration non-elevated (#3691)
* fix(windows): disabled keyboards listed in hotkeys (#3693)

## 14.0.157 alpha 2020-10-08

* fix(windows): show version with tag in setup (#3682)
* fix(windows): keyman desktop setup filename (#3684)

## 14.0.156 alpha 2020-10-08

* chore(windows): Move to Windows SDK 10.0.17763.0 (#3654)
* fix(windows): Some registry keys could have incorrect permissions (#3668)
* fix(developer): ci uses repo tier and version (#3670)
* fix(windows): update CI for publishing desktop help (#3671)
* fix(web): ci uses repo tier and version (#3672)
* fix(windows): improve version numbers (#3678)
* chore: don't add tag to version in filenames (#3681)

## 14.0.155 alpha 2020-10-07

* refactor(common/models): centralizes suggestion & keep inits (#3645)
* fix(common/models): predictions after typed whitespace (#3657)
* fix(web): Fix how layer is separated from key name (#3659)

## 14.0.154 alpha 2020-10-05

* fix(windows): Upgrading keyboards with transient profiles (#3637)
* fix(windows): upgrading disabled keyboards (#3638)
* fix(developer): coverity reports for compiler (#3640)
* fix(windows): coverity reports for mcompile (#3641)
* fix(windows): coverity reports for kmtip (#3642)
* fix(windows): coverity reports for keyman32 (#3649)
* chore: exclude parens if no scope in commit msg (#3653)

## 14.0.153 alpha 2020-10-02

* chore(common/resources): bump @actions/core from 1.2.2 to 1.2.6 in /resources/build/version (#3646)
* fix(android/app): Switch system keyboard in KMPBrowserActivity (#3648)

## 14.0.152 alpha 2020-09-30

* fix(ios/engine): package installation language-picker improvements (#3623)

## 14.0.151 alpha 2020-09-29

* fix(windows): Buffer overrun in firstrun (#3634)
* fix(windows): upgrading transitional profiles (#3635)
* fix(android/engine): Fix undetermined lexical model package ID (#3624)

## 14.0.150 alpha 2020-09-28

* feat(windows): overflow menu for osk toolbar (#3626)
* feat(windows): scrollable keyboard menu (#3627)
* fix(android/app): Allow uninstalling sil_euro_latin for non-default languages (#3628)
* feat(ios/engine): Utilizes packages' welcome pages (#3622)
* fix(android/app): Only copy asset .kmp file if it doesn't exist (#3629)

## 14.0.149 alpha 2020-09-25

* fix(windows): add back support for disabling keyboards (#3607)
* fix(windows): bootstrap package install specified language bugs (#3609)
* fix(windows): bootstrap should skip install of failed downloads (#3610)
* refactor(android/app): Move Settings activities from KMEA to KMAPro (#3614)
* fix(android/app): Change install intent to MainActivity (#3615)
* fix(windows): cleanup pointer to int typecasts (#3612)
* fix(developer): hardcoded urls in debugger (#3613)
* fix(developer): Project window and About window hardcoded urls (#3618)
* fix(windows): cleanup hardcoded urls in tsysinfo (#3619)

## 14.0.148 alpha 2020-09-24

* fix(windows): external links should open externally (#3602)
* fix(windows): hint dialog was blank when elevated (#3604)
* fix(android): Log errors for crashes involving Keyboard Picker (#3499)

## 14.0.147 alpha 2020-09-23

* feat(common/models): context + input tracking for predictive text (#3549)
* feat(common/models): initial integration for enhanced corrections (#3555)
* feat(common/models): correction thresholding, acceptance (#3556)
* refactor(common/models): distance modeler cleanup (#3565)
* change(common/models): context tracker cleanup, optimizations, fixes (#3573)
* feat(common/models): Correction improvement (#3575)
* feat(common/models): naive correction-algorithm timer (#3581)
* fix(common/models): Android API compat for upgraded correction-search (#3601)
* fix(android/app): Query api.keyman.com for downloading associated dictionary (#3606)
* refactor(web/engine): application of predictive suggestions (#3582)
* fix(windows): simplify profile repair (#3559)

## 14.0.146 alpha 2020-09-22

* chore(web): bump http-proxy from 1.17.0 to 1.18.1 in /web/testing/regression-tests (#3568)
* feat(ios/engine): go/package use for model pkg downloads (#3603)

## 14.0.145 alpha 2020-09-21

* feat(windows): improve keyboard language dialog (#3557)
* fix(windows): lookup language name on create (#3558)

## 14.0.144 alpha 2020-09-18

* feat(developer): add viewport to html templates (#3531)
* fix(windows): use new signtime.bat on build agents (#3586)
* fix(developer): Touch font size should be string (#3585)
* fix(android/app): Fix Info page title size (#3571)
* fix(linux/config): Check if file exists before creating hard links (#3592)
* fix(windows): remove obsolete releaseCapture calls (#3594)
* fix(linux/config): Catch if kmp.json is invalid JSON (#3593)

## 14.0.143 alpha 2020-09-16

* fix(android/app): Validate language selection for "INSTALL" button (#3579)

## 14.0.142 alpha 2020-09-15

* chore(common/resources): bump node-fetch from 2.6.0 to 2.6.1 in /resources/build/version (#3578)

## 14.0.141 alpha 2020-09-11

* fix(web): default layout shift not changing layer (#3574)

## 14.0.140 alpha 2020-09-09

* feat(common/models): core edit-distance calculation class (#3526)
* feat(common/models): Edit path derivation (#3547)
* feat(common/models): low-level correction-algorithm infrastructure (#3527)
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

* fix(linux): Add packaging of Linux localization files (#3504)

## 14.0.133 alpha 2020-08-25

* feat(common/models): lexicon traversal (#3479)
* feat(common/models): actual priority queue for Trie models (#3480)
* feat(common/models): efficient batch-enqueue (#3486)

## 14.0.132 alpha 2020-08-24

* feat(linux): Add link to share online (#3494)
* docs(common): Clarify l10n readme (#3496)
* feat(linux): Add i18n for Linux (#3492)
* feat(linux): Small UI improvements (#3495)

## 14.0.131 alpha 2020-08-21

* feat(android/app): Add language picker for keyboard package installation (#3481)

## 14.0.130 alpha 2020-08-19

* fix(android): Fix util to getting the tier on CI builds (#3491)

## 14.0.129 alpha 2020-08-17

* feat(windows): rework download keyboard dialog style (#3463)
* chore(common/resources): Add sample vscode settings files (#3249)
* fix(linux): Lookup language tag from keyboard (#3408)

## 14.0.128 alpha 2020-08-13

* feat(linux): Hook up Sentry for km-config (#3378)
* chore(linux): Use new staging site names and use variable for downloads.keyman.com (#3406)

## 14.0.127 alpha 2020-08-12

* refactor(common/models): LMLayer state management tweak, persistent ModelCompositor (#3477)

## 14.0.126 alpha 2020-08-10

* fix(android): Add CI script to publish to Play Store (#3469)
* fix(android): Update sentry plugin to remove obsolete API use (#3471)
* feat(ios/app): universal links for keyboard installation (#3466)
* fix(ios/engine): Proper associating-installer deinitialization, cancellation tests (#3468)
* refactor(ios): error definitions, i18n (#3470)
* chore(common/core/desktop): cleanup keyboardprocessor.h.in (#3473)

## 14.0.125 alpha 2020-08-07

* fix(android): Update Gradle wrapper to 5.6.4 (#3467)
* feat(ios/engine): Associating package installer (#3458)
* feat(ios/app): Associating package installer use within existing install paths (#3465)

## 14.0.124 alpha 2020-08-06

* feat(windows): set default UI language at install (#3438)
* feat(windows): i18n for Setup (#3444)
* feat(windows): add globe icon to Configuration UI (#3446)
* feat(windows): set UI language from Setup preference on first install (#3447)
* feat(windows): docs on editing translations (#3448)
* change(android): Improve string context to help crowdin translators (#3457)
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

* chore(common): unify crowdin.yml (#3418)
* fix(windows): more i18n cleanup: (#3415)
* feat(windows): generate message consts at build time (#3413)
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

* fix(windows): proxy configuration from system (#3389)
* fix(windows): hide unavailable options in Setup (#3392)
* fix(windows): make sure silent is actually silent for setup (#3391)
* chore(windows): staging site hostnames (#3387)
* feat(windows): cleanup server names (#3325)
* feat(windows): keyman: protocol handler (#3382)
* feat(developer/compilers): logging errors and warnings in the lexical model compiler (#3385)

## 14.0.114 alpha 2020-07-22

* chore(web): bump lodash from 4.17.15 to 4.17.19 in /web/testing/regression-tests (#3360)

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

* feat(windows): disable defaults options when Keyman already installed (#3371)
* feat(windows): setup will retry if offline during initial install steps (#3370)
* feat(windows): setup select tier from filename or parameter (#3369)
* fix(windows): re-add license to setup (#3368)
* feat(windows): strip ' (1)' from filename in setup (#3367)
* fix(windows): remove large dependencies (#3346)
* refactor(ios/engine): package-version query caching + package state properties (#3335)
* refactor(ios/engine): keyboard downloads now retrieve KMPs (#3341)
* refactor(ios/engine): download queue & download state detection cleanup (#3342)
* chore(developer): cleanup test constants (#3350)
* change(common/models/wordbreakers): update word boundary props to 13.0 (#3365)
* feat(linux): Use staging URLs for alpha version (#3364)

## 14.0.111 alpha 2020-07-16

* fix(android/engine): Update deprecated call to switch system keyboard for Android P (#3353)

## 14.0.110 alpha 2020-07-15

* fix(developer/compilers): merge duplicate words during compile (#3338)
* refactor(developer): hashmap-based wordlist compilation (#3340)
* chore(linux): Add license details for kmpdetails.* (#3355)

## 14.0.109 alpha 2020-07-14

* feat(windows): New Keyboard Search and Download (#3326)
* refactor(ios/engine): lexical model query & download rework (#3327)
* refactor(ios/engine): hashable package keys (#3333)
* fix(ios/engine): version equality (#3334)
* refactor(linux): make some methods protected (#3291)
* feat(oem/fv/android): Add nrc.str.sencoten model and update SystemKeyboard from KMAPro (#3332)

## 14.0.108 alpha 2020-07-09

* refactor(windows): PackagesOnly parameter (#3321)
* chore(windows): code cleanup (#3320)
* chore(windows): update setup i18n (#3317)
* chore(windows): Cleanup logging in Windows Setup (#3316)
* feat(windows): Keyman Setup online bootstrap (#3304)
* chore: cleanup global ExtPath (#3315)
* feat(android/app): Use build-download-resources.sh for KMApro app (#3322)
* feat(oem/fv/android): Use build-download-resources.sh for FV app (#3322)
* refactor(ios/engine): Centralized package download utility function (#3299)
* fix(oem/fv/android): Add a fallback keyboard to FV Android app (#3323)

## 14.0.107 alpha 2020-07-08

* feat(android/engine): Add additional info on installed keyboards exceptions (#3319)

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

* change(android/samples): Update addKeyboard syntax for Sample and Test apps (#3213)
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
* refactor(ios/engine): kmp.json - Lexical model info deserialization (#3195)
* feat(ios/engine): kmp.json - full deserialization (#3198)
* fix(android): Add wrapper for logging errors & exceptions (#3196)
* fix(oem/fv/ios): new certificate (#3201)

## 14.0.85 alpha 2020-05-30

* change(android/engine): Clean up naming for formatting QR code URL (#3187)

## 14.0.84 alpha 2020-05-29

* change(developer): packs ModelCompiler via npm pack, 7-zip (#3166)
* change(web): Selective Sentry uploads (#3163)
* refactor(ios/engine): Polishes HTTPDownloader to prep for unit testing (#3179)
* feat(ios/engine): HTTPDownloader unit testing (#3180)
* fix(linux): Disable buttons if no keyboard installed (#3184)
* change(ios/engine): Removes unused, deprecated JS/JSON-based adhoc install code (#3186)
* change(common/models,developer/compilers): move joiner from LMLayer to model compiler (#3071)

## 14.0.83 alpha 2020-05-28

* change(ios): Selective Sentry uploads (#3162)
* change(android/engine): Handle keyboard package updates (#3175)
* fix(common/models/types): fixes models-types build script, sets std header (#3182)

## 14.0.82 alpha 2020-05-27

* fix(ios): fixes build warning on Error coersion, fixes logged error for intermediate embedded state (#3173)
* fix(linux): Don't fail (un-)install if running multiple ibus (#3169)
* chore(linux): Update maintainer in package metadata (#3168)
* feat(linux): Improve appstream metadata (#3176)

## 14.0.81 alpha 2020-05-27

* refactor(resources): convert gosh into npm package  (#3159)
* chore(common,web): use consistent TypeScript dep on all packages (#3158)
* chore(common/resources): add `common/models` to build trigger definitions (#3144)
* fix(common/resources): adds package-lock.json for gosh package (#3171)

## 14.0.80 alpha 2020-05-25

* fix(android/engine): Remove LanguageListUtil (#3155)
* refactor(common/models/templates): create package: @keymanapp/models-templates (#3128)

## 14.0.79 alpha 2020-05-22

* fix(android): Fix system keyboard globe button override (#3140)
* fix(common/core/web): use build script to generate environment.inc.ts (#3146)
* change(common/core/web,web): update dependencies to @keymanapp/models-types (#3147)

## 14.0.78 alpha 2020-05-21

* refactor(common/models): factor out word breakers to their own package (#3125)
* fix(developer): crash when developer starts (#3138)
* chore(android): Refactor KeyboardPickerActivity (#3113)
* chore(common/resources): cleanup scopes (#3139)
* feat(web): starts proper KMW Sentry integration (#3122)
* refactor(common/models/types): rename @keymanapp/lexical-model-types => @keymanapp/models-types (#3143)
* change(common/core/web): add and distribute type declaration (#3145)
* fix(common/models/types): drops version updates for deprecated common/lexical-model-types (#3148)

## 14.0.77 alpha 2020-05-19

* refactor(web/engine): Moves common utility functions into separate `web-utils` package (#3130)
* refactor(web/engine): renames DeviceSpec, moves to utils (#3132)
* fix(ios): Fixes keyboard metadata decoding, tweaks to project files (#3137)
* feat(windows): Use http: instead of file: for Configuration UI (#3127)

## 14.0.76 alpha 2020-05-15

* fix(windows): use correct name for Sentry in C++ (#3129)
* chore: Allow to override hook defs (#3112)

## 14.0.75 alpha 2020-05-13

* feat(android/samples): Add Tamil lexical model to KMSample2 (#3123)

## 14.0.74 alpha 2020-05-11

* fix(windows): kmbrowserhost was missing debug info (#3117)

## 14.0.73 alpha 2020-05-11

* fix(windows): force output keystroke failure (#3083)
* fix(windows): kmshell title and kmbrowserhost sentry (#3115)

## 14.0.72 alpha 2020-05-08

* fix(oem/fv/ios): Fixes FV app's system keyboard (#3105)
* fix(oem/fv/ios): FV light mode lock and basic banner fix (#3108)
* fix(windows): sentry cef shutdown interactions (#3107)
* feat(android): Migrate installed keyboards list to keyboards_list.json (#3091)

## 14.0.71 alpha 2020-05-08

* fix(windows): use consistent sentry db location (#3100)

## 14.0.70 alpha 2020-05-07

* fix(android/app): Fix back button after System Keyboard dismissed (#3093)
* fix(android/samples): Back button to dismiss KMSample2 system keyboard (#3095)
* fix(oem/fv/android): Back button to dismiss FV Android system keyboard (#3096)

## 14.0.69 alpha 2020-05-06

* fix(common/resources): npm install required for auto inc lerna versions (#3089)
* fix(windows): sentry x64 stacks truncated pointers (#3087)

## 14.0.68 alpha 2020-05-06

* fix(common/core/desktop): enable build for win x64, use global VERSION.md and fix decxstr() bug (#3076)
* change(android): Add methods to go between LanguageResource and JSON (#3079)
* feat(common): auto-update for package versioning (#3078)

## 14.0.67 alpha 2020-05-05

* fix(oem/fv/ios): Additional libraries for FirstVoices SWKeyboard (#3080)

## 14.0.66 alpha 2020-05-04

* feat(web): test Recorder overhaul, Node-based tests using Recorder for KeyboardProcessor (#3060)
* fix(oem/fv/ios): add sentry framework to carthage build step (#3074)

## 14.0.65 alpha 2020-05-03

* fix(oem/fv/ios): try embed of sentry again for fv (#3072)

## 14.0.64 alpha 2020-05-01

* fix(oem/fv/ios): add sentry framework to fv keyboards (#3069)

## 14.0.63 alpha 2020-05-01

* fix(android): Fix FileUtilsTest to be cross-platform (#3061)
* fix(windows): add LARGEADDRESSAWARE flag for all CEF processes (#3064)
* fix(android): Remove more custom keyboard fields(#3051)
* fix(oem/fv/ios): FV settings bundle for SWKeyboard (#3066)
* change(common/models/wordbreakers): single-pass join word breaker decorator (#3059)

## 14.0.62 alpha 2020-05-01

* change(oem/fv/android): Update DSN for FV Android app (#3050)
* feat(web/engine): Basic KeyboardProcessor tests (#2994)
* fix(oem/fv/android): Update FirstVoices build_keyboards.sh script (#3045)
* fix(oem/fv/ios): Update FirstVoices build_keyboards.sh script (#3045)

## 14.0.61 alpha 2020-04-30

* chore(common/resources): bump @actions/http-client from 1.0.3 to 1.0.8 in /resources/build/version (#3047)
* fix(oem/fv/ios): firstvoices icon and version info (#3044)
* feat(windows): Sentry integration fixes and polish (#3006)
* feat(developer/compilers): compile joinWordsAt property (#3032)

## 14.0.60 alpha 2020-04-29

* feat(common/resources): initial use of lerna (in-repo package links only) (#2997)
* change(web/engine): spins core/web/keyboard-processor package off from KeymanWeb (#3001)
* change(web/engine): spins core/web/input-processor package off from KeymanWeb (#3008)
* chore(common/models): change author from personal to work affiliation (#3046)

## 14.0.59 alpha 2020-04-29

* chore: merge stable history (#3037)
* feat(common/models/wordbreakers): create join word breaker decorator (#3021)
* fix(oem/fv/ios): download fv keyboards (#3040)
* change(android): Refactor LanguageResource() and remove "Custom" property (#3033)

## 14.0.58 alpha 2020-04-28

* change(developer/compilers): improvements to default searchTermToKey (#3024)

## 14.0.57 alpha 2020-04-27

* change(common/resources): Update to Unicode 13.0 (#3029)
* change(android): Refactor Keyboard class to not use Map (#3020)

## 14.0.56 alpha 2020-04-24

* fix(android): Clarify label that shows "Get Started" on startup (#3025)
* feat(developer/compilers): allow for verbose word breaker specification (#3023)

## 14.0.55 alpha 2020-04-23

* change(android): Convert LanguageListActivity to utility (#3018)
* change(android): Change "Get Started" keyboard picker to bring up embedded keyboard search (#3013)
* feat(common): lerna now npm-installed locally (#3012)
* refactor(web/engine): successful web-core compilation (#2992)
* change(common/models,developer/compilers): always bundle searchTermToKey() with model (#2971)
* change(common/models): remove NFD table (#3014)

## 14.0.54 alpha 2020-04-22

* refactor(developer/compilers): word breaker compilation (#3016)

## 14.0.53 alpha 2020-04-21

* refactor(common/models): Abstracted connection between LMLayer and Worker initialization (#2986)
* refactor(common/models): starts a formal 'headless' mode (#2987)
* fix(android): predictive banner display bugfix (#3010)
* fix(android): Fix system keyboard alignment (#3009)

## 14.0.52 alpha 2020-04-17

* refactor(web/engine): precomputation for OSK key events, headless production thereof (#2969)
* refactor(web/engine): initial ModelManager split (#2974)
* refactor(web/engine): LMLayer enablement state management rework (#2975)
* refactor(web/engine): predictive data routing, LanguageProcessor as EventEmitter (#2976)
* refactor(web/engine): web-core build prep (#2982)

## 14.0.51 alpha 2020-04-16

* change(android): Update minimum SDK to 21 (#2993)

## 14.0.50 alpha 2020-04-15

* feat(android): Add KMPBrowserActivity for cloud keyboard searches (#2961)
* fix(android): Handle default font DejaVuSans.ttf (#2981)
* feat(android): Download cloud keyboards from https://keyman.com/keyboards (#2953)
* fix(ios): iOS 13.4 subkey menu workaround (#2959)
* change(ios): Web-based popup key longpresses (#2968)
* fix(web): repairs Web regression test suite (#2973)
* feat(android): Dismiss system keyboard on Back press (#2984)

## 14.0.49 alpha 2020-04-11

* chore(common/models): do not run tests in IE11 in Windows (#2978)

## 14.0.48 alpha 2020-04-07

* feat(windows): Testing Sentry integration (#2923)
* feat(windows): use crashpad and better call stacks (#2931)

## 14.0.47 alpha 2020-04-07

* fix(common/models): use searchTermToKey() on input (#2954)
* refactor(web/engine): proper split-off of DOM-reliant code (#2939)
* refactor(web/engine): InputProcessor/KeyboardProcessor split (#2940)
* fix(ios): prevents in-app keyboard resets (#2951)
* refactor(web/engine): headless KeyboardProcessor (#2941)

## 14.0.46 alpha 2020-04-06

* fix(android): Add check for WRITE_EXTERNAL_STORAGE permission (#2946)

## 14.0.45 alpha 2020-04-05

* chore(web): bump minimist from 1.2.2 to 1.2.3 in /web/testing/regression-tests (#2947)

## 14.0.44 alpha 2020-04-03

* refactor(web/engine): relocates DOM-only parts of Processor (#2922)
* refactor(web/engine): begins formally removing DOM-aware keyboard API functions from web-core KeyboardInterface (#2915)
* refactor(web/engine): start of system store abstraction (#2919)
* refactor(web/engine): Processor now manages current layer; OSK listens via callback (#2920)
* refactor(web/engine): RuleBehavior now headless (#2925)
* refactor(web/engine): variable store storage abstraction (#2926)
* fix(web): fixes activeElement typing (#2927)
* refactor(web/engine): relocates DOM-only parts of Processor (#2938)
* fix(android): Change KeyboardHarness/build.sh to not rebuild KMEA (#2943)
* feat(android): Propagate languageID when downloading kmp (#2944)

## 14.0.43 alpha 2020-04-02

* fix(web/engine): default layout fix for chiral keyboards (#2936)
* fix(android): Fix exception in ResourcesUpdateTool (#2933)
* change(android/samples): Update sample and test projects to install asset kmp's (#2935)

## 14.0.42 alpha 2020-04-01

* fix(android): Fix globe button crash on 3rd party apps (#2930)
* change(android) Install default asset kmp's (#2928)

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
* chore(common/resources): Cleanup unused folders and update README (#2916)

## 14.0.39 alpha 2020-03-29

* feat(windows): sentry x64 support (#2898)
* feat(windows): user control for upload to sentry (#2900)

## 14.0.38 alpha 2020-03-28

* feat(ios): Add Crowdin CLI for iOS strings (#2905)

## 14.0.37 alpha 2020-03-27

* feat(windows): crash reports in CEF (#2887)
* feat(common/resources): Use Crowdin CLI (v3) for handling l10n files (#2895)
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

* fix(android/samples): Update min SDK versions for sample apps (#2872)
* refactor(web/engine): Modularize default key output handling (#2853)
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

* fix(android/samples): Fix min SDK version for Sample and Test apps (#2860)

## 14.0.30 alpha 2020-03-19

* docs(common/resources): minor updates to readme (#2856)

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

* chore(web): bump minimist from 1.2.0 to 1.2.2 in /web/testing/regression-tests (#2829)

## 14.0.26 alpha 2020-03-16

* fix(oem): Disable monitoring of ANR for oem Android app (#2828)

## 14.0.25 alpha 2020-03-13

* feat(common/resources): sentry release control (#2794)
* chore(windows): improve build script tests (#2680)
* fix(web): enhanced sourcemaps + proper sourcemaps for minified KMW (#2809)
* feat(common/resources): Add script to parse crowdin translation file (#2801)
* fix(linux): Fix how keyboardprocessor version is set in dist.sh (#2814)
* feat(android): Additional Sentry integration (#2810)
* fix(web): applies base key layer property to unassigned subkeys (#2808)
* feat(android): Start adding RTL to layouts (#2816)
* fix(common/models): fixes word lookup from Tries for SMP script-based languages (#2815)
* feat(common/resources): add release finalization for Sentry (#2819)
* fix(web): further fixes BuildVisualKeyboard. Fixes #2818 (#2822)
* fix(web): fixes internal reference for validation tool (#2824)

## 14.0.24 alpha 2020-03-11

* fix(linux): Fix CI dist path to common/core/desktop (#2795)
* fix(web): fixes build number reference of API call (#2796)
* fix(web): updating BuildVisualKeyboard (#2802)

## 14.0.23 alpha 2020-03-10

* fix(ios): fixes Carthage framework copy for Sentry (#2800)
* feat(android): Start of Sentry-based crash reporting (#2778)

## 14.0.22 alpha 2020-03-09

* fix(common/resources): parameter order incorrect in git diff (#2787)
* feat(ios): compilations within Xcode now properly set version (#2775)
* fix(ios/engine): Fixes context bug for certain keyboard rules after newlines (#2770)
* feat(android): Update additional main app strings for crowdin (#2793)
* feat(ios): Start of Sentry-based crash reporting (#2771)
* feat(ios): Improved Sentry integration (first pass) (#2782)

## 14.0.21 alpha 2020-03-08

* fix(common/resources): builds were never triggered (#2790)

## 14.0.20 alpha 2020-03-08

* feat(windows): Chromium replacement for embedded MSHTML in for Keyman Desktop (#1720)
* refactor(common/core/desktop): Rename keyboard core (#2735)

## 14.0.19 alpha 2020-03-06

* fix(web): support otf extension for truetype fonts (#2780)

## 14.0.18 alpha 2020-03-04

* feat(windows): etl2log support tool (#2758)
* feat(developer): allow use of ISO9995 in key ids (#2741)
* feat(android): Handle keyman:// protocol to download kmp keyboard (#2734)
* change(android): Cleanup UI strings (#2751)
* fix(ios): fixes broken online-help versioned link (#2773)

## 14.0.17 alpha 2020-02-26

* feat(ios): KeymanEngine version migration unit tests (#2692)
* feat(ios): starts iOS unit testing (#2649)
* fix(android) Fix crash on kbdMapList (#2719)
* fix(developer): crash when switching layout templates (#2726)
* feat(developer): always save options (#2731)
* feat(common/resources): Support git worktree when configuring local hooks (#2722)
* docs(linux): Add linux packaging documentation (#2720)
* fix(developer): insert from charmap into touch editor (#2737)
* fix(developer): debugger breaking smp with bksp (#2739)
* feat(ios): Adds keyboard-scale unit tests, fixes unknown-device bug (#2695)

## 14.0.16 alpha 2020-02-25

* fix(android) Fix crash on kbdMapList (#2719)
* fix(developer): crash when switching layout templates (#2726)

## 14.0.15 alpha 2020-02-24

* fix(android): Sanitize app version string for api query (#2715)
* chore(common/resources): Improve output when triggering Jenkins jobs (#2706)
* fix(common/resources): Fix increment-version.sh script (#2714)

## 14.0.14 alpha 2020-02-21

* chore(linux): allow to trigger Jenkins build from script (#2697)
* fix(ios): Resource update issues (#2703)
* fix(web): Web CI target reversion (#2693)
* refactor(common/resources): Simplify and improve getting hook directory (#2701)

## 14.0.13 alpha 2020-02-19

* fix(linux): setup gschema (#2675)
* fix(ios): Responsive predictive toggles (#2674)
* chore(windows): cleanup documentation (#2681)
* chore: merge p9s2 beta part 2 to master (#2683)

## 14.0.12 alpha 2020-02-18
* fix(android/samples): Fix build for KMSample1 project (#2669)
* fix(developer): crash in context help (#2661)
* fix(developer): crash importing blank OSK (#2663)
* fix(common/core/desktop, linux): Misc keyboard processor fixes for Xenial (#2648)
* fix(linux): Fix tier used in debian watch files (#2664)
* fix(ios): Engine demo fix (#2662)
* docs(ios): Updates offline help for new 13.0 content (#2671)
* fix(developer): upgrade removes preferences (#2672)
* fix(developer): upgrade removes preferences (#2668)
* chore(common/resources): Rename trigger-definitions.sh to *.config (#2665)

## 14.0.11 alpha 2020-02-17

* fix(web): Fat-finger complication fixes (#2647)
* feat(web): restyles prediction banner (#2629)
* chore(oem/fv/ios): FV iconography update (#2650)
* feat(mac): install script rather than drag+drop (#2537)
* fix(common): Update configure-repo.sh to ln commit-msg-defs (#2652)
* change(web): Testing resources update (#2651)
* fix(android): Add ability to reinitialize `CloudDownloadMgr` (#2635)
* chore: merge beta changes to master (#2659)
* fix(mac): invalid build script params removed (#2660)

## 14.0.10 alpha 2020-02-14

* fix(common/core): buffer overrun in context api (#2616)
* fix(web): patches Float and Toggle UI issues (#2622)
* change(web): build.sh recompilation of LMLayer only performed when needed (#2623)
* fix(ios): Fixes in-app use of KMSample2 (#2626)
* fix(developer): touch layout editor character map integration (#2619)
* fix(developer/compilers): package: version mismatch (#2620)
* fix(developer/compilers): compiler sometimes merges touch platform support wrongly in .keyboard_info (#2621)
* fix(ios): Fixes lexical model url generation for updates (#2627)
* fix(linux): Cherrypick linux packaging fixes  (#2624)
* fix(developer/ide): model debugger mime type mismatch (#2633)
* fix(ios): Fixes invalid keyboard display when backing out of sharing text (#2631)
* fix(oem/fv/android): Update version of androidx.appcompat (#2640)
* change(android): Update in-app help (#2641)
* fix(developer/ide): Model editor various bugs (#2634)
* fix(web): fixes positioning of native-mode language menu (#2642)
* change(android): Update app dependencies (#2644)
* fix(ios): initial-install check within Migrations (#2646)
* fix(mac): Modifier keys were resetting cached context (#2588)
* refactor(mac): initial steps of input pathway (#2643)

## 14.0.9 alpha 2020-02-10

* fix(common/core/desktop): buffer overrun in context api (#2614)
* fix(common/resources): refactor trigger of test builds (#2611)
* fix(ios): fixes installation of default resources and updates them (#2578)
* chore(android): Update default nrc.en.mtnt.model to 0.1.4 (#2608)
* chore(common/resources): add build scripts for beta tests (#2612)
* fix(common/resources): don't include build-utils.sh (#2615)
* fix(common/resources): Fix setting context when >= 64 characters (#2607)
* fix(ios): Icon image change on light/dark mode transition (#2593)
* fix(ios): Keyboard banner light/dark transitions (#2594)
* fix(linux): Fix packaging of keyman-config on Xenial (#2609)

## 14.0.8 alpha 2020-02-07

* chore(common/resources): Tweak history management (#2602)
* chore(common/resources): Update README.md (#2598)
* chore: update history (#2605)
* chore: merge beta P9S1 changes to master (#2606)
* chore(common/resources): Add script that checks if build is required (#2603)
* fix(linux): Install requirements before packaging (#2599)

## 14.0.7 alpha 2020-02-07

* chore(common/resources): Tweak history management (#2602)
* chore(common/resources): Update README.md (#2598)

## 14.0.6 alpha 2020-02-06

* fix(windows): Windows touch keyboard would cancel on each keystroke (#2580)
* fix(linux): Fix 'About' keyboard when missing copyright (#2583)
* feat(linux): Keyboard options configuration dialog (#2566)
* feat(common): prepare-commit-msg hook for autogenerated conventional commit messages (#2581)
* fix(linux): cherry-pick packaging changes from beta branch (#2589)
* fix(linux): Display the script name in log output (#2591)

## 14.0.5 alpha 2020-02-04

* chore(common/resources): support test builds on master/beta/stable-x.y (#2576)
* fix(web): default BKSP output targeting (#2561)
* fix(linux): Remove cosmic and disco releases (#2574)

## 14.0.4 alpha 2020-02-03

* chore(common/resources): trigger builds after version increment (#2572)
* fix(ios): keyboard stability (#2545)
* fix(web): dev_resource compile fix (#2557)
* fix(linux): Update launchpad PPA by tier (#2551)
* fix(android): Fix cancelling ConfirmDialogFragment (#2547)
* fix(web): Fix K_TAB from external keyboard (#2546)
* fix(ios): Now (eventually) uses sys pref for kbd clicks (#2550)
* feat(ios): Deeplink into Keyman settings for system keyboard setup (#2548)
* fix(common/models): RTL model specification (#2554)
* fix(web): Suggestion reordering for RTL languages (#2553)
* fix(ios): Fixes margins on keyboard in iOS 9/10 (#2560)

## 14.0.3 alpha 2020-02-03

* chore(common/resources): increment version final (#2568)
* chore(common/resources): add version tags (#2570)

## 14.0.2 alpha 2020-01-29

* chore(common/resources): version tags (#2562)

## 14.0.1 alpha 2020-01-29

* chore(common/resources): Starting 14.0 release
