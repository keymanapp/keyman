# Keyman Version History

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
* change(web): tightens call signature for banner selection (#4966)
* chore: disable findTouchAliasElement logging (#4981)
* chore(common): update stable history for 14.0.272 (#4975)
* chore: Add cherry-pick label for cherry-pick PRs (#4973)

## 15.0.35 alpha 2021-04-23

* chore(ios): prep for CI transition to Xcode 12, build script tweak (#4967)
* chore(linux): Fix triggering of Jenkins builds for stable branch 🍒 (#4969)
* fix(linux): Don't crash if kmp file vanishes (fixes #4907) (#4970)

## 15.0.34 alpha 2021-04-22

* No changes made

## 15.0.33 alpha 2021-04-22

* chore(linux): Remove experimental scripts (#4913)
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
* fix(linux): don't crash on legacy non-Unicode files 🍒 (#4906)
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

* fix(linux): Fix crash if `<kbd>.json` doesn't contain description 🍒 (#4851)

## 15.0.27 alpha 2021-04-01

* docs(android): Update installing-keyboards.md (#4837)
* chore(deps): bump y18n from 4.0.0 to 4.0.1 in /web/testing/regression-tests (#4817)
* fix(android): ensure keyboard is always set after pageLoaded (#4840)
* chore(deps): bump y18n from 4.0.0 to 4.0.1 in /resources/build/version (#4818)
* fix(developer): buffer size for range expansions (#4831)
* chore(common): Check in crowdin for Fulah 🍒  (#4846)
* docs(android): Update image for enabling-system-keyboards (#4844)
* chore: history from 14.0.271 (#4849)

## 15.0.26 alpha 2021-03-31

* fix(android/engine): Add KMString wrapper for formatting Strings (#4813)

## 15.0.25 alpha 2021-03-30

* fix(developer): requote font names (#4814)

## 15.0.24 alpha 2021-03-29

* fix(ios): ensures JS keyboard set after page load (#4808)
* fix(developer): open containing folder was not opening correct folder (#4776)
* fix(linux): Fix crash if query doesn't contain bcp47 tag 🍒 (#4800) (#4811)

## 15.0.23 alpha 2021-03-26

* fix(ios): fixes sys-kbd setup help link for iOS 9 and 10 (#4774)
* fix(android): Fix NullPointerException in package installation (#4790)

## 15.0.22 alpha 2021-03-25

* fix(ios/samples): samples should use package-oriented API (#4771)
* fix(android/engine): Sanitize embedded KMW Sentry error (#4782)

## 15.0.21 alpha 2021-03-23

* fix(windows): Change TLangSwitchRefreshWatcher ownership from thread to form (#4752)
* fix(common/models): prediction threshold when count is low, unintentional aliasing (#4754)
* chore: merge beta to master (A15S1) (#4745)
* chore: beta to alpha A15S1 (#4759)

## 15.0.20 alpha 2021-03-22

* docs(ios): adds supported l10ns to help/about/whatsnew (#4625)
* fix(windows): represerve keys on setfocus (#4622)
* fix(windows): show version selector for Keyman in installer (#4623)
* fix(windows): ensure valid base layout on install (#4627)
* chore(common): Update readme (#4631)
* chore(linux): Update readme (#4633)
* fix(common/core/web): Add keyboard check for scriptObject (#4640)
* docs(common): Update readme (#4642)
* fix(windows): context help links (#4626)
* fix(windows): hotkeys correctly ignore right modifier keys (#4628)
* fix(windows): Handle disabled profiles and invalid language ids (#4635)
* fix(web): event handling for TouchAliasElement's blinking caret (#4638)
* fix(common/models): prevents "undefined" reversion display string (#4648)
* docs(common): Update npm-packages readme (#4644)
* chore(developer): add B11 ISO code for ABNT2 keyboard (#4654)
* fix(web): better check for missing MutationObserver type (#4646)
* fix(android/engine): Reset in-app context when selection changes (#4636)
* chore(linux): Move signature files as well (#4653)
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
* fix(windows): Trigger language sync after changes (#4663)
* chore(linux): Ignore buildtools in keyman-config tarball (#4675)
* chore(windows): breadcrumbs (#4683)
* fix(ios): set CODE_SIGN before build (#4682)
* fix(mac): prevent duplicate keyboards in list (#4674)
* fix(ios): keyboard search did not alert for lack of net (#4693)
* chore(android/engine): Send KMW console errors to Sentry (#4692)
* fix(windows): start Keyman after setup only if not running (#4681)
* fix(developer): &CasedKeys and &MnemonicLayout are not compatible together (#4690)
* fix(android/engine): Fix crash when download file fails to copy to cache (#4697)
* fix(developer): validate keyboard_info should give info on non-canonical bcp47 (#4689)
* chore(developer): upgrade bcp 47 canonicalisation to warning (#4699)
* fix(developer): support named character codes at end of line (#4691)
* fix(android/app): Load system keyboard before checking overrides (#4696)
* fix(android): Update styling and set text to black (#4662)
* fix(common/core/web): disambiguation of keys sharing same base key ID (#4703)
* fix(windows): Download keyboard dialog TLS protocol support (#4714)
* fix(web): first-pass workaround for popup-key corrections (#4704)
* fix(developer): use var not let in definitions (#4718)
* chore(windows): finalize help (#4717)
* chore(android/app): Migrate launcher icon to adaptive (#4720)
* fix(common/core/web): predictive banner activation logic (#4713)
* fix(web): allows refresh of currently-loaded model (#4728)
* fix(ios): kbd search now caches publishing data for result (#4729)
* fix(linux): Fix switching to keyboard in middle of line (#4678) (#4721)
* fix(windows): ensure profiles are reinstalled during setup (#4727)
* fix(windows): Avoid double start during setup (#4726)
* chore: B14S8 beta to alpha (#4739)
* docs(ios): Help for 14.0 features (#4741)
* chore: increment-version.sh for stable (#4742)

## 15.0.19 alpha 2021-03-09

* fix(common/models): predictions after context reset / caret shift (#4411)
* change(oem/fv/ios): FV keyboards now package-based (#4471)
* fix(common/models): merges identical suggestions after casing (#4502)
* fix(web): macOS 11 agent string parsing (#4497)
* fix(ios): app logging messages were transient, never stored (#4500)
* chore(ios): web-side sentry enablement try-catch (#4492)
* fix(common/core/web): core key-processing now always returns RuleBehavior type. (#4508)
* fix(common/resources): Set help-keyman.com.sh executable (#4510)
* fix(developer/compilers): fixes error when "constructor" is in wordlist (#4504)
* fix(web): hides touch-alias caret when keystroke causes focus change (#4514)
* chore(common): allow forced version increment (#4522)
* fix(web): keyboard documentation patch-up (#4512)
* fix(web): removes package namespacing from kbd's CSS class (#4516)
* fix(windows): Handle Caps Lock event correctly from TIP (#4536)
* fix(developer): run even if sentry unavailable (#4537)
* fix(developer): UTF-8 messages in LM compiler (#4539)
* feat(common/models): mid-context suggestions & reversions, fix(common/models): correction-search SMP issues (#4427)
* fix(ios): package installer completion requires welcome dismissal (#4543)
* fix(windows): Refresh settings on 64-bit apps (#4378)
* fix(windows): prevent re-registration of TIPs on 14.0 upgrade (#4535)
* fix(web): mnemonic keystrokes w FF keymapping (#4540)
* chore(ios/app): Adjust help titles for installing custom dictionaries (#4550)
* fix(common/resources): Fix help.keyman.com path for CI (#4565)
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
* feat(ios): enables de, fr, and km localizations (#4585)
* fix(developer): improve CEF location search stability (#4571)
* fix(developer): Support all fonts in Keyboard Fonts dialog (#4574)
* feat(developer): Add different Open Containing Folder buttons (#4576)
* feat(developer): Range expansions (#4584)
* fix(web): fixes lack of respect for underlying-key display settings (#4572)
* fix(common/models): fixes application of suggestions immediately after a backspace (#4587)
* fix(linux): Improve version number (#4582)
* chore(linux): Don't report to Sentry in dev environment (#4581)
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
* fix(web): use language code correctly in toolbar (#4620)
* chore(linux): Fix some lintian warnings for Debian package (#4621)
* chore: beta to alpha merge, B14S7 (#4624)

## 15.0.18 alpha 2021-02-25

* fix(linux): Set help-keyman.com.sh executable (#4530)

## 15.0.17 alpha 2021-02-22

* chore(windows): FirstVoices Keyboards Configuration merge (#4458)
* chore(mac): Check in markdown help (#4479)
* fix(common/core/web): mock construction with caret at index 0 (#4474)
* change(oem/fv/ios): FV keyboards now package-based (#4471)
* fix(linux): Fix dependencies on packages (#4464)
* change(ios/app): Generate offline help from markdown (#4470)
* fix(windows): tsysinfox64 not signed (#4486)
* chore(android/app): Update help formatting and images (#4485)
* fix(android/engine): Display welcome.htm help within the app (#4477)
* fix(ios): tutorial's link to "Add a Keyboard" links directly to keyboard search (#4491)
* feat(linux): Improve output of km-package-list-installed (#4481)
* chore: merge B14S6 beta to alpha (#4503)

## 15.0.16 alpha 2021-02-11

* fix(android/app): Wrap preference screen titles (#4326)
* fix(ios): better handling of scoped vs non-scoped package URLs (#4327)
* fix(web): bulk renderer using video stream capture (#4316)
* change(android/engine): Allow swipe to dismiss update notifications (#4329)
* fix(windows): improve support for strings.xml (#4323)
* fix(windows): invalid character in text content opening help (#4330)
* fix(windows): align title to top in Install Keyboard (#4331)
* chore(windows/resources): Cherry-pick 14.0 help from #4109 (#4335)
* fix(ios): renew distribution certificate 🍒 (#4344)
* fix(ios): reloads keyboard after package updates (#4347)
* fix(ios): fixes accidental logo / cancel button overlap during package installation (#4332)
* fix(ios): resolves rough edges with installation view transitions (#4338)
* fix(web/ui): propagates UI module build failures (#4352)
* chore(linux): Fix typo (#4341)
* chore(windows/resources): Address more TODO links for help (#4351)
* fix(windows): setup strings comment for language (#4362)
* fix(windows): Splash button sizes (#4348)
* chore: manual version increment (#4363)
* chore(common): Enhance PR labeling based on PR title (#4357)
* change(web): set -eu for web scripts (#4353)
* fix(ios): accidental duplicated line from merge (#4366)
* chore(linux): Don't report on Sentry when running unit tests (#4356)
* fix(windows): Ignore Access Denied error creating task (#4365)
* chore(windows/resources): Fix more titles and TODOs in help (#4367)
* fix(windows): SMP-aware deletion in TSF-aware apps (#4360)
* fix(common): tweak surrogate pair deletions (#4361)
* fix(windows): Enter and Spacebar handling in Configuration (#4349)
* fix(web): removes stylesheets from unloaded keyboards (#4371)
* fix(android, ios): eliminates OSK layout flashing from predictive text banner display (#4370)
* fix(windows): crash for Sinhala mitigation (#4380)
* chore(linux): Pass second tag parameter to Jenkins build (#4388)
* fix(windows): remove Show Keyboard Usage hotkey (#4379)
* fix(windows): avoid invalid language codes in Add Language dialog (#4381)
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
* fix(developer): crash deleting wordlist (#4406)
* fix(developer): Use forward slashes in wordlist paths (#4407)
* modify(android/app): Change build-help.sh to generate offline help from Markdown files (#4397)
* fix(linux): Don't crash if uninstalling last keyboard (#4402)
* chore(windows/resources): Fix typo about Community Forum link (#4412)
* change(android/app): Separate displaying welcome.htm from keyboard installation (#4413)
* fix(linux): Improve fix for #3399 (#4418)
* fix(linux): Fix packaging (#4428)
* fix(android/engine): Remove WRITE_EXTERNAL_STORAGE from manifest (#4434)
* chore(common): Check in crowdin files for French (#4420)
* fix(developer): debug information with unicode identifiers (#4408)
* fix(developer): Compiler check for if and nul at start of context (#4410)
* feat(developer): improve BCP 47 canonicalization (#4425)
* chore(linux): Check in markdown help for Linux (#4414)
* fix(windows): When uninstalling, exit Keyman (#4383)
* fix(common/models): predictions after context reset / caret shift (#4411)
* modify(common): Refactor help-keyman-com.sh script for uploading help files (#4433)
* fix(linux): improve BCP 47 canonicalization (#4439)
* fix(common/resources): Just use master branch for help.keyman.com (#4459)
* fix(windows): hotkeys offset in config list (#4454)
* chore(ios): Settings case-statement cleanup (#4443)
* fix(linux): Also use staging site for beta versions (#4455)
* fix(android/engine): Display online keyboard help (#4462)
* fix(windows): Track modifier changes in UWP apps (#4468)
* fix(common/resources): Fix help.keyman.com path for commit (#4469)
* fix(common): create GitHub comments serially (#4472)
* chore(linux): Improve launchpad.sh script (#4355)
* chore: merge B14S5 beta to master (#4432)
* chore: B14S6 beta to master merge (#4473)
* chore: manual version increment (#4478)

## 15.0.14 alpha 2021-02-02

* docs(linux): Improve packaging doc (#4389)

## 15.0.13 alpha 2021-02-01

* chore(linux): Don't report on Sentry when running unit tests (#4374)
* chore(linux): Pass second tag parameter to Jenkins build (#4376)

## 15.0.12 alpha 2021-01-29

* chore(linux): Improve launchpad.sh script (#4355)

## 15.0.11 alpha 2021-01-27

* chore: Enhance PR labeling based on PR title (#4226)

## 15.0.10 alpha 2021-01-27

* fix(ios): renew distribution certificate (#4342)

## 15.0.9 alpha 2021-01-25

* fix(windows): beta uses wrong server (#4142)
* chore(windows): remove obsolete importkeyboard app (#4138)
* fix(windows): glitch in keyboard menu (#4139)
* fix(windows): menu scroll positions need reset at popup (#4140)
* fix(windows): remove obsolete option 'Switch to OSK/Help' (#4141)
* fix(windows): Canonicalize BCP 47 tag on keyboard download (#4144)
* fix(windows): help window centred on load (#4147)
* fix(windows): Text editor font bugs (#4149)
* chore(common): Update history for 14.0 beta release (#4148)
* fix(common): increment version needs to check base (#4155)
* fix(windows): uninstall language button z-index (#4145)
* fix(windows): exit button position on setup (#4150)
* fix(windows): Online Update dialog layout was messy (#4152)
* fix(windows): sentrytool should fail build on exception and access violation when rewriting executables (#4158)
* fix(windows): Warn if we reach maximum transient languages (#4157)
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
* fix(developer): make touch layout editor source view fonts consistent (#4198)
* fix(developer): Respect tab editor option (#4200)
* fix(web): Solving kmwosk color inconsistency 🍒 (#4187)
* chore(linux): Allow to push to the `keyman-beta` ppa (#4214)
* chore(linux): pass tag to Jenkins build (#4160) (#4224)
* chore: bugfix cherrypick (#4208, #4210) (#4230)
* fix(common/core/web): mnemonic modifier key-up handling (#4231)
* fix(android/engine): Remove usage of WRITE_EXTERNAL_STORAGE permission (#4170)
* fix(web): osk size & position after focus changes (#4232)
* chore(linux): Update debian metadata based on Debian repos (#4233)
* fix(windows): fix menu popup position (#4175)
* fix(windows): Update mitigation for Keyman 14 and Windows 10 19597 (#4180)
* fix(developer): Allow unhandled keys to go through to debugger memo (#4209)
* chore(common): Check in crowdin files for km (Khmer) (#4228)
* fix(web): OSK key preview positioning (#4241)
* fix(web): disables predictive text on Opera mini (#4243)
* fix(windows): typo in font helper string (#4251)
* chore(android/app): Add Obolo language from crowdin (#4256)
* fix(web): unexpected errors in OSK / banner position calculations (#4238)
* chore(linux): Improve version number for debian package (#4258)
* fix(android): Popup misalignments and compatibility with WeChat, Telegram (#4254)
* chore(web): sample pages now wait on init's promise for keyboard loading (#4253)
* fix(android/samples): Fix Sentry dependencies (#4267)
* fix(web): better font-wait null guards (#4286)
* fix(web): uses CSS line-height to vertically center oversized OSK keycaps (#4255)
* fix(developer): disable TSentryClient on WINE (#4274)
* fix(web): no key previews for special keys reliant on keyboard-specific OSK font (#4282)
* fix(web): default kbd ui name is now generic (#4293)
* fix(windows, common/core/desktop): context mismatch with if and dk (#4276)
* chore(common): Check in crowdin files for de (German) (#4295)
* feat(android/app): Add option to change display language (#4261)
* fix(developer/compilers): fixes developer build breakage from #4291 (#4299)
* fix(android/app): Handle keyman protocol from external browser (#4292)
* fix(android/engine): Fix zip slip vulnerability (#4300)
* fix(web): toolbar and loading optimisations (#4304)
* fix(web): OSK loading efficiency (#4279)
* chore(web): updates MTNT for pred-text testing page to 0.1.5 (casing) (#4305)
* fix(android/samples): Add dependency on androidx.preference (#4310)
* fix(web): default popup key selection, space highlight after popup (#4306)
* fix(web): language menu key highlighting (#4308)
* fix(web): dynamic font downscaling for OSK keys (#4270)
* chore(windows/resources): Cleanup Notes and Tips in help (#4307)
* fix(developer): Debug character grid performance (#4237)
* chore(android/samples): Remove old sample keyboard loaded code (#4315)
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

* fix(common): increment version needs to check base 🍒 (#4156)

## 15.0.1 alpha 2020-12-14

* chore: prepare 15.0 alpha (#4129)

## 14.0.273 stable 2021-04-26

* chore(linux): Fix triggering of Jenkins builds for stable branch (#4968)
* fix(linux): Don't crash if kmp file vanishes (fixes #4907) 🍒 (#4972)
* fix(linux): Fix crash with incomplete metadata (#4908) 🍒 (#4979)
* chore(android/engine): Don't use localized string for Sentry errors 🍒  (#4980)
* chore: disable findTouchAliasElement logging 🍒 (#4982)
* fix(web): Make banner initialization more robust 🍒  (#4983)
* chore: Add cherry-pick label for cherry-pick PRs 🍒 (#4984)

## 14.0.272 stable 2021-04-22

* chore(linux): Debug triggering Jenkins build (#4852)
* chore(android/engine): Rename "Fula" to "Pulaar" 🍒  (#4860)
* chore(android/engine): Rename "Pulaar" to "Pulaar-Fulfulde" 🍒  (#4891)
* fix(developer): Reduce non-canonical BCP 47 tag warning in PackageInfo to Info 🍒  (#4892)
* chore(common): Update crowdin for French 🍒  (#4903)
* chore(android,windows): Check in crowdin for Indonesian 🍒  (#4904)
* fix(linux): don't crash on legacy non-Unicode files (#4878)
* fix(android/engine): Don't load woff fonts on Android N 🍒  (#4924)
* fix(linux): Don't crash on network problem (fixes #4911) 🍒 (#4931)
* fix(android/engine): Change getList() to return an empty list instead of null 🍒  (#4928)
* fix(windows): handle invalid package names during install 🍒 (#4888)
* fix(windows): crash when installing TIP in some rare situations 🍒 (#4901)
* fix(windows): access violation closing text editor 🍒 (#4921)
* fix(windows): help contents broken from tray menu 🍒 (#4923)
* fix(developer): avoid crash if .kpj.user file is malformed 🍒 (#4919)
* fix(developer): chiral mismatch warning is disruptive 🍒 (#4935)
* chore(windows): disable profile repair 🍒 (#4900)
* fix(windows): avoid error if keyman32.dll renamed 🍒 (#4941)
* change(web): adds error + console logs usable for Sentry reporting targeting 🍒 (#4929)
* fix(web): fixes subkey lookup for fat-finger processing 🍒 (#4955)
* fix(oem/fv/android): Migrate keyboard list from 12.0 to 14.0 🍒  (#4952)
* fix(web): publish restorePosition() function 🍒 (#4957)
* fix(web, ios): better SMP, emoji handling with frequent keyboard swaps 🍒 (#4958)
* feat(linux): Improve Sentry crash reporting 🍒 (#4960)

## 14.0.271 stable 2021-04-01

* fix(android/engine): Sanitize embedded KMW Sentry error (#4786)
* fix(ios/samples): samples should use package-oriented API 🍒  (#4772)
* fix(ios): fixes sys-kbd setup help link for iOS 9 and 10 🍒 (#4775)
* fix(android): Fix NullPointerException in package installation (#4796)
* fix(ios): ensures JS keyboard set after page load 🍒  (#4809)
* fix(developer): open containing folder was not opening correct folder (#4777)
* fix(linux): Fix crash if query doesn't contain bcp47 tag (#4800) (#4801)
* fix(android/engine): Add KMString wrapper for formatting Strings (#4820)
* fix(developer): requote font names 🍒 (#4815)
* fix(android): ensure keyboard is always set after pageLoaded 🍒 (#4841)
* fix(linux): Fix crash if `<kbd>.json` doesn't contain description (#4835)
* fix(developer): buffer size for range expansions 🍒 (#4832)
* chore(common): Check in crowdin strings for Fulah (#4822)
* docs(android): Update installing-keyboards and enabling-system-keyboards (#4842)

## 14.0.270 stable 2021-03-23

* chore: stable tier (#4763)

## 14.0.269 beta 2021-03-23

* No changes made

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

* fix(mac): prevent duplicate keyboards in list (#4674)
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
* fix(ios): renew distribution certificate 🍒 (#4344)
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
* fix(web): Solving kmwosk color inconsistency 🍒 (#4187)

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
* fix(windows): Some registry keys could have incorrect permissions 🍒 (#3668)
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

* refactor(resources): convert gosh into npm package 🙃 (#3159)
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
* fix(developer): insert from charmap into touch editor 🍒 (#2737)
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
* fix(developer): upgrade removes preferences 🍒 (#2672)
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
* chore: merge beta changes to master 🍒 (#2659)
* fix(mac): invalid build script params removed (#2660)

## 14.0.10 alpha 2020-02-14

* fix(common/core): buffer overrun in context api 🍒 (#2616)
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
* chore(common/resources): add build scripts for beta tests 🍒 (#2612)
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
