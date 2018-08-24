# Keyman Desktop Version History

## 2018-06-28 10.0.1200 stable
* 10.0 stable release

## 2018-06-27 10.0.1113.0 beta
* No changes to Keyman Desktop

## 2018-06-26 10.0.1112.0 beta
* No changes to Keyman Desktop

## 2018-06-21 10.0.1111.0 beta
* No changes to Keyman Desktop

## 2018-06-20 10.0.1110.0 beta
* Removes obsolete "Getting Started" links (#1004)

## 2018-06-19 10.0.1109.0 beta
* Fixes api IKeymanControl::OpenConfiguration (#995)
* Fixes some side effects when installing a keyboard (#997)

## 2018-06-15 10.0.1108.0 beta
* No changes in Keyman Desktop

## 2018-06-13 10.0.1107.0 beta
* Add Unicode 11.0 characters

## 2018-06-12 10.0.1106.0 beta
* No changes in Keyman Desktop

## 2018-06-11 10.0.1105.0 beta
* Platform tests (in keyboards) are now consistent across all platforms (#969)

## 2018-06-09 10.0.1104.0 beta
* Keyboards that have script subtags will now install on Windows 7 (#964)

## 2018-06-07 10.0.1103.0 beta
* Keyman Configuration will now show keyboards that have no name with their filename (#955)
* Keyboard packages with BCP 47 language subtags unrecognised by Windows will now install on Windows 7 (#948)

## 2018-06-05 10.0.1102.0 beta
* Improved crash reporting stability (#940)

## 2018-06-03 10.0.1101.0 beta
* No changes in Keyman Desktop

## 2018-06-01 10.0.1100.0 beta
* Fixes email link in Keyman help

## 2018-05-31 10.0.1099.0 beta
* Fixes a problem where "Castilian" was shown instead of "Spanish" in language configuration (#918)
* Fixes a crash when the Keyboard Options dialog is closed (#916)

## 2018-05-30 10.0.1098.0 beta
* Prevents a crash if an invalid .kvk file is found in a package (#915)

## 2018-05-29 10.0.1097.0 beta
* Fixes incorrect icons in the Hotkeys tab in Keyman Configuration (#906)
* Ensure language tags are correct when installing a keyboard (#892)

## 2018-05-28 10.0.1096.0 beta
* No changes in Keyman Desktop

## 2018-05-28 10.0.1095.0 beta
* No changes in Keyman Desktop

## 2018-05-26 10.0.1094.0 beta
* No changes in Keyman Desktop

## 2018-05-25 10.0.1093.0 beta
* No changes in Keyman Desktop

## 2018-05-22 10.0.1092.0 beta
* No changes in Keyman Desktop

## 2018-05-17 10.0.1091.0 beta
* Fixes Ctrl+Alt simulation regression (#835)

## 2018-05-11 10.0.1090.0 beta
* No changes in Keyman Desktop

## 2018-05-11 10.0.1087.0 beta
* No changes in Keyman Desktop

## 2018-05-09 10.0.1086.0 beta
* No changes in Keyman Desktop

## 2018-05-08 10.0.1085.0 beta
* Clarified restart requirements and UI in Setup (#840)

## 2018-05-08 10.0.1084.0 beta
* Added implementation for IKeymanKeyboardFile.DefaultHotkey (#839)

## 2018-05-08 10.0.1083.0 beta
* Fixes a crash in Keyman Configuration (#838)

## 2018-05-08 10.0.1082.0 beta
* Adds script lookup to Add Keyboard Language dialog (#827)

## 2018-05-07 10.0.1081.0 beta
* Fixes crash in Keyman Desktop when keyboard name contains an ampersand (#831)

## 2018-05-07 10.0.1080.0 beta
* Fixes crash starting Keyman Desktop when using certain base keyboards (#832)

## 2018-05-07 10.0.1079.0 beta
* Fixes crash in updating active keyboard icon in On Screen Keyboard (#828)

## 2018-05-04 10.0.1078.0 beta
* No changes in Keyman Desktop

## 2018-05-04 10.0.1077.0 beta
* No changes in Keyman Desktop

## 2018-05-04 10.0.1076.0 beta
* No changes in Keyman Desktop

## 2018-05-04 10.0.1075.0 beta
* No changes in Keyman Desktop

## 2018-05-03 10.0.1074.0 beta
* No changes in Keyman Desktop

## 2018-05-03 10.0.1073.0 beta
* No changes in Keyman Desktop

## 2018-05-03 10.0.1072.0 beta
* No changes in Keyman Desktop

## 2018-05-03 10.0.1071.0 beta
* No changes in Keyman Desktop

## 2018-04-30 10.0.1070.0 beta
* No changes in Keyman Desktop

## 2018-04-30 10.0.1066.0 beta
* No changes in Keyman Desktop

## 2018-04-27 10.0.1061.0 beta
* No changes in Keyman Desktop

## 2018-04-25 10.0.1060.0 beta
* No changes in Keyman Desktop

## 2018-04-25 10.0.1059.0 beta
* No changes in Keyman Desktop

## 2018-04-25 10.0.1058.0 beta
* No changes in Keyman Desktop

## 2018-04-12 10.0.1057.0 beta
* Fix for backspace in legacy mode breaking SMP characters (#729)

## 2018-03-31 10.0.1056.0 beta
* No changes in Keyman Desktop

## 2018-03-22 10.0.1055.0 beta
* Offline help updates are synchronised with help.keyman.com in the correct version (#695)

## 2018-03-22 10.0.1054.0 beta
* Initial beta release of Keyman Desktop 10

## 10.0 alpha
* Keyman Desktop moved to open source (#121)
* Support for custom BCP 47 language codes: you can now associate a keyboard with any valid language code in Windows 8 and later
* Keyman API: extensively rewritten with improved consistency
* Additional user interface language - Turkish (translation done by Stevan Vanderwerf)
* Support for Unicode 10.0
* Fix for sticky shift key when using Alt+Left Shift to switch languages (#315, #144, #129)
* Show all keyboard icons on Visual Keyboard toolbar (#338)
* Keyman API: Added support for x64 (#513)
* Keyman Desktop now connects only to *api.keyman.com* and *keyman.com* for online tools (#653)
* Add Language dialog shows suggested languages first (#619)

## 9.0.522 stable
* Keyboard hotkey toggles are not working in 9.0 (I5086)
* Improve compatibility with Firefox 42 and Internet Explorer 11 (I4933)
* Fix hang when closing Keyman if Windows compatibility flag is incorrectly set (I5018)

## 9.0.521 stable
* Updated digital certificate for Windows 8, 8.1 and 10 (I4978)

## 9.0.519 stable
* Keyboard options do not work always work correctly because they are set twice while processing (I4978)
* Update requirements in Help for Windows 10 (I4984)

## 9.0.518 stable
* OEM products now have a cleaner menu display with indented keyboards and languages (I4920)

## 9.0.516 stable
* Added: Note on why "Select keyboard layout for all applications" is disabled on Win 8.1+ (I4871)

## 9.0.514 stable
* Fixed: Inconsistent display of panels through Desktop (I4851)

## 9.0.513 stable
* Fixed: Keyman Desktop title in OSK has wrong grey background (I4849)

## 9.0.512 stable
* Fixed: If no baselayout is specified by the user, default to en-US (kbdus.dll) (I4786)
* Fixed: Shift keys would sometimes 'stick' in Mozilla Firefox (I4793)
* Fixed: Log reported modifier state as well as Keyman current modifier state in debug logs (I4843)
* Fixed: FileMakerPro 14 causes crash in Keyman Engine (I4846)

## 9.0.510 stable
* Keyman needs to rebuild its language profiles if they are inadvertently deleted (I4773)

## 9.0.507 stable
* Keyman loses focus sometimes when switching keyboards using the menu (I4731)

## 9.0.506 stable
* Language profile change notification while Keyman menu is visible sometimes causes a crash (I4715, I4683, I4591, I4577, I4541, I4472, I4431)
* Improve reporting on registry errors (I4565, I4657)

## 9.0.503 stable
* MSKLC keyboards do not get correct name in Configuration Hotkeys tab (I4712)
* MSKLC keyboards are not shown in the Keyman menu (I4713)
* Keyboard and language hotkeys don't always work (I4714)

## 9.0.494 stable
* Fixed hotkeys not always working consistently (I4674)
* Fixed read of invalid registry setting on some computers (I4660)

## 9.0.493 stable
* Add more detailed keyboard diagnostics (I4659)
* Add Keep in Touch screen (I4658)

## 9.0.492 stable
* On Screen keyboard translates keys wrongly for European keyboards (I4650)
* Mnemonic layout recompiler maps AltGr+# rather than \ on UK layouts (I4651)

## 9.0.491 stable
* Add logging for registration of keyboards for hotkey matching (I4648)

## 9.0.490 stable
* Add 'Enter License Key' link to splash screen (I4645)
* Fix crash on startup on some computers with multiple Keyman products installed (I4624, I4519, I4602, I4640, I4633, I4636, I4637, I4638, I4639)

## 9.0.489 stable
* Backspace key was not working in Logos (I4642)

## 9.0.488 stable
* Keyman could crash silently on exit due to null hotkeys being addressed (I4623)

## 9.0.487 stable
* .kmx installs upgraded from earlier versions were placed in the wrong folder (I4623)

## 9.0.485 stable
* keymanimport.log was generated incorrectly as unicode strings in an ansi file (I4617)
* Keyman crashed when trying to recompile a missing mnemonic layout (I4615)

## 9.0.483 stable
* Support install of keyboard against fallback locales (I4607)
* Support single keyboard buttons on OSK toolbar for OEM products (I4606)

## 9.0.482 stable
* Keyman installer did not show EULA when bundled with a keyboard (I4598)
* Keyman Configuration enabled keyboards when OK clicked even if Keyman not running (I4382)

## 9.0.481 stable
* If a computer does not have US keyboard installed, then AltGr rules can go wrong (I4592)
* The keyboard usage page can appear outside the OSK in some situations (I4593)

## 9.0.480 stable
* Spacebar results in incorrect output for subsequent letters on some keyboards (I4585)
* If Ctrl+Alt simulates RAlt is on, then Ctrl+Alt rules don't work at all (I4551, I4583)
* Add option to treat base keyboard deadkey as plain keys (I4552)
* Switch for all languages not disabled on Win8 upgrades (I4576)
* Support if(&baselayout) with all of the ISO names (I4588)
* Keyman fails to install shortcuts for keyboard documentation correctly (I4590)

## 9.0.479 stable
* Solution for output of Enter and Tab keys for some keyboards (I4575)

## 9.0.478 stable
* Solution for output of Enter and Tab keys for some keyboards (I4575)

## 9.0.477 stable
* Test solution for output of Enter and Tab keys for some keyboards (I4562)

## 9.0.476 stable
* Hotkey switching resulted in stuck Ctrl,Alt,Shift keys in some apps (I4511)
* On Win 8, Keyman keyboards appear as "Unavailable Input Method" in Control Panel - mitigation only, not fixed (I4531)
* Fixed: when Alt is down, release of Ctrl, Shift is not detectable within Keyman in some languages (I4548)
* Mnemonic layout recompiler did not translate Lctrl Ralt for deadkeys correctly (I4549)
* Logical flaw in mnemonic layout recompiler meant that AltGr base keys were never processed (I4550)
* Upgrade to 476 or later requires recompile of all mnemonic layouts (I4553)
* Keyboards without an icon must specify a default icon when registering to prevent control panel crashing (I4555)
* Attached files were not shown when loading diag files (I4559)
* Binary data in diagnostics was not streamed correctly (I4560)

## 9.0.475 stable
* Language hotkeys associated with non-primary keyboards do not trigger language change (I4516)
* Switch for all apps is not disabled in Win 8 (I4515)

## 9.0.474 stable
* Crash when saving OSK to file, changing keyboard midstream [CrashID:keyman.exe_9.0.473.0_2C59B75E_EAccessViolation] (I4487)
* The character map is not falling back to system fonts well when Code2000 missing (I4488)
* Crash calling TSF [CrashID:kmshell.exe_9.0.473.0_2C45D42D_EOleSysError] (I4494)
* Damaged package causes crash when trying to uninstall [CrashID:kmshell.exe_9.0.473.0_2C6B80C4_EOleException] (I4495)

## 9.0.473 stable
* Verify that Internet Explorer 9.0 or later is present at install time (I4470)
* Fix crash showing keyboard menu when product details are missing (I4458)
* Fix crash when menu popup is dynamically resized by system (I4429)
* Setup bootstrapper now handles upgrade scenarios with a prompt (I4460)
* Upgrade dialog showed wrong version of Keyman Desktop (I4445)
* Keyman Desktop Update dialog showed broken Tavultesoft image (I4456)
* API fix: Keyman had a mismatch between KEYBOARDINFO and INTKEYBOARDINFO (I4462)
* API fix: Keyman_BuildKeyboardList was including keyboards installed but with no profiles (I4461)

## 9.0.472 stable
* Chinese keyboard was not working correctly (I4452)
* Language hotkeys were not working (I4451)

## 9.0.471 stable
* Browser emulation control for kmshell breaks downlevel versions of Keyman (I4436)
* Crash if Keyman Engine 7 or 8-based product installed when starting Keyman Desktop 9 (I4421)
* Show Send to Tavultesoft button in Diagnostics (I4439)

## 9.0.470 stable
* Download Keyboard dialog had broken link (I4419)

## 9.0.469 stable
* Download Keyboard dialog does not display correctly (I4414)
* OSK does not show underlying characters if base keyboard is not loaded (I4415)
    
## 9.0.467 stable
* Character Map needs to insert characters using SendInput (I4412)
* Manual Activate dialog is misformatted (I4408)
* Add HKCU FEATURE_BROWSER_EMULATION 9000 for kmshell.exe (I4400)
* Character map allows Ctrl+Click to insert character (I4411)

### What's New
* Free Edition – Keyman Desktop now has a Free Edition with no restrictions on use – use it in your office, your home, your school. Upgrade to Pro for powerful features, additional keyboards, and personalized technical support.
* Rewritten for Windows 7, 8 and 8.1. Now integrates deeply into Windows Text Services Framework and presents as a keyboard through all Windows language interfaces. This means that keyboard input support is more consistent and more efficient in all applications.
* Behind the scenes, Keyman is now fully Unicode internally.
* User interface extensively redesigned, cleaned up and simplified.
* Keyboards now support hi-res icons for clean presentation on large screens.
* Keyboards now have more version information and online help integration.
* Keyman now supports iPhone, iPad and Android – all your favourite keyboard layouts available on your phone and tablet devices

## 9.0.466 beta
* OK and Cancel buttons are no longer missing on Proxy dialog (I4387)

## 9.0.465 beta
* Added HKCU FEATURE_BROWSER_EMULATION 9000 for kmshell.exe (I4400)

## 9.0.464 beta
* Added clean user interface selection for associated language in Free Edition (I4395)
* Keyman Desktop Free Edition polish (I4393)
* When configuration run from Splash and license key entered, splash didn't refresh (I4396)
* Get Started got impatient and showed nag too quickly on start (I4397)
* Hotkeys didn't show on keyboard list (I4398)
* HTTP download now reports progress more cleanly (I4399)

## 9.0.463 beta
* Initial Free Edition changes (I4390)

## 9.0.462 beta
* Unticked keyboards in Keyman Configuration are not now shown in Windows Languages (I4376)
* Keyman keyboards are no longer visible in Windows Languages when Keyman is not running (I4381)

## 9.0.461 beta
* Rapid typing in legacy mode no longer breaks (regression from 9.0.460.0) (I4378)

## 9.0.460 beta
* Icon size in tool tray is now correct when using large fonts (I4314)
* Keyboard Upgrade from 6.0, 7.0, 8.0 now supports keyboards installed for Current User, fonts and Start Menu entries (I4324)
* When On Screen Keyboard opens, if Keyman is off then icon now shows correctly (I4360)
* On Screen Keyboard now always shows correct base layout when keyboard active (I4363)
* Installer now enforces Windows 7 or later (I4366)
* Deadkeys are now working with Microsoft Word in TSF-aware mode (I4370)
* WOW64 is now tested consistently in all locations (I4374)
* Add registry flag 'deep tsf integration' to allow us to disable enhanced integration with TSF-aware applications (I4375)
    
## 9.0.459 beta
* Deadkeys are now working correctly in all cases in Wordpad and other TSF-aware applications (except Word) (I4278)
* All .ico formats do not load correctly in icon conversion for keyboard layouts (I4317)
* Alt+LeftShift hotkey is now set on clean install (I4318)
* If Keyman is not running, selecting a Keyman layout in Windows will no longer have any effect (I4325)
* Keyboard and interface hotkeys are now working (I4326)
* Deadkeys are now working correctly with mnemonic layouts (I4353, I4327)
* AltGr keys are now working correctly in enhanced integration mode (I4351)
* If splash screen is minimized, it can now be restored (I4356)
* Splash screen buy links now go to correct version of Keyman (I4357)
* COM registration updated for new interfaces in Keyman 9 (I4358)
* OSK now shows correct base keyboard and refreshes when switching languages (I4359)
    
## 9.0.458 beta
* Getting Started window gave instructions that were not valid for KM9 (I3674)
* Script error dialog was appearing behind splash dialog (regression from I3710) (I3730)
* Balloon tip and About page had wrong product version (I4311)
* Keyboard icons are now converted to 32BPP RGBA on install for Windows 8 compatibility (I4316)

## 9.0.457 beta
* Fixed: Keys that have rules but are not matched due to context did not generate output (I4290)
* Fixed: Additional minor bug fixes (I4302)
    
## 9.0.456 beta
* Fixed: Crash in Keyman Configuration (I4296)
* Fixed: Upgrade of keyboards failed to register in local machine context (I4297)
* Fixed: Old TSF addin remained registered when upgrading (I4298)
* Fixed: Keyman-installed Windows languages needed to be removed when upgrading (I4299)

## 9.0.455 beta
* Added: Support for upgrading configuration and keyboards from 8.0 to 9.0 (I4292, I4293)

## 9.0.454 beta
* Fixed: Switch from Keyman to Keyman keyboard caused loop in global language switch (I4277)
* Fixed: Keyboard switching and legacy support edge case scenarios (I4285, I4286, I4287, I4288)
    
## 9.0.453 beta
* Fixed: Shift states were not being preserved correctly (I3605)
* Fixed: Opening User Interface Language menu causes crash [CrashID:kmshell.exe_9.0.447.0_script_TfrmMain_0] (I4199)
* Fixed: Deadkeys only work in first 61 characters of document (I4266)
* Fixed: If Keyboard usage refreshes during exit, Keyman crashes [CrashID:keyman.exe_9.0.452.0_2C5FB0CD_EAccessViolation] (I4268)
* Fixed: Switch language for all applications is not working (I4271)
* Fixed: TIP only outputs first 127 characters of a rule result (I4272)
* Fixed: kmtip does not work if already active before KM starts (I4274)
    
## 9.0.452 beta
* Fixed: Keyman installed keyboards do not seem to appear in Windows Language control panel in Win 8 (I4202)
* Fixed: Icons do not show background correctly in lang switch window and Win 8 languages controls (also I4316) (I4204)
* Fixed: Crash in Keyman Configuration [kmshell.exe_9.0.451.0_script_TfrmMain_0] (I4251)
* Fixed: kmtip install does not register Win 8 support features (I4252)
* Fixed: TSF deadkeys do not function correctly (I4262)
* Fixed: Test for text editor running fails (I4265)

## 9.0.451 beta
* Fixed: Keyman Configuration crashed on first run due to koKeymanUniscribeManager reference (I4250)

## 9.0.450 beta
* Fixed: If kmtip CKMTipTextService::Activate fails, cleanup (I3706)
* Minor: Refactor kmxfile utility functions (I3757)
* Added: Removed all legacy keyboard management Win32 API calls and use only TSF (I4220)
* Fixed: Crash when OSK closed/reopened without dismissing hint window [CrashID:keyman.exe_9.0.449.0_2C405C5D_EInvalidPointer] (I4242)

## 9.0.449 beta
* Opening font helper or keyboard usage from Keyman menu on Win 8 still shows HTML outside window (I4225)
* Excmagic.debug left scattered around program file directories after uninstall (I4218)

## 9.0.448 beta
* Shift + Arrows do not select text (only move caret) in Win 8 when Keyman keyboard is active (I4201)
* Keyman TIP should use ITfTextInputProcessorEx (I4216)
* Keyman leaks an Internet Explorer window handle (I4214)
* Help dialog appears below OSK and is inaccessible (I4209)
* Font helper and Keyboard usage appear outside frame in Win 8 (I4208)
* Shift states still not working with unprocessed keys in V9 (I4128)
* Activate/Purchase dialogs are incomplete and the Buy Modules button doesn't work (I4090)

## 9.0.447 beta
* Exit Keyman hint appears to be blank on Win8? (I4187)
* Pressing Enter in install keyboard dialog gives error about admin req (I4172)
* Help Contents link does not work from Keyman menu (I3993)
* Help window Help and Help on Keyboard links don't work (I3676)
* Base Keyboard dialog has wrong style of buttons (I4184)
* Use TTempFileManager for all temporary files (I4195)
* Lang switch window shows wrong selection with Alt+LeftShift when TIP is active (I4191)
* keyman.exe seems to be missing icon (I3769)
* Lang switch window shifts on first view (I4190)
* wm_kmmoreposting must be refactored for TIP work as it is not sequential (I4196)
* Avoid interactions with full-screen RDP (I4197)
* mcompile logs should be stored in diag folder (I4174)
* Uninstalling a keyboard leaves the mnemonic recompiled layouts behind (I4173)

## 9.0.446 beta
* Keyman Engine installer does not include mcompile.exe (I4171)

## 9.0.445 beta
* Mnemonic layouts should be recompiled to positional based on user-selected base keyboard (I4169)
* Console execute in utilexecute.pas needs a temp copy of buffer to avoid write access violations (I4170)
* Shift states still not working with unprocessed keys in V9 (I4128)
    
## 9.0.444 beta
* Keyman Desktop installer does not install x64 TIP (I4161)

## 9.0.442 beta
* Add keyboard version information to Keyman Configuration (Tweak) (I4136)

# Legacy Release Notes - What's New?

## 8.0.0 stable

Here are some of the great things we have added to Keyman Desktop 8.0.

* 64-bit Support — Run Keyman Desktop in 64-bit Windows operating systems and applications.
* Language Switcher — Access all your Keyman keyboards, Windows languages and Windows keyboards in one pop-up menu with a single hotkey (by default Alt+LeftShift).
* Universal Language Switching — Switch keyboards once to use the same Keyman keyboard across all applications.
* Keyboard Options — Keyboards for Keyman Desktop 8 can come with options which let you type as you prefer. Options allow you to do things like type French accents before or after the vowel, type Lao with or without spaces, or type Tigrigna with Ethipian or Eritrean punctiaton.
* Enhanced UI — The Keyman interface has been redesigned, from much simpler setup to cleaner, more polished menus and dialogs.
* Enhanced Keyman Menu — Access any tool in the Keyman Toolbox directly from the menu.
* Enhanced On Screen Keyboard — The OSK better mimics your hardware keyboard, matching both the language and layout.
* Enhanced Font Helper — Now see exactly which keyboard characters your fonts support.
* Enhanced Character Map — Support for Planes 1-15 of the Unicode Standard has been greatly improved, as has filtering and search. For example, search now with instant feedback by part or all of character code point or name.
* Support for Unicode 5.2 & 6.0 — Type in everything from Egyptian to Emoji.
* Improved Application Compatibility — The keystroke processor in Keyman Desktop has been made more compatible with major applications such as OpenOffice, Microsoft Office and popular web browsers.
* Single Installer — Both Keyman Desktop Light & Keyman Desktop Professional now install from the same file, which there's no longer any need to worry about downloading the wrong edition. 

## 7.0.0 stable

* The Keyman Character Map — Enables instant input of any Unicode character into the active application. Characters can be searched by name, number, block, font, and more.
* The Keyman Font Helper — Lists all currently-installed fonts which work with the active Keyman keyboard.
* The Keyman Usage View — Includes keyboard-specific help, especially useful for layouts with no On Screen Keyboard.
* The Keyman Text Editor with Getting Started Tutorial — Teaches new users how to start using Keyman Desktop.
* Tips & Hints — Assist new users throughout the program.
* Improved Language Linking — Say goodbye to the Language Bar, Keyman Desktop now handles language switching in addition to switching Keyman keyboards.
* Enhanced Keyman Toolbox — Resizable, supports non-Latin Windows keyboards, and improved font-linking.
* Localisable Keyman Menus — Translate the Keyman Desktop user interface into any language.
* Modern COM API — Control all aspects of Keyman Desktop from Windows Scripting, Visual Basic for Applications, or your application!
* Windows Installer (.msi) — Permits straightforward deployment of Keyman Desktop.

## 6.2.0 stable
* Text Services Framework support - Keyman 6.2 supports the new Text Services Framework that is part of Microsoft Office XP. This enables Keyman to integrate with Microsoft Word 2002 much better than previously. Click here for more information.
* Enhanced European keyboard support - Keyman 5.0 required you to use a US English keyboard (QWERTY layout) for full compatibility with Keyman keyboards. Version 6.2 now supports any layout you wish to use, and is not dependent on US English in any way.
* Visual keyboards - An on-screen and printable keyboard is now available for Keyman 6.2 keyboards.
* Add-ins - Keyman now supports add-ins for other applications. Included with Keyman 6.2 are add-ins for Word and RichEdit. The RichEdit add-in does not require RichEdit 3.0.
* Enhanced integration with Windows languages - It is now possible to switch Keyman keyboards on automatically when a language is selected as a non-Administrative user.

## 5.0.0 stable
* Unicode support - With the release of version 5.0, Keyman now includes full support for Unicode. Unicode is a character encoding standard that supports most of the world's more common scripts, and includes support for user-defined scripts. Keyman 5.0 keyboards now support input and output of any of the thousands of characters defined in Unicode, including characters outside the Basic Multilingual Plane which are encoded with surrogate pairs.
* Integration with Windows - Keyman 5.0 integrates more tightly with the multilingual features of Windows 9x, Me, NT, and 2000. Keyman 5.0 also features full shell integration, so that you can install a keyboard by simply double-clicking on its icon in Windows Explorer.
* Keyman Developer- From version 5.0 and up, the Keyman Developer (previously TIKE) is a separate application, which must be downloaded and registered independently. This simplifies the deployment of Keyman on the majority of systems, where keyboard development is not required. See the Keyman Developer website for more information.

