# KeymanWeb Version History

## 2018-05-09 10.0.91 beta
* Fixes basic support for mnemonic keyboards.  (#517)
  * At this time, all mnemonic keyboards will assume a US keyboard layout within KeymanWeb.

## 2018-05-08 10.0.90 beta
* Fixes support for Keyman-language 'beep' statements as part of keyboard stores. (#733)

## 2018-05-07 10.0.89 beta
* Fixes an issue with case sensitive virtual keys used by some Keyman keyboards. (#162)

## 2018-04-25 10.0.86 beta
* Fixes display of popup keys representing modifiers and other special characters. (#698)

## 2018-03-22 10.0.83 beta
* Initial beta for KeymanWeb v 10.0.

## 10.0 alpha
* Updated versioning scheme for uniformity across all Keyman products.
* Reworked the KeymanWeb attachment model significantly to allow greater user control. (#98)
  * The 'kmw-disabled' flag may now be used to automatically enable and disable KMW for attached controls.
  * Attachment is now separate from the enabled and disabled state of a control.
  * New API functions have been added:  `detachFromControl`, `enableControl`, `disableControl`, `setKeyboardForControl`.
* Removed old code directed at legacy browsers no longer supported.  (#212)
* Refactored code paths relating to KMW's embedding in the iOS and Android apps.  (#211)
* Added support for L/R Alt and Ctrl modifiers for keyboards. (#9) (#52)
* Added support for use of the Caps Lock state within keyboards if specified by a keyboard designer.
* Reworked the keyboard-interfacing API calls and base layout of KeymanWeb (#349)
* Fixed keyboard loading upon initialization, redundant cloud requests for keyboards.  (#103)
* Fixed next-layer processing (#116) (#358)
* Fixed auto-attaching mode bug. (#352)
* Set the default behavior of KMW to auto-attach for all devices, rather than only desktops. (#375)
* Fixed key codes for longpress keys and handle default keys beyond U_FFFF
* Enhanced the removeKeyboards API function (#5)
* Fixed an issue with the Toolbar UI under certain scenarios (#271)
* New feature:  physical keyboard input now supported on touch-enabled inputs. (#237) (#311)
  * Note that hardware-based keyboard input will always follow 'hardware' and 'desktop' platform rules to ensure consistency.
* Fixed behavior of longpress subkeys specifying (layer-default) modifier settings. (#161)
* Converted KeymanWeb (largely) to TypeScript and streamlined the code to reflect new minimum requirements.
* Fixed styling of "Special" keys on touch layouts
* Added automated testing for KeymanWeb builds. (#350)
* Fixed bugs in the handling of deadkeys. (#281)
* Change from ISO 639-3 language codes to BCP-47 language codes
* Now generates 'change' and 'input' events from keyboard and OSK input (#42) (#571)

## 2017-07-10 2.0.473 stable
* 2.0 stable release build.
* Removed a build warning. (#125)
* Fixed bug with longpress menus not working correctly on symbol/numeric layers (#115)

## 2017-07-04 2.0.465 beta
* Fixed issues with the Bookmarklet feature not loading correctly.  (#112)

## 2017-06-29 2.0.464 beta
* Fix for broken keys in hardware keyboards on Android devices (#110)

## 2017-06-27 2.0.463 beta
* Reworked keyboard load to provide clearer and faster feedback on errors, improve performance (#87)

## 2017-06-21 2.0.460 beta
* Automated build configuration with enhanced build scripts (#102)

## 2017-06-16 2.0.453 stable
* KeymanWeb now supports MutationObservers for dynamically added and removed input elements on the page
when the initialization option `attachType` is set to auto and will attach/detach to these automatically. (#29)
* The touch implementation now supports keyboards with layers of differing row counts (#53)
* KeymanWeb support on touchscreen laptops is now greatly improved when not in touch-oriented mode (#61)
* Updated the source for compatibility and removal of warnings when building under the latest versions of Google Closure (#72)
* Updated copyright messages (#96)
* General improvements to the KeymanWeb build paths (#95)
* Fix bugs where `setActiveKeyboard` would fail, and setting default keyboards fail (#94)

## 2015-06-26 2.0.394 stable
* Added Android-specific OSK styling for phones and tablets.

## 2015-06-18 2.0.393 beta
* Corrected text label for added popup copy of base key.

## 2015-06-18 2.0.392 beta
* Key highlighting behavior corrected.
* OSK key sizes corrected.

## 2015-06-18 2.0.391 beta
* Fixed text on key preview.

## 2015-06-11 2.0.390 stable
* Updated CSS for vertical centering of text on keys.

## 2015-06-02 2.0.389 stable
* Corrected key preview/popup key interaction on phones.

## 2015-05-28 2.0.388 beta
* Key preview further corrected for compatibility with embedded iOS apps.

## 2015-05-26 2.0.387 beta
* Key preview logic corrected for KMEI, KMEI-specific code moved out of kmwosk.js.

## 2015-05-22 2.0.386 stable
* Key preview now fully tested, excluded for space bar, special keys, and popup keys.

## 2015-05-15 2.0.385 beta
* Further changes to key preview and key highlighting.

## 2015-05-14 2.0.384 beta
* Changed key preview to key highlighting for space bar and special keys.

## 2015-05-12 2.0.383 beta
* Corrected moveOver, backspace key, preview management.

## 2015-05-12 2.0.382 beta
* Changed integration of key preview notification (for embedded KMW).
* Corrected key highlighting behavior.
* Fix nearestKey() error if returning null.

## 2015-05-07 2.0.381 stable
* Single keyboard event model replacing separate key events.
* Added changes to static layout.

## 2015-04-24 2.0.380 stable
* Fix inconsistency from adjustHeights issue.

## 2015-04-24 2.0.379 beta
* Trap errors in adjustHeights().

## 2015-04-24 2.0.378 beta
* Correct adjustHeights().

## 2015-02-16 2.0.377 stable
* Consolidation from separate development builds, no significant changes.
* Fixed application of embedded font to OSK. (#7)

## 2015-01-23 2.0.376 stable
* RTL set on entry, add support for SVG (for system keyboard fonts).
* Added delay between installing and activating keyboards. (#6)

## 2014-11-11 2.0.375 stable
* First open source build.
* OSK key text truncated on phones. (#1)

## 2014-10-28 2.0.369 beta
* Adjust OSK padding and margin in CSS.

## 2014-10-25 2.0.368 beta
* Correct remote paths to keyboards, resources and fonts to correctly reference cloud server.

## 2014-10-24 2.0.367 beta
* Prevent failure if bad arguments provided in request for meta-data sent to keyman cloud, warn user if unrecognized language given.

## 2014-10-24 2.0.366 beta
* Fix addKeyboards() and related code to return complete keyboard list if called with no arguments.
* Correct invalid return values in smpstring.js.
* Remove debug reference from kmwtoggle.js.
* kmwbase.js: accept file: as protocol for page or resource references.
* Expose font checking as *isFontAvailable(name)* to allow site developers to check that a custom font has in fact been installed by the user's browser. 
(This was added partly to make it possible to know if cross-source font loading was being prevented, 
as by Firefox unless an override option is set. Not currently used, as use during page loading is 
problematic.)

## 2014-10-20 2.0.365 stable
* Source, output and samples directories reorganized and new build.bat files added.

## 2014-10-17 2.0.364 stable
* Updated initialization (and other) code to handle file:// when loading page directly from source rather than form a web site.
* Tidy up OSK styles.

## 2014-10-17 2.0.363 stable
* Added conditionallyHideOsk() to hide OSK if anywhere except mapped input touched except when scrolling.
* Get more accurate (browser and device-dependent) device width for touch devices.
* Add pixel-based scaling for OSK, move styles from js to CSS where possible.
* Remove duplicated code for mouse event handling.
* Add class modifier (kmw-5rows) to apply different scaling for 5-row keyboards in CSS.
* Prevent failure if requested keyboard font unavailable, set default options (native.js).
* Moved some default code back to keymanweb.js from native.js. (Can be overridden in embedded.js)
* Moved option key and rotation event handling to native.js (not used by KMEI/KMEA).
* kmwbase.js - add stub functions to avoid failure if initialization delayed, add exception trapping to viewport scaling.

## 2014-09-05 2.0.362 stable
* Fixed addKeyboards() and related functions for loading keyboards fro the cloud or local files.
* Added addKeyboardsByLanguage().
* Added doKeyboardUnregistered() function to handle kmw.registered again if the list of registered keyboards changes to allow external UI to update language/keyboard menus.
* Modified installKeyboard to use the file name as a parameter rather than the stub object.
* Removed remaining domain validation code, revised path reference resolution during initialization.
* OSK - qualify styles by OS, correct style name for OSK frame.
* Add support for keyboard-specific style sheets.
* Desktop UIs - set default keyboard to English to allow mapping to remain active, update menus after loading or unloading keyboard references.
* Keyboard load wait indicator disabled when loading local keyboards.
* Added member lists of languages and stubs to allow deferred loading of metadata from cloud.
* kmwbase.js: update code for getting path and protocol from executing script.

## 2014-07-31 2.0.361 stable
* Added addKeyboards and related functions to load keyboards from the cloud server.
* Moved alignInputs to native.js.
* Removed explicit references to KeymanTouch from shared code.
* Moved keyboard load timer and device rotation handling to native.js, excluded from embedded.js.
* Moved OSK refresh, keyboard load fail warning to native.js.

## 2014-07-25 2.0.360 stable
* Moved keyboard path and root path resolution to getKeyboardPath() function (for KMEW/KMEI branching).
* Moved all KMEI/KMEA specific code to embedded.js, KMEW specific code to native.js.
* Removed duplicated keymanweb.addEventListener function.
* OSK - correct popup delay, moved showKeyTip() and popup-key highlighting to native.js. (Not used by KMEI/KMEA).
* OSK - moved KMEI/KMEI specific code to embedded.js.
* KMW and OSK - added mouse event handlers for desktop simulation.
* OSK - use getStyleSheetPath() to separate KMEW from KMEI/KMEA paths.
* OSK - move font check code to native.js.
* kmwbase.js - added fontCheckTimer(), updated help URL, added default event handlers to replace inline handlers where possible, changed myPath(srcFile) to require name of source file to be used to identify path.
* kmwbase.js - moved keyboard loading wait() function to kmwnative.js (not used by embedded apps).

## 2014-06-12 2.0.359 stable
* Moved all key tip and key preview (KMEA, KMEI) code to highlight on/off function.
* Added call to kmw.keyboardchange event from OSK language menu change.

## 2014-06-12 2.0.358 stable
* Added call to oskClearKeyPreview() to cancel display of key preview window (where appropriate) by KMEA, KMEI.

## 2014-05-08 2.0.357 stable
* Now calls oskCreateKeyPreview in KMEA, KMEI to support display of local key preview windows on those devices.
* Corrected language menu on touch devices to ensure that the keyboard selector buttons remain visible when multiple keyboards are available for one language.

## 2014-05-08 2.0.356 beta
* Correct keyboard load sequencing (KMEW-101).

## 2014-05-02 2.0.355 stable
* Added code to executePopupKey to fully support unmapped key fallback processing.

## 2014-05-01 2.0.354 beta
* Corrected code in executePopupKey to support layer mapping from native apps.
* Reduced popup delay time if user slides touch point up toward popup window.
* Reduced OSK spacebar caption line spacing and text size (CSS change and new class for spacebar caption parent).

## 2014-05-01 2.0.352 beta
* Reduced popup key delay when sending popup data to native keyman app (Keyman for iPhone, Keyman for Android). Does not affect Keyman for Web.

## 2014-05-01 2.0.351 beta
* Removed debug alert (affected IE only).

## 2014-04-24 2.0.350 stable
* Corrected handling of U_xxxx virtual key codes to support all values except control codes.
* Adjusted pop-key positioning.
* Add support for code conditioning by layer (KIFS statement).

## 2014-04-17 2.0.349 stable
* Added 'keytip' labels to phone layouts, removed simulated (pop-up key) labels. Increased delay before displaying pop-up keys to 0.5 sec.
* Hide non-functional keyboard layers and remove any shift-key references to such layers.

## 2014-04-03 2.0.348 stable
* keymanweb.BuildVisualKeyboard (exposed) function restored to allow an image of the keyboard for any device to be displayed (or printed) using a desktop browser.
* Updated file locations: if keyboard (or font) file URL start with /,./ or ../, load with respect to current page; if URL includes :, treat as absolute, otherwise prepend keyboards (or fonts) path to given file name.

## 2.0.347 beta
* Change all key ids to Keyman Desktop names, remove all duplicate names, and renumber special (non-outputting) keys from 50000.
* Add special handling to distinguish between K_xxxx (mapped physical keys), T_xxxx (named touch keys), U_xxxx (touch keys with default Unicode output).
* Extend and correct KIFS handling to constrain rules according to touch/physical keyboard, platform, form factor and browser/application.
* Add device.browser member, extend identification of IE version to include IE11.
* Fix incorrect language menu position for Firefox on Android.

## 2.0.346 beta
* Correct font path to allow use of page-relative fonts. (Remove all absolute paths from code.).

## 2.0.345 beta
* Prevent display of undefined OSK layers.
* Set default font family for each OSK key (not only for entire OSK).
* Expose function 'correctOSKTextSize()' to allow app to correct OSK text size.
* Change pop-up key element id from 'subkey-...' to 'popup-[layer]-...' e.g. change subkey-K_A to popup-default-K_A. (KMEW-93).
* Ensure that popup keys managed by device are correctly cancelled.
* Apply modifier state to popup keys. (KMEW-93).

## 2.0.344 beta
* Add charset=UTF-8 to script tags to ensure correct interpretation of keyboards loaded by non-UTF-8 page. (KMEW-89).
* Correct font scaling on phone devices. (KMEW-90, KBDS-5).
* Add support for platform-dependent font size specification. (KMEW-90, KBDS-5).

## 2014-03-28 2.0.343 stable
* Now supports WordPress plug-in on touch devices as well as desktop. (KMEW-79).
* Correct layout issues with Chrome, Firefox and Opera browsers on Android, but abandon support for native Android browsers, as behaviour varies too much between devices and Android versions, which do not always display embedded (complex) fonts. (KMEW-82, KMEW-83).
* Default embedded font format for Android is now TTF (as well as for other platforms).
* Fix duplicated element border and background display.
* Prevent *touchend* event propagation (causing unwanted behaviour).
* Correct focus when moving between mapped and unmapped elements.

## 2.0.342 beta
* Correct OSK key sizing and appearance (especially Firefox).
* Correct language selection from language menu (manage change of focus).
* Correct duplicated element transparency.

## 2.0.341 beta
* Hide OSK when moving to unmapped input.
* Correct browser-dependent alignment of duplicated input elements.
* Match duplicated INPUT element height and spacing with base element.
* Cache keyboard input context strings to optimize rule processing (MCD).
* Prevent OSK disappearing when moving to next element.
* Support *email* and *url* input types (as well as *text* and *search*).
* Resize inputs after a rotation (Android browsers).
* Handle device rotation for Firefox browser.
* Recognize and correctly manage non-iOS devices that identify as iPad or iPhone in user agent string. (Android does not handle *gesture* events.)

## 2.0.340 beta
* Handle viewport scaling to remove fixed scaling restriction, required for WordPress Plug-In.
* Correct duplicated touch-screen input element for box-sizing and parent element attributes.
* Now use script-dependent character strings to check loading of embedded fonts.
* Sort input elements by position (once) rather than each time Tab is pressed. (KMEW-24).
* Add support for direct write to element by Keyman Engine for iPhone and iPad, Keyman Engine for Android.
* Handle device rotation using *orientationchange* event instead of resize event, and rescale OSK keys after a rotation. (Corrects resizing issue on iOS7.).
* Handle Enter/Return from OSK for search and submit input types.
* Correct OSK height and support external call by Keyman Engines for iPhone, iPad, Android.
* Handle blur event for duplicated input fields, and remove *onfocus* handler, since required behaviour is performed by *touchstart* handler.
* Recognize *KeymanTouch* “Mobile” and “Tablet” application identifiers and select form factor accordingly.
* Disable base elements on touch devices instead of setting readonly, to prevent base elements from ever receiving focus.
* Correct OSK key highlighting.
* Correct display and hiding of pop-up keys (especially for KeymanTouch).
* Adjust size and position of OSK and language menu for viewport and device-dependent scaling.

## 2.0.339 beta
* Identify Android version from user agent string.
* Manage double-tap zoom on Android native browsers (not completely effective).
* Support direct character insertion and pop-up key cancellation for *KeymanTouch.*.

## 2.0.338 beta
* Use absolute key positions for OSK.
* Manage standard keyboard layers on touch devices.
* Allow external control of OSK height (*KeymanTouch*).

## 2.0.337 beta
* Correct ‘*wait’* window behaviour.
* Prevent crash if keyboard stylesheet unavailable.

## 2.0.336 beta
* Correct API calls for returning keyboard metadata.
* Add new icons to desktop OSK title bar.
* Truncate long language names in toolbar UI.

## 2.0.335 beta
* Correct initialization timing.
* Add call-back to allow UI to manage OSK display events.
* Correct display of desktop ‘toggle’ UI.

## 2.0.334 beta
* Fix multi-touch response when hiding language menu on touch devices.
* Add special key recognition based on names instead of key IDs.
* Use custom alert for displaying build on touch devices.

## 2.0.333 beta
* Add support to allow KeymanTouch to manage iPad as well as iPhone.
* Add support for new special numeric, symbol and currency OSK layers.
* Correct OSK row sizing.
* Add single-key pop-up as temporary solution for enlarging visible key on phone devices.
* Correct OSK cookie behaviour (for desktop browsers).
* Correct desktop UI initialization timing.

## 2.0.332 beta
* Recognize special numeric, symbol and currency OSK layers.

## 2.0.331 beta
* Rename attribute for embedded font files from ‘source’ to ‘files’.

## 2.0.330 beta
* Rename embedded font location option from ‘fontSource’ to ‘fonts’.
* Correct OSK initialization timing for subscription model.

## 2.0.329 beta
* Support downloadable embedded fonts using added style-sheets.
* Correct computed styles for non-IE browsers.
* Fix non-ASCII key-cap labels for non-English layouts.
* Manage scroll behaviour on language menu.
* Rename duplicated input element class as ‘keymanweb-input’ (was ‘KMW-input’).
* Align duplicated elements with base elements after a rotation or resize.
* Set font for base elements according to keyboard and possibly embedded font.
* Use root-relative paths for fonts and other resources.

## 2.0.327 beta
* Add API call ‘moveToElement’ to allow UI or page to set focus.
* Correct appearance and behaviour of toolbar UI.

## 2.0.325 beta
* Correct resource paths.
* Fix alert window closure.
* Recognize own name as ‘keymanios’ and fix resource path for KeymanTouch applications.
* Clarify keyboard load failure message.
* Set US English as default keyboard.
* Correct determination of resource path.
* Delay touch device and UI initialization until page and all code loaded.
* Define special key codes for special layer selection.
* Support repeat on backspace for touch devices.
* Correct popup key font and text size.
* Fix language menu background behaviour.
* Add language menu index strip and highlight language menu key before selecting language.
* Remove explicit styles from code, add to default style sheet.
* Correct OSK size according to device (screen height and width reported differently by different devices).
* Add keycap labels for phonetic keyboards on touch devices.
* Use special font lookup for modifier and other special function keys.
* Revise (and simplify) default touch layout.
* Correct focus management for desktop toolbar UI.

## 2.0.320 beta
* Now takes language properties from keyboard descriptor (stub), not from keyboard, correcting problem with Eurolatin and other keyboards used for many different languages. (KMEW-26).
* Remove (obsolete) shadow and rounding style functions.
* Add custom alert and wait messages.
* Correct size and position of duplicated touch-device input elements.
* Add exposed function for selecting active input element.
* Prevent handling non-mappable input types.
* Add timeout to keyboard load.
* Add special initialization for KeymanTouch.
* Correct handling of saved language cookie.
* Re-align inputs on rotation.
* Correct keyboard registration issues.
* Add or correct calls for external UI management of OSK.
* Use classes to highlight OSK keys when touched.
* Correct positioning of pop-up key arrays.
* Move explicit OSK styles to CSS classes.

## 2013-01-25 2.0.315 beta
* Add function for removal of external style sheet.
* Add function to automatically focus first mapped element on page.
* Corrected language menu behaviour.
* Moved language indicator to space bar.
* Redesign of language menu on touch screen devices.

## 2.0.310 beta
* Corrected keyboard mapping context error (KMEW-1).
* Fixed touch input element scrollbar positioning error (KMEW-5).
* Add function to manage loading external style sheets.
* (Many other developmental changes.)

## 2011-08-26 2.0.300 beta
* First version of KeymanWeb with touch screen support.
