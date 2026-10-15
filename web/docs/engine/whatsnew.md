---
title: What's New in KeymanWeb 19.0
---

* **BREAKING** `file:` protocol is no longer supported, as keyboards are loaded
  with `fetch()` API (#12334)
* Bug fix: the `keyboardloaded` event now fires only once per loaded keyboard.
  Previously the event fired twice. (#16297)
* Fixed `loaduserinterface` and `unloaduserinterface` events to fire
  consistently (#16353)
* Clarified use of asynchronous APIs (such as `addKeyboards()`) (#16064)
* Cleanup of various examples (#16064)
* Addressed several bugs with control of active keyboard (#16524)
* Removed `keyman.FocusLastActiveElement()` (use
  `keyman.focusLastActiveElement()`) (#16170)
* Removed `keyman.GetLastActiveElement()` (use `keyman.getLastActiveElement()`)
  (#16170)
* Removed `keyman.HideHelp()` (use `keyman.osk.hide()`) (#16170)
* Removed `keyman.ShowHelp()` (use `keyman.osk.show()`, `keyman.osk.setRect()`)
  (#16170)
* Removed `keyman.ShowPinnedHelp()` (use `keyman.osk.show()`,
  `keyman.osk.setRect()`) (#16170)
* Deprecated `keyman.build` and `keyman.version` (use `keyman.versionInfo`)
  (#16351)
* Deprecated `keyman.helpURL` (use Keyman Cloud APIs) (#16351)
* Deprecated `keyman.moveToElement` (use `elem.focus()`) (#16661)
* Deprecated `keyman.interface.FocusLastActiveElement()` (use
  `keyman.interface.focusLastActiveTextStore()`) (#16170)
* Deprecated `keyman.interface.GetLastActiveElement()` (use
  `keyman.interface.getLastActiveTextStore()`) (#16170)
* Deprecated `keyman.interface.HideHelp()` (use `keyman.osk.hide()`) (#16170)
* Deprecated `keyman.interface.ShowHelp()` (use `keyman.osk.show()`) (#16170)
* Deprecated `keyman.interface.ShowPinnedHelp()` (use `keyman.osk.show()`,
  `keyman.osk.setRect()`) (#16170)
* Deprecated `keyman.util.toFloat()` (#16351)
* Deprecated `keyman.util.toNumber()` (#16351)
* Deprecated `keyman.util.toNzString()` (#16351)
* Removed deprecated `onunloaded` handler (#16351)
* About [550 other fixes and changes](https://keyman.com/go/app/whatsnew/web/19.0)

## See also

* [Keyman Engine for Web Documentation](index)
