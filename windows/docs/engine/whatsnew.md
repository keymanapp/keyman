---
title: What's New in Keyman Engine 19.0 for Windows
---

* **kmcomapi:** Added the `MCompileForBaseKeyboard(long KLID)` COM method to the
  new `IKeymanKeyboardInstalled2` interface, which extends
  `IKeymanKeyboardInstalled`. The method accepts the user's base keyboard ID and
  allows `mcompile` to compile all installed keyboard layouts against that
  keyboard ID. See #16162 for details.
* **kmcomapi:** Added the `Install3` COM method, which accepts a base keyboard
  ID. The base keyboard ID is passed to `mcompile` during installation so that
  the keyboard layout can be compiled against the specified base keyboard.
  `Install3` has been added to the following interfaces: `IKeymanKeyboardFile3`,
  `IKeymanPackageFile3`, `IKeymanKeyboardsInstalled3`, and
  `IKeymanPackagesInstalled3`. See #16528 for details.

## See Also

* [Keyman Engine for Windows Documentation](index)