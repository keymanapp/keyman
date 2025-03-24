---
title: Manager.stateForKeyboard()
---

## Summary

The **`stateForKeyboard()`** method returns the current state of availability for a keyboard.

**Deprecated**, as keyboards and lexical models are no longer directly downloaded in favor of packages. Refer to [`ResourceFileManager.shared.installState()`](../ResourceFileManager/installState)

## Syntax

``` swift
Manager.stateForKeyboard(keyboardID: String)
```

### Parameter

`keyboardID`
:   ID of the keyboard.

### Returns

Returns the keyboard state: `downloading`, `needsDownload`, `needsUpdate`, or `upToDate`.

## Description

This syntax can be used for getting the state of a keyboard.

## Examples

### Example: Using `stateForKeyboard()`

The following script illustrates the use of `stateForKeyboard()`:

``` swift
let kbState = Manager.shared.stateForKeyboard(withID: kb.id)
if kbState == .needsUpdate {
  if !kbIDs.contains(kb.id) {
    kbIDs.insert(kb.id)
    updateQueue!.append(kb)
  }
}
```

## History

Deprecated in Keyman Engine for iPhone and iPad 14.0.
