---
title: Manager.stateForLexicalModel()
---

## Summary

The **`stateForLexicalModel()`** method returns the current state of
availability for a dictionary.

**Deprecated**, as keyboards and lexical models are no longer directly
downloaded in favor of packages. Refer to [`ResourceFileManager.shared.installState()`](../ResourceFileManager/installState)

## Syntax

``` swift
Manager.stateForLexicalModel(withID: String)
```

### Parameter

`withID`
:   ID of the dictionary.

### Returns

Returns the dictionary state: `downloading`, `needsDownload`,
`needsUpdate`, or `upToDate`.

## Description

This syntax can be used for getting the state of a dictionary.

## Examples

### Example: Using `stateForLexicalModel()`

The following script illustrates the use of `stateForLexicalModel()`:

``` swift
let dictState = Manager.shared.stateForLexicalModel(withID: dict.id)
if dictState == .needsUpdate {
  if !dictIDs.contains(dict.id) {
    dictIDs.insert(dict.id)
    updateQueue!.append(dict)
  }
}
```

## History

Deprecated in Keyman Engine for iPhone and iPad 14.0.
