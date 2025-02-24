---
title: KMManager.getDefaultKeyboard()
---

## Summary
The `getDefaultKeyboard()` method returns the keyboard information for the fallback keyboard.

## Syntax
```java
KMManager.getDefaultKeyboard()
```

### Returns
Returns `Keyboard` type for the fallback keyboard. If not specified, this defaults to keyboard information for sil_euro_latin.

## Description
The `getDefaultKeyboard()` method returns the keyboard information for the fallback keyboard. If Keyman Engine
has issues with a current keyboard, KMManager will switch to this fallback keyboard.

## Examples

### Example: Using getDefaultKeyboard()
The following script illustrates the use of `getDefaultKeyboard()`: 
```java
    Keyboard kbd = KMManager.getDefaultKeyboard();
```

## See also
* [setDefaultKeyboard](setDefaultKeyboard)
