---
title: KMManager.getSpacebarText()
---

## Summary

The `getSpacebarText()` method returns the current text display pattern for the
spacebar.

## Syntax

```java
KMManager.SpacebarText KMManager.getSpacebarText()
```

### Returns

The current text display pattern for the spacebar, one of:

  * `LANGUAGE` - the language name for the keyboard
  * `KEYBOARD` - the keyboard name
  * `LANGUAGE_KEYBOARD` - both the language name and the keyboard name,
    separated by hyphen
  * `BLANK` - no text to be displayed

## Description

The default text display pattern is `LANGUAGE_KEYBOARD`. The text shown on the
keyboard may be overridden on a per-keyboard basis with the `displayName`
parameter of the `setKeyboard()` function.

---

## Example: Using `getSpacebarText()`

The following script illustrates the use of `getSpacebarText()`:

```java
// get the current spacebar text mode
KMManager.SpacebarText mode = KMManager.getSpacebarText();
```

## See also

* [`setSpacebarText()`](setSpacebarText)
