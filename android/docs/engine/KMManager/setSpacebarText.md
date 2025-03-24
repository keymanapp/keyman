---
title: KMManager.setSpacebarText()
---

## Summary

The `setSpacebarText()` method sets the text display pattern for the
spacebar.

## Syntax

```java
KMManager.setSpacebarText(KMManager.SpacebarText mode)
```

### Parameters

`mode`
: The display pattern to use for the spacebar, one of:

  * `LANGUAGE` - the language name for the keyboard
  * `KEYBOARD` - the keyboard name
  * `LANGUAGE_KEYBOARD` - both the language name and the keyboard name,
    separated by hyphen
  * `BLANK` - no text to be displayed

### Returns

No return value.

## Description

The default text display pattern is `LANGUAGE_KEYBOARD`. The text shown on the
keyboard may be overridden on a per-keyboard basis with the `displayName`
parameter of the `setKeyboard()` function.

---

## Example: Using `setSpacebarText()`

The following script illustrates the use of `setSpacebarText()`:

```java
// don't show anything on the spacebar
KMManager.setSpacebarText(KMManager.SpacebarText.BLANK);
```

## See also

* [`getSpacebarText()`](getSpacebarText)
