---
title: KMManager.advanceToPreviousInputMethod()
---

## Summary
The **advanceToPreviousInputMethod()** method switches to the previous system keyboard input mode.

## Syntax

```javascript
KMManager.advanceToPreviousInputMethod()
```

## Description
Use this method to switch to the previous system keyboard.

When only 1 Keyman keyboard is installed, this is the default action for pressing the globe key.

## Examples

### Example: Using `advanceToPreviousInputMethod()`

The following script illustrates the use of `advanceToPreviousInputMethod()`:
```java
    // Remove the second keyboard in the list
    KMManager.removeKeyboard(this, 1);

    KMManager.advanceToPreviousInputMethod();
```

## See also
* [advanceToNextInputMode()](advanceToNextInputMode)
