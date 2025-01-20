---
title: KMManager.advanceToNextInputMode()
---

## Summary
The **advanceToNextInputMode()** method switches to the next system keyboard input mode.

## Syntax

```javascript
KMManager.advanceToNextInputMode()
```

## Description
Use this method to switch to the next system keyboard.


## Examples

### Example: Using `advanceToNextInputMode()`

The following script illustrates the use of `advanceToNextInputMode()`:
```java
    // Remove the second keyboard in the list
    KMManager.removeKeyboard(this, 1);

    KMManager.advanceToNextInputMode();
```

## See also
* [advanceToPreviousInputMethod()](advanceToPreviousInputMethod)
