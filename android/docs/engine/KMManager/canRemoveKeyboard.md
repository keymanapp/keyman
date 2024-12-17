---
title: KMManager.canRemoveKeyboard()
---

## Summary
The **canRemoveKeyboard()** method returns whether removing a keyboard is enabled, like in the keyboard picker menu.

## Syntax

```javascript
KMManager.canRemoveKeyboard()
```
## Returns

Returns `true` if removing a keyboard is enabled, `false` otherwise.

## Description
Use this method to check if a keyboard can be removed. If enabled, you can long-press on a row in the keyboard picker menu to remove a keyboard.

## Examples

### Example: Using `canRemoveKeyboard()`

The following script illustrates the use of `canRemoveKeyboard()`:
```java
    if (KeyboardController.getInstance().get().size() > 1 && KMManager.canRemoveKeyboard()) {
      // Remove the second keyboard
      KMManager.removeKeyboard(this, 1);
    }
```

## See also
* [canAddNewKeyboard()](canAddNewKeyboard)
