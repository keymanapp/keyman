---
title: KMManager.updateSelectionRange()
---

## Summary

The `updateSelectionRange()` method updates the selection range of the current context.

## Syntax


```java
KMManager.updateSelectionRange(KeyboardType kbType)
```

### Parameters

`kbType`
: Keyboard type requesting the selection range update. `KEYBOARD_TYPE_INAPP` or `KEYBOARD_TYPE_SYSTEM`.

### Returns

Returns `true` if the selection range was updated successfully, `false` otherwise.

## Description

Use this method to update the selection range of the current context. It must be called in response to InputMethodService's onUpdateSelection method and whenever InputMethodService's onStartInput method has been called. Normally you do not need to call this method for in-app keyboards.

----

## Syntax (Deprecated)

```javascript
KMManager.updateSelectionRange(KeyboardType kbType, int selStart, int selEnd)
```

### Parameters

`kbType`
: Keyboard type requesting the selection range update. `KEYBOARD_TYPE_INAPP` or `KEYBOARD_TYPE_SYSTEM`.

`selStart`
: The new selection start location.

`selEnd`
: The new selection end location.

### Returns

(Deprecated) Returns `true` if the selection range was updated successfully, `false` otherwise.

## Description

Use this method to update the selection range of the current context. It must be called in response to InputMethodService's onUpdateSelection method and whenever InputMethodService's onStartInput method has been called. Normally you do not need to call this method for in-app keyboards.

Since this syntax is deprecated, use `KMManager.updateSelectionRange(KeyboardType kbType)` syntax instead.

## Example: Using `updateSelectionRange()`

The following script illustrates the use of `updateSelectionRange()`:

```javascript
    @Override
    public void onUpdateSelection(int oldSelStart, int oldSelEnd, int newSelStart, int newSelEnd, int candidatesStart, int candidatesEnd) {
        super.onUpdateSelection(oldSelStart, oldSelEnd, newSelStart, newSelEnd, candidatesStart, candidatesEnd);
        KMManager.updateSelectionRange(KMManager.KeyboardType.KEYBOARD_TYPE_SYSTEM);
    }
```

## History

Deprecated `updateSelectionRange(KeyboardType kbType, int selStart, int selEnd)` syntax in Keyman Engine for Android 17.0


## See also

* [`updateText()`](updateText)
