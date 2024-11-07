---
title: KMManager.isKeyboardLoaded()
---

## Summary
The **isKeyboardLoaded()** method returns whether the specified in-app or system keyboard is loaded.

## Syntax
```java
KMManager.isKeyboardLoaded(KeyboardType type)
```

### Parameters
type

: `KeyboardType.KEYBOARD_TYPE_INAPP` or `KeyboardType.KEYBOARD_TYPE_SYSTEM`

If type is `KeyboardType.KEYBOARD_TYPE_UNDEFINED`, the function will return `false`.

### Returns
Returns `true` if the specified keyboard is loaded, `false` otherwise.

## Description
Use this method to check if a keyboard is loaded and ready to use.

---

## Examples

### Example: Using isKeyboardLoaded
The following script illustrate the use of `isKeyboardLoaded`

```java
if (KMManager.isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM) {
   // Get the keyboard info at index 0
   Keyboard keyboardInfo = KMManager.getKeyboardInfo(this, 0)
}
```

## History
Added syntax in Keyman Engine for Android 14.0.

## See also
* [setKeyboard](setKeyboard)
