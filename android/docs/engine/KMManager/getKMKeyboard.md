---
title: KMManager.getKMKeyboard()
---

## Summary
The `getKMKeyboard()` method returns the [KMKeyboard](../KMKeyboard) type depending whether it's an in-app or system keyboard.

## Syntax
```java
KMKeyboard KMManager.getKMKeyboard(KeyboardType type)
```
### Parameters 
type
: `KeyboardType.KEYBOARD_TYPE_INAPP` or `KeyboardType.KEYBOARD_TYPE_SYSTEM`

### Returns
Returns the KMKeyboard object. 

## Description
The `getKMKeyboard()` method returns the KMKeyboard for the specified keyboard type.

## Examples

### Example: Using getKMKeyboard()
The following script illustrates the use of `getKMKeyboard()`: 
```java
    KMKeyboard keyboard = KMManager.getKMKeyboard(KeyboardType.KEYBOARD_TYPE_SYSTEM);
```
