---
title: KMManager.setCanRemoveKeyboard()
---

## Summary
The **setCanRemoveKeyboard()** method sets whether removing a keyboard is allowed, like in the keyboard picker menu.

## Syntax

```java
    void KMManager.setCanRemoveKeyboard(boolean newValue)
```

## Parameters

`newValue`
: `true` if removing a keyboard is allowed, `false` otherwise.

## Description
Use this method to grant the end user permission to remove keyboards from the keyboard list. If set to `false`, the installed keyboard list is permanent.

## Examples

### Example: Using `setCanRemoveKeyboard()`

The following script illustrates the use of `setCanRemoveKeyboard()`:
```java
    // Disable removing keyboards
    KMManager.setCanRemoveKeyboard(false);
```

## See also
* [`canRemoveKeyboard()`](canRemoveKeyboard)
