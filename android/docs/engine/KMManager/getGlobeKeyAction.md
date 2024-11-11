---
title: KMManager.getGlobeKeyAction()
---

## Summary
The **getGlobeKeyAction()** method returns the short-press action type of the 'Globe' key.

## Syntax
```java
KMManager.getGlobeKeyAction(KeyboardType kbType)
```

### Parameters
kbType
: The keyboard type. `KEYBOARD_TYPE_INAPP` or `KEYBOARD_TYPE_SYSTEM`

### Returns
Returns the action type of the short-press 'Globe' key as one of
  `GLOBE_KEY_ACTION_SHOW_MENU`, `GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD`,
  `GLOBE_KEY_ACTION_ADVANCE_TO_PREVIOUS_SYSTEM_KEYBOARD`, `GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD`,
  `GLOBE_KEY_ACTION_SHOW_SYSTEM_KEYBOARDS`, or `GLOBE_KEY_ACTION_DO_NOTHING`.

## Description
Use this method to get the short-press action type of the 'Globe' key.

## Examples

### Example: Using getGlobeKeyAction()
The following script illustrate the use of `getGlobeKeyAction()`:
```java
GlobeKeyAction action = KMManager.getGlobeKeyAction(KeyboardType.KEYBOARD_TYPE_SYSTEM);
```

## See also
* [setGlobeKeyAction()](setGlobeKeyAction)
