---
title: KMManager.setGlobeKeyAction()
---

## Summary
The **setGlobeKeyAction()** method sets the short-press action type for the 'Globe' key.

## Syntax
```java
KMManager.setGlobeKeyAction(KeyboardType kbType, int action)
```

### Parameters
type

: `KeyboardType.KEYBOARD_TYPE_INAPP` or `KeyboardType.KEYBOARD_TYPE_SYSTEM`

action

: The action type. `GLOBE_KEY_ACTION_SHOW_MENU`, `GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD`,
    `GLOBE_KEY_ACTION_ADVANCE_TO_PREVIOUS_SYSTEM_KEYBOARD`, `GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD`, 
    `GLOBE_KEY_ACTION_SHOW_SYSTEM_KEYBOARDS`, `GLOBE_KEY_ACTION_DO_NOTHING`.

## Description
Use this method to set the short-press action type for the 'Globe' key. Even when the default action type is 
set, Keyman Engine will still use the following default action types when only one Keyman keyboard is installed:

| KeyboardType         | # Keyman Keyboards Installed | Globe Key Action Type                                |
|----------------------|:----------------------------:|------------------------------------------------------|
| KEYBOARD_TYPE_INAPP  | 1                            | GLOBE_KEY_ACTION_SHOW_MENU                           |
| KEYBOARD_TYPE_SYSTEM | 1                            | GLOBE_KEY_ACTION_ADVANCE_TO_PREVIOUS_SYSTEM_KEYBOARD |

The action `GLOBE_KEY_ACTION_SHOW_MENU` displays the Keyman keyboard picker menu. 
Other enabled system keyboards are also listed at the end of the menu</p>

The action `GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD` switches to the next Keyman keyboard (if more than 1 are installed).
Otherwise, the Keyman keyboard picker menu is displayed.

The action `GLOBE_KEY_ACTION_ADVANCE_TO_PREVIOUS_SYSTEM_KEYBOARD` switches to the previous system keyboard.

The action `GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD` switches to the next system keyboard.

The action `GLOBE_KEY_ACTION_SHOW_SYSTEM_KEYBOARDS` brings up the Android input method picker and
can only be set for `KEYBOARD_TYPE_SYSTEM`.

## Examples

### Example: Using setGlobeKeyAction
The following script illustrate the use of `setGlobeKeyAction`

```java
// Tapping 'Globe' key will switch to the next keyboard
KMManager.setGlobeKeyAction(KeyboardType.KEYBOARD_TYPE_SYSTEM, 
    GlobeKeyAction.GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD);
```

## See also
* [getGlobeKeyAction](getGlobeKeyAction)
