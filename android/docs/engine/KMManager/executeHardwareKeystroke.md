---
title: KMManager.executeHardwareKeystroke()
---

## Summary
The **executeHardwareKeystroke()** method passes the keystroke generated from a physical keyboard to the KeymanWeb engine to handle.

## Syntax
```java
bool KMManager.executeHardwareKeystroke(int code, int shift, KeyboardType keyboard, int lstates, int eventModifiers)
```

### Parameters

`code`
: key identifier

`shift`
: shift state

|     Flag     |   Value  | Description                            |
|--------------|----------|----------------------------------------|
| Left Ctrl    | `0x01`   | Left Control Flag                      |
| Right Ctrl   | `0x02`   | Right Control Flag                     |
| Left Alt     | `0x04`   | Left Alt Flag                          |
| Right Alt    | `0x08`   | Right Alt Flag                         |
| Shift        | `0x10`   | Shift Flag                             |
| Ctrl         | `0x20`   | Control Flag                           |
| Alt          | `0x40`   | Alt Flag                               |

`keyboard`
: `KeyboardType.KEYBOARD_TYPE_INAPP` or `KeyboardType.KEYBOARD_TYPE_SYSTEM`

`lstates`
: lock state. If neither the on or off state is specified for a lock key, then it is treated as unknown state for keyboard rule matching.

| Value  | Description    |
|--------|----------------|
| `0x0100` | Caps lock      |
| `0x0200` | No caps lock   |
| `0x0400` | Num lock       |
| `0x0800` | No Num lock    |
| `0x1000` | Scroll         |
| `0x2000` | No scroll lock |

`eventModifiers`
: [KeyEvent Flags](https://developer.android.com/reference/android/view/KeyEvent#getMetaState\(\)) indicating which meta keys are currently pressed.

### Returns
Returns `true` if the keyboard was initialized and executed the keystroke, `false` otherwise.

## Description
Use this method to pass a hardware keystroke to KeymanWeb Engine to process. The KeymanWeb Engine also factors in modifier and meta keys.

## Examples

### Example: Using `executeHardwareKeystroke()`
The following script illustrate the use of `executeHardwareKeystroke()`:

```java
    // Send keystroke to KeymanWeb for processing: will return true to swallow the keystroke
    return KMManager.executeHardwareKeystroke(code, keymanModifiers, keyboardType, Lstates, androidModifiers);
```
