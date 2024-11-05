---
title: registerKeyboard (KR)
---

## Summary

Registers and loads the keyboard.

## Syntax

```c
keyman.interface.registerKeyboard(Pk);
```

or

```c
KeymanWeb.KR(Pk); // Shorthand
```

### Parameters

`Pk`
:   Type: `Object`
:   The keyboard object to be loaded and registered.

### Return Value

`undefined`

## Description

---
**Note:** Any calls made to this function before KeymanWeb has initialized will be deferred until initialization occurs. Furthermore, an existing keyboard stub must exist for KeymanWeb to fully link with the keyboard.

---
This keyboard object is typically provided directly by the keyboard-loading process employed within KeymanWeb and its keyboard source files.
