---
title: registerStub (KRS)
---

## Summary

Registers the keyboard stub or returns true if already registered.

## Syntax

```c
keyman.interface.registerStub(Pstub);
```

or

```c
KeymanWeb.KRS(Pstub); // Shorthand
```

### Parameters

`Pstub`
:   Type: `Object`
:   A keyboard stub object representing the keyboard's basic lookup parameters.

### Return Value

`optional number`
:   `1` if the keyboard is preregistered, otherwise `null`.

## Description

---
**Note:** This only registers the stub with KeymanWeb, allowing it to later request the keyboard from the server or filesystem upon demand in order to complete the linking process.

---

The `keyman.interface.registerStub` function is typically called on the user's behalf by the [`keyman.addKeyboards()`](../core/addKeyboards) function.
