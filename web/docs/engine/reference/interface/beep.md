---
title: beep (KB)
---

## Summary

Flash body or element as substitute for an audible feedback [`beep`](/developer/language/reference/beep).

## Syntax

```c
keyman.interface.beep(Pelem);
```

or

```c
KeymanWeb.KB(Pelem); // Shorthand
```

### Parameters

`Pelem`
:   Type: `Element`
:   The page element to be temporarily set flashing.

### Return Value

`undefined`

## Description

This function is designed to implement feedback for erroneous input sequences, as with the keyboard language [`beep`](/developer/language/reference/beep) command. The flashing feedback should last approximately 1/20 of a second.
