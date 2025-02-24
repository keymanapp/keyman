---
title: beepReset (KBR)
---

## Summary

Cancels previous feedback [`beep`](/developer/language/reference/beep) operations across the page.

## Syntax

```c
keyman.interface.beepReset();
```

or

```c
KeymanWeb.KBR(); // Shorthand
```

### Parameters

None.

### Return Value

`undefined`

## Description

The `keyman.interface.beepReset()` function is automatically called by [`keyman.interface.beep()`](beep) after a short interval (approximately 1/20 of a second) and should not need to be called manually.
