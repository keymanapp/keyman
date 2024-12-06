---
title: getSavedKeyboard
---

## Summary

Get the (internal) keyboard name and language code of the most recently active keyboard, as stored in KeymanWeb's cookie management system.

## Syntax

```c
keyman.getSavedKeyboard();
```

### Parameters

None.

### Return Value

`string`
:   Format: `Internal-Name:Language-Code`

## Description

This function is used to automatically restore the last keyboard selected by a user in order to restore it upon a new visit to the page KeymanWeb is embedded within.
