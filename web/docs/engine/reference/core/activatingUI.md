---
title: activatingUI
---

## Summary

Sets an internal flag to notify KeymanWeb of change in UI activation state.

## Syntax

```c
keyman.activatingUI(state);
```

### Parameters

`state`
:   Type: `boolean|number`
:   Used to signify if the user is interacting with KeymanWeb UI elements.

### Return Value

`undefined`

## Description

This function is used by the various non-mobile UI implementations to aid the KeymanWeb engine with internal bookkeeping. In particular, it is necessary for accurately tracking effects related to the active control when interacting with UI elements.

As a result, this function should only ever be utilized within custom UI code.
