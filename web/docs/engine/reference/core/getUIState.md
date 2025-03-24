---
title: getUIState
---

## Summary

Get the KeymanWeb user interface activation state.

## Syntax

```c
keyman.getUIState()
```

### Parameters

None.

### Return Value

`Object`
:   A '`ui_state`' object specifying the UI's current activation state. Please see below for more details.

## Description

The `ui_state` object contains the following members:

`activationPending`
:   `boolean`
:   Indicates that the KeymanWeb UI is being activated.

`activated`
:   `boolean`
:   Indicates that the KeymanUI is presently active.
