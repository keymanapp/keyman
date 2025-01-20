---
title: alert
---

## Summary

Generates a KeymanWeb alert window.

## Syntax

```c
keyman.util.alert(msg, fn)
```

### Parameters

`msg`
:   Type: `string`
:   Alert message text.

`fn`
:   Type: `function()` *optional*
:   Function to be called upon alert dismissal.

### Return Value

`undefined`

## Description

The optional second parameter specifies a function to be called when the
window is dismissed.