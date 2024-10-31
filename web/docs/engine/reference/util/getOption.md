---
title: getOption
---

## Summary

Get a KeymanWeb, On-Screen Keyboard or User Interface option value.

## Syntax

```c
keyman.util.getOption(optionName, dflt);
```

### Parameters

`optionName`
:   Type: `string`
:   The named initialization option.

`dflt`
:   Type: `string|Object` *optional*
:   The value to return if the option is not set.

### Return Value

`string|Object`
:   The value of the option (if set). If not set and no value is provided for `dflt`, returns the empty string.

## Description

See also [`keyman.util.setOption()`](setOption).
