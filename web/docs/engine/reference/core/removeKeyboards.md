---
title: removeKeyboards
---

## Summary

Removes keyboards (by ID) from KeymanWeb.

## Syntax

```c
keyman.removeKeyboards(id[, id2, ...])
```

### Parameters

`id`
:   Type: `string`
:   keyboard name string

### Return Value

`boolean`
:   `true` if all keyboards were successfully removed, otherwise `false`.

## Description

Allows a web page designer to dynamically remove keyboards from KeymanWeb by their ID(s).

If the value `false` is returned, it is most likely that at least one specified keyboard was not actively loaded in KeymanWeb at the time of the call.
