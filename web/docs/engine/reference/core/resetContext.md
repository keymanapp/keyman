---
title: resetContext
---

## Summary

Reverts the OSK to the default layer, clears any processing caches and modifier
states, and clears deadkeys and prediction-processing states on the active
element (if it exists)

## Syntax

```js
keyman.resetContext();
```

### Parameters

None.

### Return Value

`undefined`

## Description

This function can be used by a site to reset KeymanWeb's internal context state.
This could be used after direct manipulation of a text store by the site -- for
example, a paste command -- so that users are not presented with unexpected
predictions or deadkeys.
