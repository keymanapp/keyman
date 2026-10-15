---
title: toNumber (deprecated)
---

## Summary

String -&gt; number conversion, with default.

## Syntax

```js
keyman.util.toNumber(s, dflt);
```

### Parameters

`s`
:   Type: `string`
:   Numeric string.

`dflt`
:   Type: `number`
:   Default value if parse is unsuccessful.

### Return Value

`number`
:   The string's conversion to a whole-number numeric value, or the default value if unsuccessful.

## Description

This is a simple wrapper around `parseInt(s, 10)` to handle invalid inputs, for which
`dflt` will be returned instead.

This function has been deprecated and will be removed in a future version of
KeymanWeb.

## History

19.0: deprecated
