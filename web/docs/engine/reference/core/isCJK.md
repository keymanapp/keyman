---
title: isCJK
---

## Summary

Test if a given keyboard or keyboard stub (or the current keyboard) is for Chinese, Japanese, or Korean.

## Syntax

```c
keyman.isCJK(keyboard);
```

### Parameters

`keyboard`
:   Type: `object` *optional*
:   A loaded keyboard. Defaults to the currently-active keyboard if not specified.

### Return Value

`boolean`
:   `true` if the keyboard uses a pick list, as with the keyboards mentioned before, `false` if not.

## Description

East Asian keyboards (and some others) may use an IME with a "pick list" of characters, requiring special handling.
