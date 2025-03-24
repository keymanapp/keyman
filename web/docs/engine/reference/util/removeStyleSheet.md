---
title: removeStyleSheet
---

## Summary

Remove user-defined style sheet.

## Syntax

```c
keyman.util.removeStyleSheet(Pelem);
```

### Parameters

`Pelem`
:   Type: `Element`
:   Element representing the style sheet to be removed.

### Return Value

`boolean`
:   `true` if the style sheet element was successfully removed, otherwise `false`.

## Description

If the element does not represent a style sheet, this function will not remove the element and will simply return `false`.

See also:
- [`keyman.util.addStyleSheet()`](addStyleSheet)
- [`keyman.util.linkStyleSheet()`](linkStyleSheet)
