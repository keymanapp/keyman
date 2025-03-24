---
title: addStyleSheet
---

## Summary

Generates a style sheet element for use in adding or overriding default On-Screen Keyboard styles.

## Syntax

```c
keyman.util.addStyleSheet(sheet)
```

### Parameters

`sheet`
:   Type: `string`
:   A string representing a CSS style sheet.

### Return Value

`Object`
:   A page element representing the newly-embedded style sheet.

## Description

See our list of [standard OSK style names](../osk/classes) as a reference if implementing your own stylesheets.

See also:
- [`keyman.util.linkStyleSheet()`](linkStyleSheet)
- [`keyman.util.removeStyleSheet()`](removeStyleSheet)
