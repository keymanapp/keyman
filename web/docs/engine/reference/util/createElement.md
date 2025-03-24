---
title: createElement
---

## Summary

Create an unselectable HTML element for the *KeymanWeb* On-Screen keyboard and User Interfaces.

## Syntax

```c
keyman.util.createElement(nodeName);
```

### Parameters

`nodeName`
:   Type: `string`
:   Name for the newly-created element.

### Return Value

`Element`
:   The new, requested user-unselectable HTML element.

## Description

Elements created with this function do not capture the focus. Use `document.createElement` to create normally selectable elements.
