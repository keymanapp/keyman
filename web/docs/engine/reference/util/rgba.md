---
title: rgba
---

## Summary

Browser-independent alpha-channel management.

## Syntax

```c
keyman.util.rgba(s, r, g, b, a);
```

### Parameters

`s`
:   Type: `Element`
:   Element style object

`r`
:   Type: `number`
:   Value for red (0-255)

`g`
:   Type: `number`
:   Value for green (0-255)

`b`
:   Type: `number`
:   Value for blue (0-255)

`a`
:   Type: `number`
:   Value for alpha/opacity (0-1.0)

### Return Value

`string`
:   Background color CSS value.

## Description

This function returns the required string value for setting background transparency, and applies an appropriate `a` filter to the element for IE versions before IE9.
