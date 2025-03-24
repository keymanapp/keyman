---
title: Manager.canAddNewKeyboards
---

## Summary

The **`canAddNewKeyboards`** method returns whether adding a new keyboard is enabled in the keyboard picker menu.

## Syntax

``` swift
Manager.canAddNewKeyboards
```

### Returns

Returns `true` if adding a new keyboard is enabled, `false` otherwise.

## Description

Use this method to check if '+' (add new keyboard) button is enabled in the keyboard picker menu.

## Examples

### Using `canAddNewKeyboards`

The following script illustrates the use of `canAddNewKeyboards`:

``` swift
    if Manager.canAddNewKeyboard {
        // add new keyboard button is enabled
    }
```
