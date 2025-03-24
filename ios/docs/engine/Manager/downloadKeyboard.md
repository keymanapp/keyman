---
title: Manager.downloadKeyboard()
---

## Summary

The **`downloadKeyboard()`** method installs keyboards for active use.

## Syntax

``` swift
Manager.downloadKeyboard(withID: String, languageID: String, isUpdate: Bool, fetchRepositoryIfNeeded: Bool = true)
```

### Parameters

`withID`
:   ID of the keyboard.

`languageID`
:   ID of the language.

`isUpdate`
:   Boolean to keep the keyboard files on failure.

`fetchRepositoryIfNeeded`
:   Fetch the list of keyboards from the API if necessary.

## Description

This syntax can be used to asynchronously fetch the keyboard files.

### Example 1: Using `downloadKeyboard()`

The following script illustrates the use of `downloadKeyboard()`:

``` swift
Manager.shared.downloadKeyboard(withID: keyboard.id, languageID: keyboard.languageID, isUpdate: false)
```
    
------------------------------------------------------------------------

## Syntax

``` swift
Manager.downloadKeyboard(url: URL)
```

### Parameter

`url`
:   URL to a .KMP package containing the keyboard.

## Description

This syntax can be used to asynchronously fetch the keyboard files.

### Example 2: Using `downloadKeyboard()`

The following script illustrates the use of `downloadKeyboard()`:

``` swift
let url = URL(string: "https://example.com/keyboard.kmp")
Manager.shared.downloadKeyboard(from: url!)
```
