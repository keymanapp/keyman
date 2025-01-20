---
title: Manager.downloadLexicalModel()
---

## Summary

The **`downloadLexicalModel()`** method installs dictionaries (lexical models) for active use.

## Syntax

``` swift
Manager.downloadLexicalModel(withID: String, languageID: String, isUpdate: Bool, fetchRepositoryIfNeeded: Bool = true)
```

### Parameters

`withID`
:   ID of the dictionary.

`languageID`
:   ID of the language.

`isUpdate`
:   Boolean to keep the dictionary files on failure.

`fetchRepositoryIfNeeded`
:   Fetch the list of language resources from the API if necessary.

## Description

This syntax can be used to asynchronously fetch dictionary files.

### Example 1: Using `downloadLexicalModel()`

The following script illustrates the use of `downloadLexicalModel()`:

``` swift
Manager.shared.downloadLexicalModel(withID: model.id, languageID: model.languageID, isUpdate: false)
```

------------------------------------------------------------------------

## Syntax

``` swift
Manager.downloadLexicalModel(url: URL)
```

### Parameter

`url`
:   URL to a .MODEL.KMP package containing the dictionary.

## Description

This syntax can be used to asynchronously fetch the dictionary files.

### Example 2: Using `downloadLexicalModel()`

The following script illustrates the use of `downloadLexicalModel()`:

``` swift
let url = URL(string: "https://example.com/en.example.model.kmp")
Manager.shared.downloadLexicalModel(from: url!)
```
