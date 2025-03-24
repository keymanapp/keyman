---
title: KeymanPackage.Key class
---

## Summary

The **KeymanPackage.Key** class serves as a unique identifier for packages.

## Syntax

``` swift
KeymanPackage.Key.field
```

## Description

The KeymanPackage.Key class is used within the KeymanEngine by installed resources when there is a need to find and/or use their source packages. Such operations only succeed with installed packages.

## Fields

`id: String`
:   The package's id string.  
      
    Generally speaking, this is the filename for the package file once the file extension (`.kmp` for keyboard packages, `.model.kmp` for lexical model packages) has been removed.

`type: LanguageResourceType`
:   An enum representing resource types supported by the KeymanEngine. May be either `.keyboard` (for a `KeyboardKeymanPackage`) or `.lexicalModel` (for a `LexicalModelKeymanPackage`).

## History

Added in Keyman Engine for iPhone and iPad 14.0.

