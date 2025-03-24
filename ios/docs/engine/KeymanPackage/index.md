---
title: KeymanPackage class
---

## Summary

The **KeymanPackage** class and its subclasses represent Keyman package
files and their contents.

## Syntax

``` swift
KeymanPackage.methodName()
```

## Description

The KeymanPackage is the base class used to represent various types of
Keyman packages within the Keyman Engine and their contents. As an
abstract class, it serves as a "type erasure" for its subclasses
`KeyboardKeymanPackage` and `LexicalModelKeymanPackage`, both of which
offer richer typing for certain fields and methods.

## Fields

`key:` [`KeymanPackage.Key`](key)
:   Returns a unique identifier for the package, usable in package-search operations.

`languages: [Language]`
:   An array of languages supported by the keyboards or lexical models contained by the package.  
      
    The most relevant fields for `Language` type are the `name: String` and `id: String` fields. (The other fields are mostly 'legacy'.)

`name: String`
:   The user-friendly name of the package.

`resourceType: LanguageResourceType`
:   An enum representing the type of LanguageResource contained within the package.  
      
    May be either `.keyboard` (for a `KeyboardKeymanPackage`) or `.lexicalModel` (for a `LexicalModelKeymanPackage`).

`sourceFolder: URL`
:   The base folder for the package's extracted contents.  
      
    ---  
    **Note:** For packages opened with [`ResourceFileManager.shared.prepareKMPInstall`](../ResourceFileManager/prepareKMPInstall), this may be a temporary location!  

    ---
      
    Stability is not guaranteed if you write to any contents of the folder or its subfolders, but read operations are permitted.

`version: Version`
:   The package's version.

## Methods

[`installableResourceSets -> [[AnyLanguageResource]]`](installableResourceSets)` `
:   Returns a list of all resources & language pairings supported by this package.

[`installableResources(forLanguage: String) -> [AnyLanguageResource]`](installableResources)` `
:   A language-filtered version of the method above.

[`pageURL(for: KeymanPackagePage) -> URL?`](pageURL)
:   Provides a link to known .html files contained by the package.  
      
    Supported pages: `.readme`, `.welcome`, `.custom(bundledPath: String)`, the latter of which may target arbitrary locations within the package.

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See Also

- [`PackageWebViewController`](../PackageWebViewController)
:   Used to display web pages contained within a package.

- [`KeyboardKeymanPackage`](../TypedKeymanPackage)
:   Represents packages containing keyboards.

- [`LexicalModelKeymanPackage`](../TypedKeymanPackage)
:   Represents packages containing lexical models.
