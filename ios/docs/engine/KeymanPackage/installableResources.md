---
title: .installableResources(forLanguage:)
---

## Summary

The **`KeymanPackage.installableResources(forLanguage: String)`** method
enumerates all resources and language pairings supported by the current
package for the specified language code.

## Syntax

``` swift
KeymanPackage.installableResources(forLanguage: String) -> [ AnyLanguageResource ]
```

### Parameter

`forLanguage: String`
:   A BCP-47 language code.

### Value

An array of `InstallableKeyboard` (for a `KeyboardKeymanPackage`) or `InstallableLexicalModel` (for a `LexicalModelKeymanPackage`) instances.  
  
For most packages, there will only be a single entry in this array.
However, note that multi-keyboard packages may support multiple
keyboards for the same target language.

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`installableResourceSets: [[ AnyLanguageResource ]]`](installableResourceSets)
:   Returns a list of all resources & language pairings supported by this package.

- [`TypedKeymanPackage.installables(forLanguage: String): [ TypedLanguageResource ]`](../TypedKeymanPackage/installables_forLanguage)
:   Provides the same functionality as this method for `TypedKeymanPackage` and its subclasses, taking advantage of the class's generic specification to return a more strongly-typed array.
