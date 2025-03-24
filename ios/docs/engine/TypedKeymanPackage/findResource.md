---
title: .findResource(withID:)
---

## Summary

The **`TypedKeymanPackage.findResource(withID:)`** method allows direct lookup of a resource's metadata.

## Syntax

```swift
TypedKeymanPackage.findResource(withID:) -> TypedLanguageResource?
```

### Parameters

`withID: TypedLanguageResource.FullID`
:   The unique identifier for the desired keyboard or lexical model and target language.

### Value

- An `InstallableKeyboard` (for a `KeyboardKeymanPackage`) or `InstallableLexicalModel` (for a `LexicalModelKeymanPackage`).  
  
- May be `nil` if no resource matching the specified ID is found within the package.

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`installables: [[ TypedLanguageResource ]]`](installables)
:   Returns a list of all resources & language pairings supported by this package.

- [`installables(forLanguage: String): [ TypedLanguageResource ]`](installables_forLanguage)
:   Provides the same functionality as the field above, but filtered to a specific language code.
