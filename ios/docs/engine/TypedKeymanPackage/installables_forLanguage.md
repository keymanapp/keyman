---
title: .installables(forLanguage:)
---

## Summary

The **`TypedKeymanPackage.installables(forLanguage:)`** method
enumerates all resources and language pairings supported by the current
package for the specified language code.

## Syntax

```swift
TypedKeymanPackage.installables(forLanguage:) -> [ TypedLanguageResource ]
```

### Parameters

`forLanguage: String`
:   A BCP-47 language code.

### Value

An array of `InstallableKeyboard` (for a `KeyboardKeymanPackage`) or
`InstallableLexicalModel` (for a `LexicalModelKeymanPackage`)
instances.  
  
For most packages, there will only be a single entry in this array.
However, note that multi-keyboard packages may support multiple
keyboards for the same target language.

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`findResource(withID:) -> TypedLanguageResource?`](findResource)` `
:   Returns the resource matching the specified ID, if contained within the package.

- [`installables: [[ TypedLanguageResource ]]`](installables)
:   Returns a list of all resources & language pairings supported by this package.
