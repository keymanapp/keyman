---
title: .installables()
---

## Summary

The **`TypedKeymanPackage.installables`** field enumerates all resources and language pairings supported by the current package. This is equivalent to [` KeymanPackage.installableResourceSets`](../KeymanPackage/installableResourceSets), but with more precise typing.

## Syntax

```swift
TypedKeymanPackage.installables: [[ TypedLanguageResource ]]
```

### Value

The two dimensional array may be interpreted as follows:

```swift
package.installables[resourceIndex][languageIndex]
```

`resourceIndex`
:   Some legacy keyboard packages may support multiple keyboards. This
    index enumerates the unique keyboards (or lexical models) contained
    within the package.

`languageIndex`
:   Some resources support multiple languages. For example, Keyman's
    default keyboard - `sil_euro_latin` There will be one entry for each
    language supported by the resource.

The indexed value
:   An `InstallableKeyboard` (for a `KeyboardKeymanPackage`) or an
    `InstallableLexicalModel` (for a `LexicalModelKeymanPackage`).  
      
    These objects include data about the resource's fonts, targeted
    language code, and resource identifiers.

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`findResource(withID:) -> TypedLanguageResource?`](findResource)` `
:   Returns the resource matching the specified ID, if contained within
    the package.

- [`installables(forLanguage: String): [ TypedLanguageResource ]`](installables_forLanguage)
:   Provides the same functionality as this field, but filtered to a
    specific language code.
