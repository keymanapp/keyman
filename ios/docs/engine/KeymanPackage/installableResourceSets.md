---
title: .installableResourceSets()
---

## Summary

The **`KeymanPackage.installableResourceSets`** field enumerates all resources and language pairings supported by the current package.

## Syntax

``` swift
KeymanPackage.installableResourceSets: [[ AnyLanguageResource ]]
```

### Value

The two dimensional array may be interpreted as follows:

``` swift
package.installableResourceSets[resourceIndex][languageIndex]
```

`resourceIndex`
:   Some legacy keyboard packages may support multiple keyboards. This index enumerates the unique keyboards (or lexical models) contained within the package.

`languageIndex`
:   Some resources support multiple languages. For example, Keyman's default keyboard - `sil_euro_latin` There will be one entry for each language supported by the resource.

The indexed value
:   `AnyLanguageResource` is a Swift "type erasure" that may represent either an `InstallableKeyboard` or an `InstallableLexicalModel`, each of which enumerates the metadata needed for the Keyman engine to properly load and use the resource.  
      
    These objects typically include data about the needed fonts, underlying language codes, and resource identifiers.

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`KeyboardKeymanPackage.installables: [[ InstallableKeyboard ]]`](../TypedKeymanPackage/installables)
:   The equivalent field for keyboard packages.

- [`LexicalModelKeymanPackage.installables: [[ InstallableLexicalModel ]]`](../TypedKeymanPackage/installables)` `
:   The equivalent field for lexical model packages.

- [`installableResources(forLanguage: String): [ AnyLanguageResource ]`](installableResources)
:   Provides the same functionality as this field, but filtered to a specific language code.
