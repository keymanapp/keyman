---
title: TypedKeymanPackage class
---

## Summary

The **TypedKeymanPackage** class and its subclasses provide precise typing for the contents of a [`KeymanPackage`](../KeymanPackage).

## Syntax

```swift
TypedKeymanPackage<TypedLanguageResource>.methodName()
```

### Generic Parameters

`TypedLanguageResource: `[`LanguageResource`](../LanguageResource)
:   The type of resource contained within the package. Should be either `InstallableKeyboard` or `InstallableLexicalModel`

## Description

TypedKeymanPackage is the base class of `KeyboardKeymanPackage` and
` LexicalModelKeymanPackage`, both of which represent richly-typed
parses of their corresponding package files.

---
**Note:** The type erasure, [`KeymanPackage`](../KeymanPackage), will
often be easier to work with in methods looking to handle both types of
packages due to Swift's typing constraints.

---

## Methods and Fields

[`findResource(withID:) -> TypedLanguageResource?`](findResource)` `
:   Returns the resource matching the specified ID, if contained within
    the package.

[`installables: [[TypedLanguageResource]]`](installables)` `
:   Returns a list of all resources & language pairings supported by
    this package.  
      
    For `KeyboardKeymanPackage`, this resolves to
    `[[InstallableKeyboard]]`, while for `LexicalModelKeymanPackage`,
    this resolves to `[[InstallableLexicalModel]]`.

[`installables(forLanguage: String) -> [TypedLanguageResource]`](installables_forLanguage)` `
:   A language-filtered version of the field above, with the 'language
    index' dimension dropped.  
      

## History

Added in Keyman Engine for iPhone and iPad 14.0.

## See Also

- [`KeymanPackage`](../KeymanPackage)
:   The base class (and type erasure) for packages.
