---
title: LanguageResource protocol
---

## Summary

The **LanguageResource** protocol and the types that implement it
(`InstallableKeyboard` and `InstallableLexicalModel`) represent keyboards and
lexical models installed within Keyman Engine.

Generally speaking, this is a lower-level type than
[`LanguageResourceFullID`](LanguageResourceFullID), requiring extra metadata
only discovered during installation from a resource's package and only used
internally within Keyman Engine.

## Syntax

```swift
LanguageResource.fieldName
```

## Description

The `LanguageResource` protocol is the base type used to represent specific
types of installed resources within Keyman Engine, paired with their target
languages. Both `InstallableKeyboard` and `InstallableLexicalModel` implement
this protocol, allowing methods to be written that can handle either resource
type. Also note the existence of `AnyLanguageResource`, which serves a "type
erasure" role for classes implementing this protocol.

Important note: multiple copies of `LanguageResource` for the same underlying
resource will exist if that resource has been installed for use with multiple
languages. One instance will exist per target language that was selected for
installation.

## Associated Types

`FullID: LanguageResourceFullID`

: The [`LanguageResourceFullID`](LanguageResourceFullID) type used to uniquely
identify the resource, comprised of the resource's ID, type, and paired language
code. Frequently used within the engine as a unique identifier for the resource
in searches and comparisons.

`Package: KeymanPackage`

: The derived type of [KeymanPackage](../KeymanPackage) supporting this type of
resource.

For example, `InstallableKeyboard.Package == KeyboardKeymanPackage`.

## Fields

`id: String`

: The resource's internal name. This must be unique among all resources of the
same type and follows standard unique ID constraints: a-z, A-Z, 0-9, and
underscores only.

`name: String`

: The resource's external name, as displayed to users.

`languageID: String`

: One language paired with this resource during installation.

`packageID: String?`

: The ID of the resource's source package. Is 'optional' for legacy reasons.

`packageKey: `[`KeymanPackage.Key`](../KeymanPackage/key)

: The unique identifier for the resource's source package.

`version: String`

: The resource's installed version.

`sourceFilename: String`

: The filename of the package component used internally within Keyman Engine
to load the resource.

`fullID: `[`AnyLanguageResourceFullID`](LanguageResourceFullID)

: A unique identifier used in language resource searches and comparisons.

## History

While the classes that implement the protocol existed before this, the protocol
itself was added in Keyman Engine for iPhone and iPad 14.0.

## See Also

[`KeymanPackage`](../KeymanPackage)

: Represents packages containing language resources.

[`LanguageResourceFullID`](LanguageResourceFullID)

: Unique identifiers used in language resource searches and comparisons.

