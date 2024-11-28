---
title: LanguageResourceFullID protocol
---

## Summary

The **LanguageResourceFullID** protocol and the types that implement it
(`FullKeyboardID` and `FullLexicalModelID`) represent unique identifiers
for installed resource-language pairings of keyboards and lexical models
within the KeymanEngine.

`FullKeyboardID` and `FullLexicalModelID` (both of which implement this
protocol) are designed as a 'higher level' and 'friendlier' analogue to
their corresponding `LanguageResource`s and are safe to construct
dynamically for use in any of KeymanEngine's API functions.

## Syntax

``` swift
LanguageResourceFullID.fieldName
```

## Description

The `LanguageResourceFullID` protocol is frequently used for lookup
operations within the KeymanEngine, as it represents the minimal amount
of data needed to uniquely identify an installation for any
`LanguageResource`. Both `FullKeyboardID` (for keyboards) and
`FullLexicalModelID` (for lexical models) implement this protocol,
allowing methods to be written that can handle either resource type.
Also note the existence of `AnyLanguageResourceFullID`, which serves a
"type erasure" role for classes implementing this protocol.

## Associated Types

`Resource: `[`LanguageResource`](.)
:   The type of `LanguageResource` represented by this type.

## Fields

`id: String`
:   The resource's internal name. This must be unique among all resources of the same type and follows standard unique ID constraints: a-z, A-Z, 0-9, and underscores only.

`languageID: String`
:   One language paired with this resource during installation.

`type: LanguageResourceType`
:   An enum representing resource types supported by the KeymanEngine. May be either `.keyboard` (for `FullKeyboardID`) or `.lexicalModel` (for `FullLexicalModelID`).

### Example: Using `FullKeyboardID` to install a keyboard

The following script illustrates the use of `FullKeyboardID`, which implements this protocol:

``` swift
let packageURL = Bundle.main.url(forResource: "sil_euro_latin", withExtension: "kmp")!
let keyboardID = FullKeyboardID("sil_euro_latin", "fr") // Marks the keyboard for use with French.

if let keyboardPackage = try ResourceFileManager.shared.prepareKMPInstall(packageURL) as? KeyboardKeymanPackage {
  // Gets the all of the package's metadata for the specified resource, without installing it.
  // Returned type:  InstallableKeyboard
  let keyboard = keyboardPackage.findResource(withID: keyboardID)
  // ...
}
```

## History

While the classes that implement the protocol existed before this, the protocol itself was added in Keyman Engine for iPhone and iPad 14.0.

## See Also

[`LanguageResource`](.)
:   Represents all metadata used internally by the KeymanEngine for installed keyboards and lexical models.
