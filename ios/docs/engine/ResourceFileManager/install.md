---
title: .install()
---

## Summary

The **`install()`** method is used to install resources from opened package files.

## Syntax

```swift
ResourceFileManager.shared.install(resourceWithID: URL, from: KeymanPackage) throws
```

### Parameters

`resourceWithID`
:   Either a [`FullKeyboardID`](../LanguageResource/LanguageResourceFullID) or a [`FullLexicalModelID`](../LanguageResource/LanguageResourceFullID) corresponding to the resource to be installed from the package.

`from`
:   The opened package file

### Throws

May fail if the package does not contain or support the designated
resource and language pairing specified by the resource's ID.

## Description

This syntax can be used for installing resources from a KMP package.

### Example: Using `install()` to install a keyboard

The following script illustrates the use of `install()`:

```swift
let packageURL = Bundle.main.url(forResource: "sil_euro_latin", withExtension: "kmp")!
let keyboardID = FullKeyboardID("sil_euro_latin", "fr") // Marks the keyboard for use with French.
do {
  if let keyboardPackage = try ResourceFileManager.shared.prepareKMPInstall(packageURL) as? KeyboardKeymanPackage {
    try ResourceFileManager.shared.install(resourceWithID: keyboardID, from: keyboardPackage)
  } else {
    // Report the error
  }
} catch {
  // Report the error
}
```

## History

- Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- [`LanguageResourceFullID`](../LanguageResource/LanguageResourceFullID)
:   A loaded package file containing one or more keyboards.

- `KeyboardKeymanPackage`
:   A loaded package file containing one or more keyboards.

- `LexicalModelKeymanPackage`
:   A loaded package file containing a lexical model.

- [`ResourceFileManager.shared.prepareKMPInstall(from:)`](prepareKMPInstall)
:   This method is used to open packages for use by `install()`.
