---
title: .prepareKMPInstall()
---

## Summary

The **`prepareKMPInstall()`** method imports and parses package files, making their contents easily accessible for installation.

## Syntax

```swift
ResourceFileManager.shared.prepareKMPInstall(from: URL) throws -> KeymanPackage
```

### Parameter

`from`
:   URL to the KMP package.

### Returns

Returns either a `KeyboardKeymanPackage` or a `LexicalModelKeymanPackage`, depending on which type of resource the .kmp package represents.

### Throws

May fail if the file specified by the URL does not exist, is not a KMP, or if the KMP is malformed or corrupted.

## Description

This syntax can be used for installing resources from a KMP package.

### Example: Using `prepareKMPInstall()` to install a keyboard

The following script illustrates the use of `prepareKMPInstall()`:

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

---
**Note:** the package and keyboard must explicitly support your selected language code.

---

## History

- Added in Keyman Engine for iPhone and iPad 14.0.

## See also

- `KeyboardKeymanPackage`
:   A loaded package file containing one or more keyboards.

- `LexicalModelKeymanPackage`
:   A loaded package file containing a lexical model.

- [`ResourceFileManager.shared.install(resourceWithID:,from:)`](install)
:   This method is used to install specific resources from package files opened by `prepareKMPInstall()`.
