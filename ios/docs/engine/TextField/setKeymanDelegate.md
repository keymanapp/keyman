---
title: TextField.setKeymanDelegate()
---

## Summary

The **`setKeymanDelegate()`** method sets the delegate.

## Syntax

```swift
TextField.setKeymanDelegate(keymanDelegate: TextFieldDelegate)
```

### Parameter

`keymanDelegate`
: Keyman Delegate.

## Description

Use this TextFieldDelegate instead of the normal UITextFieldDelegate.

## Example

### Using `setKeymanDelegate()`
The following script illustrates the use of `setKeymanDelegate()`:

```swift
textField1 = TextField()
setTextFieldStyle(textField1)
textField1.setKeymanDelegate(self)
```
