---
title: TextView.setKeymanDelegate()
---

## Summary

The **`setKeymanDelegate()`** method sets the delegate.

## Syntax

```swift
TextView.setKeymanDelegate(keymanDelegate: TextViewDelegate)
```

### Parameter

`keymanDelegate`
: Keyman Delegate.

## Description

Use this TextViewDelegate instead of the normal UITextViewDelegate.

## Example

### Using `setKeymanDelegate()`
The following script illustrates the use of `setKeymanDelegate()`:

```swift
textView1 = TextView()
setTextViewStyle(textView1)
textView1.setKeymanDelegate(self)
```


