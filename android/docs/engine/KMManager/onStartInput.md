---
title: KMManager.onStartInput()
---

## Summary

The **`onStartInput()`** method performs necessary actions in an
InputMethodService's `onStartInput()`.

## Syntax

``` javascript
KMManager.onStartInput(EditorInfo attribute, boolean restarting)
```

### Parameters

`attribute`
:   Description of the type of text being edited.

`restarting`
:   Set to true if we are restarting input on the same text field as
    before.

## Description

To be called from an InputMethodService's `onStartInput()` method.

## Examples

### Example: Using `onStartInput()`

The following script illustrate the use of `onStartInput()`:

``` javascript
@Override
public void onStartInput(EditorInfo attribute, boolean restarting) {
    // ...
    super.onStartInput(attribute, restarting);
    KMManager.onStartInput(attribute, restarting);
    // ...
}
```

## See also

-   [`createInputView()`](createInputView)
-   [`onConfigurationChanged()`](onConfigurationChanged)
-   [`onDestroy()`](onDestroy)
