---
title: KMManager.createInputView()
---

## Summary

The **`createInputView()`** creates the input view to be used in
InputMethodService.

## Syntax

``` javascript
KMManager.createInputView(InputMethodService inputMethodService)
```

### Parameters

`inputMethodService`
:   The InputMethodService.

### Returns

Returns the input view created.

## Description

Use this method to create the input view in response to an
InputMethodService's `onCreateInputView()` method.

## Examples

### Example: Using `createInputView()`

The following script illustrate the use of `createInputView()`:

``` javascript
private static View inputView = null;

@Override
public View onCreateInputView() {
    // create the inputView only once
    if (inputView == null)
        inputView = KMManager.createInputView(this);

    // we must remove the inputView from its previous parent before returning it
    ViewGroup parent = (ViewGroup) inputView.getParent();
    if (parent != null)
        parent.removeView(inputView);

    return inputView;
}
```

## See also

-   [`onStartInput()`](onStartInput)
-   [`onConfigurationChanged()`](onConfigurationChanged)
-   [`onDestroy()`](onDestroy)
