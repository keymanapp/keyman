---
title: KMManager.updateText()
---

## Summary

The **`updateText()`** method updates the current context with the
specified text.

## Syntax

``` javascript
KMManager.updateText(KeyboardType kbType, String text)
```

### Parameters

`kbType`
:   Keyboard type requesting the context update. `KEYBOARD_TYPE_INAPP`
    or `KEYBOARD_TYPE_SYSTEM`.

`text`
:   The text to replace the current context.

### Returns

Returns `true` if the current context was updated successfully, `false`
otherwise.

## Description

Use this method to update the current context. It must be called in
InputMethodService's onStartInput method to match the current context
with the text in the editor. Normally you do not need to call this
method for in-app keyboards.

## Examples

### Example: Using `updateText()`

The following script illustrate the use of `updateText()`:

``` javascript
    @Override
    public void onStartInput(EditorInfo attribute, boolean restarting) {
        super.onStartInput(attribute, restarting);
        // ...
        InputConnection ic = getCurrentInputConnection();
        if (ic != null) {
            ExtractedText icText = ic.getExtractedText(new ExtractedTextRequest(), 0);
            if (icText != null) {
                KMManager.updateText(KeyboardType.KEYBOARD_TYPE_SYSTEM, icText.text.toString());
                // ...
            }
        }
        // ...
    }
```

## See also

-   [updateSelectionRange()](updateSelectionRange)
