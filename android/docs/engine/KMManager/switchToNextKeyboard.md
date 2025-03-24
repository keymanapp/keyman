---
title: KMManager.switchToNextKeyboard()
---

## Summary

The **`switchToNextKeyboard()`** method loads the next available
keyboard in keyboards list.

## Syntax

``` javascript
KMManager.switchToNextKeyboard(Context context)
```

### Parameters

`context`
:   The context.

## Description

Use this method to switch to next keyboard in the keyboards list. If the
next keyboard does not exists, then it loads the first keyboard in the
keyboards list.

## Examples

### Example: Using `switchToNextKeyboard()`

The following script illustrate the use of `switchToNextKeyboard()`:

``` javascript
    final Context context = this;
    ImageButton nextButton = (ImageButton) findViewById(R.id.nextButton);
    nextButton.setOnClickListener(new OnClickListener() {
        @Override
        public void onClick(View v) {
            KMManager.switchToNextKeyboard(context);
        }
    });
```

## See also

-   [`setKeyboard()`](setKeyboard)
