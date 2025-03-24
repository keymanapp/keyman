---
title: KMManager.showKeyboardPicker()
---

## Summary

The **`showKeyboardPicker()`** method displays the keyboard picker menu.

## Syntax

``` javascript
KMManager.showKeyboardPicker(Context context, KeyboardType kbType)
```

### Parameters

`context`
:   The context.

`kbType`
:   The keyboard type. `KEYBOARD_TYPE_INAPP` or `KEYBOARD_TYPE_SYSTEM`.

## Description

Use this method to display keyboard picker menu. Normally you do not
need to call this method explicitly since, by default, Keyman on-screen
keyboard calls this method to display the keyboard picker menu whenever
'globe' key is tapped. Multiple calls to this method is unsafe and may
result in multiple instances of keyboard picker menu being displayed at
the same time.

## Examples

### Example: Using `showKeyboardPicker()`

The following script illustrate the use of `showKeyboardPicker()`:

``` javascript
    final Context context = this;
    ImageButton globeButton = (ImageButton) findViewById(R.id.globeButton);
    globeButton.setOnClickListener(new OnClickListener() {
        @Override
        public void onClick(View v) {
            KMManager.showKeyboardPicker(context, KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);
        }
    });
```

## See also

-   [`showLanguageList() (Deprecated)`](showLanguageList)
