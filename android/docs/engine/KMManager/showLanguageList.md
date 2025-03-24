---
title: KMManager.showLanguageList() (Deprecated)
---

## Summary

(Deprecated) The **`showLanguageList()`** method displays the language
list.

## Syntax

``` javascript
KMManager.showLanguageList(Context context)
```

### Parameters

`context`
:   The context.

## Description

The Keyman keyboard picker menu used to call this method to display the
language list whenever the '+' button was tapped. As of 14.0, keyboards
are no longer added this way, and this call is removed.

## Examples

### Example: Using `showLanguageList()`

The following script illustrate the use of `showLanguageList()`:

``` javascript
    final Context context = this;
    ImageButton languageButton = (ImageButton) findViewById(R.id.languageButton);
    languageButton.setOnClickListener(new OnClickListener() {
        @Override
        public void onClick(View v) {
            KMManager.showLanguageList(context);
        }
    });
```

## History

Deprecated syntax in Keyman Engine for Android 14.0

## See also

-   [`showKeyboardPicker()`](showKeyboardPicker)
