---
title: KMManager.getKeyboardInfo()
---

## Summary

The **`getKeyboardInfo()`** method returns information of the specified
keyboard.

## Syntax

``` javascript
KMManager.getKeyboardInfo(Context context, int index)
```

### Parameters

`context`
:   The context.

`index`
:   0-based position of the keyboard in keyboards list.

### Returns

Returns an information `Keyboard` type of the specified keyboard.

## Description

Use this method to get details of the keyboard at given position in
keyboards list. Details include keyboard ID, language ID, keyboard name,
language name and fonts.

  

------------------------------------------------------------------------

## Syntax (Deprecated)

``` javascript
KMManager.getKeyboardInfo(Context context, int index)
```

### Parameters

`context`
:   The context.

`index`
:   0-based position of the keyboard in keyboards list.

### Returns

(Deprecated) Returns an information dictionary of the specified keyboard
with keys and values defined as `HashMap<String key, String value>`.

## Description

Use this method to get details of the keyboard at given position in
keyboards list. Details include keyboard ID, language ID, keyboard name,
language name and fonts.

## Examples

### Example: Using `getKeyboardInfo()`

The following script illustrate the use of `getKeyboardInfo()`:

``` javascript
    Keyboard keyboardInfo = KMManager.getKeyboardInfo(this, 1);
    if (keyboardInfo != null) {
        String packageID = keyboardInfo.getPackageID();
        String keyboardId = keyboardInfo.getKeyboardID();
        String keyboardName = keyboardInfo.getKeyboardName();
        String languageId = keyboardInfo.getLanguageID();
        String languageName = keyboardInfo.getLanguageName();
        String version = keyboardInfo.getVersion();
        String font = keyboardInfo.getFont();
        String oskFont = keyboardInfo.getOSKFont();
        //
    }
```

## History

Added syntax for returning Keyboard type in Keyman Engine for Android
14.0.

Deprecated syntax for returning the HashMap&lt;String key, String
value&gt; in Keyman Engine for Android 14.0

## See also

-   [`getCurrentKeyboardIndex()`](getCurrentKeyboardIndex)
-   [`getCurrentKeyboardInfo()`](getCurrentKeyboardInfo)
-   [`getKeyboardIndex()`](getKeyboardIndex)
-   [`getKeyboardsList()`](getKeyboardsList)
-   [`keyboardExists()`](keyboardExists)
