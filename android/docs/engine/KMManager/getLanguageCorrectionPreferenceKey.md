---
title: KMManager.getLanguageCorrectionPreferenceKey()
---

## Summary
A constant to use as a shared preference key to store whether the LMLayer should enable corrections for a given language.

## Syntax
```java
KMManager.getLanguageCorrectionPreferenceKey(String langID)
```

### Parameters

`langId`
: The BCP 47 language ID

### Returns
Returns the language correction preference key as a String.

## Description
Use this method to get a String that can be used as a shared preference key.

The usage of this preference key is experimental and likely to be deprecated in a future release of Keyman Engine for Android.

## Examples

### Example: Using `getLanguageCorrectionPreferenceKey()`
The following script illustrates the use of `getLanguageCorrectionPreferenceKey()`:

```java
    SharedPreferences prefs = ...; // Get the app's shared preferences
    String langID = "ta";    
    boolean key  = KMManager.getLanguageCorrectionPreferenceKey(langID);

    boolean mayCorrect = prefs.getBoolean(key, true);
```

## See also
* [getLanguagePredictionPreferenceKey()](getLanguagePredictionPreferenceKey)
