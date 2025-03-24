---
title: KMManager.getLanguagePredictionPreferenceKey()
---

## Summary
A constant to use as a shared preference key to store whether the LMLayer should enable suggestions for a given language.

## Syntax
```java
KMManager.getLanguagePredictionPreferenceKey(String langID)
```

### Parameters

`langId`
: The BCP 47 language ID 

### Returns
Returns the language prediction preference key as a String.

## Description
Use this method to get a String that can be used as a shared preference key.

The usage of this preference key is experimental and likely to be deprecated in a future release of Keyman Engine for Android.

## Examples

### Example: Using `getLanguagePredictionPreferenceKey()`
The following script illustrates the use of `getLanguagePredictionPreferenceKey()`:

```java
    SharedPreferences prefs = ...; // Get the app's shared preferences
    String langID = "ta";    
    boolean key = KMManager.getLanguagePredictionPreferenceKey(langID);

    boolean mayPredict = prefs.getBoolean(key, true);
```

## See also
* [getLanguageCorrectionPreferenceKey()](getLanguageCorrectionPreferenceKey)
