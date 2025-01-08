---
title: KMManager.registerLexicalModel()
---

## Summary
The **registerLexicalModel()** method registers a lexical model to use with the LMLayer.

## Syntax
```java
KMManager.registerLexicalModel(HashMap<String, String> lexicalModelInfo)
```

### Parameters
`lexicalModelInfo`

: A dictionary of lexical model information with keys and values defined as `HashMap<String key, String value)`.

### Returns
Returns `true` if the lexical model was registered successfully, `false` otherwise.

## Description
Use this method to registers a lexical model so it will generate suggestions with an
associated keyboard of a matching language.

**Note:** When typing in a password field, the word suggestions are temporarily disabled.

## Examples

### Example: Using `registerLexicalModel()`
The following script illustrates the use of `registerLexicalModel()`:

```java
    HashMap<String, String>lexicalModelInfo = new HashMap<String, String>();
    lexicalModelInfo.put(KMManager.KMKey_PackageID, "example.ta.wordlist");
    lexicalModelInfo.put(KMManager.KMKey_LanguageID, "ta");
    lexicalModelInfo.put(KMManager.KMKey_LexicalModelID, "example.ta.wordlist");
    lexicalModelInfo.put(KMManager.KMKey_LexicalModelVersion, "1.0");
    KMManager.addLexicalModel(this, lexicalModelInfo);

    KMManager.registerLexicalModel(lexicalModelInfo);
```

## See also
* [addLexicalModel()](addLexicalModel)
* [deregisterLexicalModel()](deregisterLexicalModel)
