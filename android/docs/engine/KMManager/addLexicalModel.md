---
title: KMManager.addLexicalModel()
---

## Summary
The **addLexicalModel()** method adds a lexical model into the lexical models list.

## Syntax

```javascript
KMManager.addLexicalModel(Context context, HashMap<String, String> lexicalModelInfo)
```

### Parameters

`context`
: The context.

`lexicalModelInfo`
: A dictionary of lexical model information with keys and values defined as `HashMap<String key, String value)`.

### Returns
Returns `true` if the lexical model was added successfully, `false` otherwise.

## Description
Use this method to include a lexical model in the lexical models list. If the lexical model with the same 
package ID, language ID, and lexical model ID exists, it updates the existing lexical model info.

After a lexical model has been added, call [registerLexicalModel()](registerLexicalModel) when you want the lexical model
to generate suggestions with an associated keyboard of a matching language.

## Examples

### Example: Using `addLexicalModel()`

The following script illustrates the use of `addLexicalModel()`:
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
* [registerLexicalModel()](registerLexicalModel)
