---
title: KMManager.getLexicalModelInfo()
---

## Summary

The `getLexicalModelInfo()` method returns returns dictionary information of the specified lexical model.

## Syntax

```java
HashMap<String,String> KMManager.getLexicalModelInfo(Context context, int index)
```

### Parameters

`context`
: The context.

`index`
: Zero-based index of the installed lexical model

### Returns
Returns a dictionary `Hashmap<String, String>` containing information on the specified lexical model.
The keys for the HashMap are:
| Keys                                        |
|---------------------------------------------|
| `KMManager.KMKey_PackageID`                 |
| `KMManager.KMKey_LexicalModelID`            |
| `KMManager.KMKey_LexicalModelName`          |
| `KMManager.KMKey_LexicalModelVersion`       |
| `KMManager.KMKey_LanguageID`                |
| `KMManager.KMKey_LanguageName`              |
| `KMManager.KMKey_CustomHelpLink` (optional) |

## Description
Use this method to get details about a specified lexical model. (language and keyboard information)

## Examples

### Example: Using `getLexicalModelInfo()`

The following code illustrates the use of `getLexicalModelInfo()`:
```java
    // Gets information on the first installed lexical model
    HashMap<String, String> lexicalModelInfo = KMManager.getLexicalModelInfo(context, 0);
```
