---
title: KMManager.lexicalModelExists()
---

## Summary
The **lexicalModelExists()** method returns whether the specified lexical model exists in lexical models list.

## Syntax
```java
bool KMManager.lexicalModelExists(Context context, String packageID, String languageID, String modelID) 
```

### Parameters
`context`
: The context.

`packageID`
: ID of the package

`languageID`
: ID of the associated language

`modelID`
: ID of the lexical model

### Returns
Returns `true` if the lexical model exists in the lexical models list, otherwise `false`.

## Description
Use this method to determine if a lexical model with given package ID, language ID, and model ID exists in the lexical models list.

## Examples

### Example: Using lexicalModelExists()
The following script illustrate the use of `lexicalModelExists()`:
```java
    // Determine if the specified lexical model exists for English (en)
    boolean exists = KMManager.lexicalModelExists(context, "nrc.en.mtnt", "en", "nrc.en.mtnt");
```

## See also
* [`getLexicalModelInfo()`](getLexicalModelInfo)
