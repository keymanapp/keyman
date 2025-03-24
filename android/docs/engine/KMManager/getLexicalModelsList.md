---
title: KMManager.getLexicalModelsList()
---

## Summary
The **getLexicalModelsList()** method returns the array of lexical models list.

## Syntax
```java
ArrayList<HashMap<String, String>> KMManager.getLexicalModelsList(Context context)
```

### Parameters
`context`
: The context.

### Returns
Returns a list of all the installed lexical models.

## Description
Use this method to get details of all the installed lexical models.

## Examples

### Example: Using getLexicalModelsList()
The following script illustrate the use of `getLexicalModelsList()`:
```java
    ArrayList<HashMap<String, String>> lexicalModelList = KMManager.getLexicalModelsList(context);
```

## See also
* [getLexicalModelInfo()](getLexicalModelInfo)
