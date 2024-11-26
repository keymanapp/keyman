---
title: IKeymanBCP47Canonicalization::GetCanonicalTag Method
---

## Introduction

The `IKeymanBCP47Canonicalization::GetCanonicalTag` method returns a
canonical tag according to data from [SIL
langtags.json](https://github.com/silnrsi/langtags). The method will
convert ISO639-3 language tags, ISO639-1 language tags, remove
suppressed script tags, and append a default region if one is not
present.

**Note:** This tag may not always be identical to a Windows canonical
tag. A region subtag may be present in the canonical tag which may later
be removed by Windows, if Windows believes the language is used in only
one region.

## Specification

``` clike
string GetCanonicalTag(string Tag)
```

## Parameters

`Tag`
:   The BCP 47 tag to canonicalize.
