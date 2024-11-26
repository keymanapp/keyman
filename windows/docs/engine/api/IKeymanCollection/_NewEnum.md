---
title: IKeymanCollection::_NewEnum() Method
---

## Introduction

The `IKeymanCollection::_NewEnum()` method instantiates an enumerator
for the collection. This is required for COM enumeration of the
collection. Generally, languages provide a method of iterating over a
collection that uses `_NewEnum()` internally, and you will not be
required to call this directly.

## Specification

``` clike
IUnknown* _NewEnum(void)
```
