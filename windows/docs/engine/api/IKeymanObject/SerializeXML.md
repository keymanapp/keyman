---
title: IKeymanObject::SerializeXML Method
---

## Introduction

The `IKeymanObject::SerializeXML` method serializes the properties and
owned objects of the object as XML. The content of the XML varies by
interface. Some objects will serialize image data as well, for example
keyboard files will serialize an icon for the keyboard, which will be
saved to disk as a temporary file.

## Specification

``` clike
string SerializeXML(KeymanSerializeFlags Flags, string ImagePath, out string[] References)
```

## Parameters

`Flags`
:   Can be `0` or `ksfExportImages`.

`ImagePath`
:   If `ksfExportImages` is set, then this must contain a valid fully
    qualified path where temporary image files will be saved.

`References`
:   If `ksfExportImages` is set, then on return includes a list of all
    image files generated for the XML.

## Return Value

`IKeymanObject::SerializeXML` returns a well-formed snippet of XML in a
string.
