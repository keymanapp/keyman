---
title: IKeymanPackageContentFile Interface
---

## Introduction

The `IKeymanPackageContentFile` interface describes a file within a
package.

## Interface Hierarchy

`IDispatch`  

> [`IKeymanObject`](../IKeymanObject)  
>
> > **`IKeymanPackageContentFile`**  

## Properties

[`Description`](Description) <span class="readonly">read only</span>
:   An optional description of the file from the package metadata.

[`Filename`](Filename) <span class="readonly">read only</span>
:   The filename of the file, without path.

[`FullFilename`](FullFilename) <span class="readonly">read only</span>
:   The filename of the file, including path. This may point to a
    temporary path.

[`Stream`](Stream) <span class="readonly">read only</span>
:   Returns an `IStream` reference to the file content.
