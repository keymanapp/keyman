---
title: IKeyman::AutoApply Property
---

## Introduction

The `IKeyman::AutoApply` property determines if the API will apply
changes automatically to the Keyman Engine after each change is made.
Some changes are applied immediately, such as installing a keyboard
layout. However other changes will not be applied unless the
[`Apply`](Apply) method is called, or unless `AutoApply` is set to
`True`. The documentation for each method that makes changes documents
whether or not `Apply` needs to be called.

`AutoApply` defaults to `True`.

## Specification

``` clike
bool AutoApply
```
