---
title: IKeyman::Errors Property
---

## Introduction

The `IKeyman::Errors` property returns the Keyman
[`IKeymanErrors`](../IKeymanErrors) interface which provides a list of
errors encountered by the Keyman API, for example when installing a
keyboard or package. When the Keyman API raises an exception, e.g. when
trying to install a corrupt package file, there may be additional
details available in the `Errors` collection.

## Specification

``` clike
readonly IKeymanErrors* Errors
```
