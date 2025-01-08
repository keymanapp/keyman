---
title: IKeyman::Apply Method
---

## Introduction

The `IKeyman::Apply` method applies the changes which have been made
through the current API instantiation to the Keyman Engine. If
[`IKeyman::AutoApply`](AutoApply) is set to `False`, `Apply` must be
called after making changes through the Keyman API. The documentation
for each method that makes changes documents whether or not `Apply`
needs to be called.

## Specification

``` clike
void Apply(void)
```
