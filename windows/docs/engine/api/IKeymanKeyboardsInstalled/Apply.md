---
title: IKeymanKeyboardsInstalled::Apply Method
---

## Introduction

The `IKeymanKeyboardsInstalled::Apply` method applies any changes which
have been made to the collection to the Keyman Engine. `Apply` must be
called if [`IKeyman::AutoApply`](../IKeyman/AutoApply) is set to
`False`, after making any changes through the Keyman API. The
documentation for each method that makes changes documents whether or
not `Apply` needs to be called.

Generally, it is better to call the top-level
[`IKeyman::Apply`](../IKeyman/Apply) as that will ensure that changes to
be applied will be consistently applied from all parts of the API at
once. It is also faster than calling each collection's `Apply` method
separately in order.

## Specification

``` clike
void Apply(void)
```
