---
title: IKeymanCollection::Refresh() Method
---

## Introduction

The `IKeymanCollection::Refresh()` method refreshes the collection. All
existing elements in the collection are invalidated, even if nothing has
changed.

Generally, it is better to call the top-level
[`IKeyman::Refresh`](../IKeyman/Refresh) as that will ensure that the
Keyman API snapshot is internally consistent. See
[`IKeyman::Refresh`](../IKeyman/Refresh) for more information on Keyman
API snapshots.

## Specification

``` clike
void Refresh(void)
```
