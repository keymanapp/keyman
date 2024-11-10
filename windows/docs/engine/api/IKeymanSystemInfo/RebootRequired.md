---
title: IKeymanSystemInfo::RebootRequired Property
---

## Introduction

The `IKeymanSystemInfo::RebootRequired` property returns `True` if a
previous action performed through the Keyman Engine API cannot be
completed until the system is restarted. Typically this can arise if a
file is locked by another process at install time, and is most likely to
occur with font installation and removal.

## Specification

``` clike
readonly bool RebootRequired
```
