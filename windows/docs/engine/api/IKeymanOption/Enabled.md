---
title: IKeymanOption::Enabled Property
---

## Introduction

The `IKeymanOption::Enabled` property returns `True` if the option value
can be changed. Some options are unavailable to be changed at certain
times, for example if the user does not have sufficient permissions, or
on certain operating system configurations.

## Specification

``` clike
readonly bool Enabled
```
