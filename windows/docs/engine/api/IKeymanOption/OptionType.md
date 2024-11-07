---
title: IKeymanOption::OptionType Property
---

## Introduction

The `IKeymanOption::OptionType` property returns the data type for the
[`Value`](Value) and [`DefaultValue`](DefaultValue) properties.

## Specification

``` clike
readonly KeymanOptionType OptionType
```

## Option Data Types

| Value | Name      |
|-------|-----------|
| 0     | kotUnknown|
| 1     | kotBool   |
| 2     | kotLong   |
| 3     | kotString |

