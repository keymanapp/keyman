---
title: IKeymanError::Severity Property
---

## Introduction

The `IKeymanError::Severity` property returns the severity level of the
error.

## Specification

``` clike
readonly KeymanErrorSeverity Severity
```

## Severity Levels

| Value | Name       |
|-------|------------|
| 0     | kesFatal   |
| 1     | kesError   |
| 2     | kesWarning |
| 3     | kesHint    |
