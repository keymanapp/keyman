---
title: versionInfo
---

## Summary

Returns an object describing the current KeymanWeb version.

## Syntax

```js
keyman.versionInfo
```

### Type

`object`

### Access

Read only

### Return Value

An object with the following properties:

* `full: string`
: `"major.minor.patch[-tier][-environment]"`. `tier` will be included for alpha
  and beta. `environment` will be included for local or test builds, and for
  test builds may include an additional test context such as a pull request
  number.

* `major: number`
: major component of the version string

* `minor: number`
: minor component of the version string

* `patch: number`
: patch component of the version string

* `version: string`
: `"major.minor.patch"` string

* `tier: string`
: The tier, `"stable"`, `"beta"`, or `"alpha"`

* `environment: string`
: One of `"stable"`, `"beta"`, `"alpha"`, `"local"`, or `"test"`.  For release
  builds, the same as `tier`; for development builds, `"local"`, and for test
  builds `"test"`. Test context is only included in the `full` property.

## Example return value

```js
{
  full: "18.0.249-alpha-local",
  major: 18,
  minor: 0,
  patch: 249,
  version: "18.0.249",
  tier: "alpha",
  environment: "local"
}
```

## Description

This property should be used instead of the deprecated [`keyman.build`] or
[`keyman.version`] properties. In most cases, you should use the `full` property
as the version to display to users. The `major` property can be used for version
gating.

## History

* 19.0: introduced `versionInfo` property, deprecated `build` and `version`.

## See also

* [`keyman.build`](build)
* [`keyman.version`](version)
