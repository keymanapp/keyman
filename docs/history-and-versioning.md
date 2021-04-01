# History and Versioning

In Keyman 14.0, history and versioning is controlled as part of the build process,
and unified. We no longer have a separate version or history for each project. The
controlling version number is in `/VERSION.md` and the master history is in `/HISTORY.md`.

While there may be multiple versions of `HISTORY.md` in different branches, the
**master** branch is source of truth and is the version that is published to the
website.

## VERSION.md

The version shown in `VERSION.md` is the upcoming release version. This means:

  1. A snapshot of the repository will show the changes relevant to that version
  2. The build system will simply use that version number when building.
  3. If a release does not exist for the version shown, then you are at the
     bleeding edge.

## TIER.md

The release tier is updated in `TIER.md`?

## How history and version are updated

For Alpha versions (**master**), the version number is incremented at the end of a
successful Nightly build. This means that during a single day, there may be multiple
builds with the same version number. Only the last one of these is kept.

A similar process will be followed for Beta versions.

For Stable versions, the version number should be manually incremented in a pull
request (these are much rarer).

### History format

```text
## <yyyy-mm-dd> <version> <tier>

<type> [(<scope>[/<sub-scope>])]: <message> [(#<PR>)]
```

For example:

```text
## 2020-01-27 13.0.65 beta
feat(developer/compiler): Hotkeys defined in .kmn no longer need to be quoted (#2432)
chore: Release beta (#2555)
```
