---
title: KPS files
---

Used by:
:   <span class="application">Keyman Developer</span>.

Description:
:   A .KPS file is a Keyman package source file.

Details:
:   A .KPS file is created using the Package Editor in
    <span class="application">Keyman Developer</span>. It specifies what
    files are to be included in the package. It is compiled into a
    Keyman Package file ([.KMP](kmp)).

Distributed with keyboard:
:   No. This is a development file and should not be distributed.

# File format

## Schema

The schema for .kps is documented at:
https://github.com/keymanapp/keyman/tree/master/common/schemas/kps

## Strings
The optional `<Strings>` section of the file can be included to
customise the text in the bootstrap installer. The default strings are
found in the file:
[strings.xml](https://github.com/keymanapp/keyman/blob/stable-14.0/windows/src/desktop/setup/locale/en/strings.xml).
You can add your own strings for a given package which are used when
compiling as a bundled installer.


## File.Name

The `Name` element may be either a local filename, or a GitHub raw permanent
URL, conforming to the pattern:

```
<Name>https://github.com/{owner}/{repo}/raw/{commit}/[path/]{filename}</Name>
```

## File.Source

The `<Source>` element documents the original source of the file, from
one of several supported sources:
* flo - fonts.languagetechnology.org
* github
* noto - in the future

While the `<Name>` element must refer to either a local file or to a permanent
GitHub file (as recognized by the presence of a commit hash), the `<Source>`
element typically will refer to a file which may change over time.

The `<Source>` element is supported in version 18.0 and later of .kps
format.

### Local file system

The `<Source>` element must not be included for files sourced from the local
file system, where `<Name>` references a file on the local filesystem.

Note: the `<Source>` element is optional for other sources as well; the absence
of the element does not mean that the `<Name>` element is necessarily
referencing a local file -- that is determined by pattern match.

```xml
<File>
  <Name>[relative-or-absolute-path/]{filename}</Name>
</File>
```

Note: absolute paths are not recommended because they are not portable to
other computers.

#### Example

```xml
<File>
  <Name>Alkalami-Regular.ttf</Name>
</File>
```

```xml
<File>
  <Name>../../shared/fonts/Alkalami-Regular.ttf</Name>
</File>
```

### fonts.languagetechnology.org `flo:<id>`

References a font by ID from fonts.languagetechnology.org. The resource must be
resolved to a permanent URL. If the `<Name>` field is missing, the compiler will
return an error, reporting the URL it discovers, so the author should be able to
paste that URL in.

If the URL in the `<Name>` field differs from the resolved URL given by FLO,
then a hint will be issued by the compiler, allowing the author to easily update
to a new version of the resource at their convenience. It is anticipated that
this may be scripted on the keyboards repository in the future.

```xml
<File>
  <Name>https://github.com/{owner}/{repo}/raw/{commit}/[path/]{filename}</Name>
  <Source>flo:{id}</Source>
</File>
```

#### Example

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>flo:alkalami</Source>
</File>
```

### GitHub

A reference to a resource located on GitHub. The resource must be resolvable to
a permanent URL. If the `<Name>` field is missing, the compiler will return an
error, reporting the URL it discovers, so the author should be able to paste
that URL in.

If the URL in the `<Name>` field differs from the resolved URL from the
`<Source>` element, then a hint will be issued by the compiler, allowing the
author to easily update to a new version of the resource at their convenience.
It is anticipated that this may be scripted on the keyboards repository in the
future.

```xml
<File>
  <Name>https://github.com/{owner}/{repo}/raw/{commit}/[path/]{filename}</Name>
  <Source>https://github.com/{owner}/{repo}/(raw|blob)/([refs/heads/]{branch}|[refs/tags/]{tag}|{commit})/[path/]{filename}</Source>
</File>
```

#### Examples

A GitHub raw reference, from a branch:

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>https://github.com/silnrsi/fonts/raw/main/fonts/sil/alkalami/Alkalami-Regular.ttf</Source>
</File>
```

Including `/refs/heads/` is also acceptable (as often found in GitHub UI):

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>https://github.com/silnrsi/fonts/raw/refs/heads/main/fonts/sil/alkalami/Alkalami-Regular.ttf</Source>
</File>
```

A GitHub blob reference, as copied from a URL in GitHub:

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>https://github.com/silnrsi/fonts/blob/main/fonts/sil/alkalami/Alkalami-Regular.ttf</Source>
</File>
```

A GitHub raw reference, from a commit, resolves identically. Note that as the
`<Source>` element includes a commit hash, the referenced file can never change
and so there is no real benefit in including it:

```xml
<File>
  <Name>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Name>
  <Source>https://github.com/silnrsi/fonts/raw/b88c7af5d16681bd137156929ff8baec82526560/fonts/sil/alkalami/Alkalami-Regular.ttf</Source>
</File>
```

### noto

In a future version.

## Additional file format notes

The `CopyLocation`, `Description`, and `FileType` elements are deprecated as of
v18.0 and totally ignored by the compiler and end-user apps.

