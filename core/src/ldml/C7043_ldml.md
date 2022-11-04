# KMX Plus Binary Format #7043

- copied from <https://github.com/keymanapp/keyman/issues/7043>

- Authors: MD SL

## C7043.0 Introduction

This document discusses the binary format for KMXPlus, which contains keyboards
converted from source LDML data.

Draft spec PR: <https://github.com/unicode-org/cldr/pull/1847>

## C7043.1 Principles

- The data described here is located at byte offset `dpKMXPlus`.
- All integer values are unsigned 32-bit little-endian unless otherwise
  specified.
- All strings are UTF-16LE unless otherwise specified. (See the `strs` section.)
  String data items are identified with `str:`, indicating a 32 bit index into
  the `strs` table.
- All offsets are 32-bit little-endian values.  For all sections except for the
  `'sect'` section (which see), offsets are relative to the beginning of each
  section.

## C7043.2 Sections

- Data is divided into several sections. The very first section is the `'sect'`
  section which is the table of contents.
- All sections, including the first, begin on a 128-bit boundary with zero
  padding as needed.
- All variable length sections have the variable part begin on a 128-bit
  boundary, with zero padding as needed.
- All sections begin with a 32-bit (four character) section identifier, and a
  32-bit section size.
- Other than the `sect` table itself, the rest of the sections follow in binary
  order in the file.  In other words, the binary ordering of the section
  identifiers determines the order of the file layout.
- All sections other than the `sect` table are optional

### C7043.2.1 `sect`—Section Table of contents

The very first section is a table of contents listing the rest of the sections.
The table of contents does not list itself.

This is the only section where all byte offsets are relative to the value of
`dpKMXPlus`.

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0 |  32  | ident   | `sect`                                   |
| 4 |  32  | size    | int: Length of section                   |
| 8 |  32  | total   | int: KMXPlus entire length               |
|12 |  32  | count   | int: Number of following section headers |

Then for `count` repetitions:

| ∆ | Bits | Name    | Description                                       |
|---|------|---------|---------------------------------------------------|
|16+|  32  | sect    | Section identity, e.g. `loca`                     |
|20+|  32  | offset  | off: offset relative to `dpKMXPlus` of section    |

This list is in sorted order based on the `sect` identifier.

### C7043.2.2 `bksp`—Backspace transform

See [C7043.2.11](#c7043211-tran-finl-and-bksptransforms).

### C7043.2.3 `elem`—Transform and Reorder element strings

| ∆ | Bits | Name     | Description                              |
|---|------|----------|------------------------------------------|
| 0 |  32  | ident    | `elem`                                   |
| 4 |  32  | size     | int: Length of section                   |
| 8 |  32  | count    | int: Number of element string entries    |
|12 |  32  | reserved | padding                                  |

Then for each element string:

| ∆ | Bits | Name          | Description                                   |
|---|------|---------------|-----------------------------------------------|
|16+|  32  | offset        | off: Offset to element string                 |
|20+|  32  | length        | int: Number of elements in element string     |

The first entry in the element string list MUST be the null element string,
which has zero length and zero offset.

Each element string is made up of elements with the following item structure:

| ∆ | Bits | Name      | Description                                              |
|---|------|-----------|----------------------------------------------------------|
| 0 |  32  | element   | str: output string OR UTF-32LE codepoint                 |
| 4 |  32  | flags     | flags and order values                                   |

- `element`: either a UnicodeSet stored in a `strs` section entry, or a UTF-32LE
  codepoint; see also `unicode_set` flag.
- `flags`: a 32-bit bitfield defined as below:

  | Bit position | Meaning       | Description                      |
  |--------------|---------------|----------------------------------|
  |       0      | unicode_set   | `element` is 0: UTF-32LE, 1: str |
  |       1      | tertiary_base | 1: tertiary_base is true         |
  |       2      | prebase       | 1: prebase is true               |
  |      3-15    | reserved      | reserved                         |
  |     16-23    | order         | signed int: -128 to +127         |
  |     24-31    | tertiary      | signed int: -128 to +127         |

  For transforms, only `flags.unicode_set` will be used. The remaining flags are
  used for reorders, `from` attribute only.

### C7043.2.4 `finl`—Final transform

See [C7043.2.11](#c7043211-tran-finl-and-bksptransforms).

### C7043.2.5 `keys`—Keybag

| ∆ | Bits | Name      | Description                              |
|---|------|-----------|------------------------------------------|
| 0 |  32  | ident     | `keys`                                   |
| 4 |  32  | size      | int: Length of section                   |
| 8 |  32  | count     | int: Number of keys                      |
|12 |  32  | reserved  | reserved                                 |

The keys are sorted in binary order based on the `vkey` and `mod` fields.

For each key:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|16+|  32  | vkey    | int: vkey ID                             |
|20+|  32  | mod     | int: modifier key flags                  |
|24+|  32  | to      | str: output string OR UTF-32LE codepoint |
|28+|  32  | flags   | int: per-key flags                       |

- `vkey`: If this is 0-255, it is the resolved standard/predefined vkey (K_A,
  etc.). It is resolved because the `vkeyMap` from LDML has already been
  applied.  If this is 256 or above, it is a custom touch layout vkey generated
  by the compiler.
- `mod`: TODO define this.  0 for no modifiers.
- `flags`: Flags is a 32-bit bitfield defined as below:

| Bit position | Meaning  |  Description                                |
|--------------|----------|---------------------------------------------|
|       0      | extend   | 0: `to` is a char, 1: `to` is a string      |

- `to`: If `extend` is 0, `to` is a UTF-32LE codepoint. If `extend` is 1, `to`
  is a 32 bit index into the `strs` table. The string may be zero-length.

### C7043.2.6 `loca`—Locales

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0 |  32  | ident   | `loca`                                   |
| 4 |  32  | size    | int: Length of section                   |
| 8 |  32  | count   | int: Number of locales                   |
|12 |  32  | reserved| padding                                  |

`count` is always ≥1, because a keyboard always has a primary locale identifier.

For each locale ID in `count`

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|16+|  32  | locale  | str: Locale ID in BCP47 format           |

The first locale ID is always the primary locale identifier.  The rest of the
locale IDs (starting at offset 16) are in sorted binary order.

### C7043.2.7 `meta`—Metadata

| ∆ | Bits | Name          | Description                         |
|---|------|---------------|-------------------------------------|
| 0 |  32  | ident         | `meta`                              |
| 4 |  32  | size          | int: Length of section              |
| 8 |  32  | author        | str: Keyboard author                |
|12 |  32  | conform       | str: CLDR 'conformsTo' version      |
|16 |  32  | layout        | str: layout type                    |
|20 |  32  | normalization | str: normalization mode             |
|24 |  32  | indicator     | str: indicator                      |
|28 |  32  | settings      | int: keyboard settings              |

The `settings` is a 32-bit bitfield as below:

| Bit position | Meaning          |  Description                 |
|--------------|------------------|------------------------------|
|       0      | fallback         | fallback=omit                |
|       1      | transformFailure | transformFailure=omit        |
|       2      | transformPartial | transformPartial=hide        |

### C7043.2.8 `name`—Names

Defines the names of the keyboard as found in the source `<names>` element.
While this section is optional in the binary format, in practice it will always
be present, as the source format requires at least one name.

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0 |  32  | ident   | `name`                                   |
| 4 |  32  | size    | int: Length of section                   |
| 8 |  32  | count   | int: Number of names                     |
|12 |  32  | reserved| padding                                  |

Note that `count` is always ≥1, as the source format requires at least one name.

For each name in `count`:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|16+|  32  | name    | str: A name for the keyboard             |

Names are stored in source file order, and the semantic meaning of each name is
not defined here.

### C7043.2.9 `ordr`—Reorders

| ∆ | Bits | Name     | Description                              |
|---|------|----------|------------------------------------------|
| 0 |  32  | ident    | `ordr`                                   |
| 4 |  32  | size     | int: Length of section                   |
| 8 |  32  | count    | int: Number of reorders                  |
|12 |  32  | reserved | padding                                  |

For each reorder item:

| ∆ | Bits | Name     | Description                                              |
|---|------|----------|----------------------------------------------------------|
|16+|  32  | elements | elem: string of elements, index into `elem` section      |
|20+|  32  | before   | elem: look-behind required match, index into `elem`      |

- `elements`: index into the `elem` section, coding `from`, `order`, `tertiary`,
  `tertiary_base`, and `prebase` properties.
- `before`: follows `transform/@before`

### C7043.2.10 `strs`—Strings

All strings are stored in the Strings section.

| ∆ | Bits | Name          | Description                         |
|---|------|---------------|-------------------------------------|
| 0 |  32  | ident         | `strs`                              |
| 4 |  32  | size          | int: Length of section              |
| 8 |  32  | count         | int: Number of strings              |
|12 |  32  | reserved      | Padding                             |

Then for each string:

| ∆ | Bits | Name          | Description                                   |
|---|------|---------------|-----------------------------------------------|
|16+|  32  | offset        | off: Offset to string                         |
|20+|  32  | length        | int: Length of string in UTF-16LE code units  |

After the string offset table comes the actual UTF-16LE data. There is a null
(\u0000) after each string, which is _not_ included in the string length.

The string offset table, and then strings themselves, are sorted according to a
binary codepoint sort, not including the null.

The first string in the string table MUST always be the zero-length string. A
zero-length string is considered the same as a null string. For metadata fields,
references to this zero-length string will have the dword index value 0, which
can safely be interpreted as "not set". There may be other locations which have
required strings, but for which a zero-length string is permissible, and this
index value of 0 can be used for that purpose.

A distinction between zero-length string and optional should be avoided (e.g.
the difference between "" and null in Javascript). If this is truly required, a
separate flag field must be used to denote the difference.

### C7043.2.11 `tran`, `finl`, and `bksp`—Transforms

All three of these tables have the same format. and differ only in their
identity. The simple transform table has the ident `tran`; the final transform
table has the ident `finl`, and the backspaces table has the ident `bksp`.


| ∆ | Bits | Name     | Description                              |
|---|------|----------|------------------------------------------|
| 0 |  32  | ident    | `tran` / `finl` / `bksp`                 |
| 4 |  32  | size     | int: Length of section                   |
| 8 |  32  | count    | int: Number of transforms                |
|12 |  32  | reserved | padding                                  |


The transforms are sorted in binary order based on the `from` field.

For each transform:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|16+|  32  | from    | elem: combination of characters          |
|20+|  32  | to      | str: output text                         |
|24+|  32  | before  | elem: look-behind text (0 = omitted)     |
|28+|  32  | flags   | int: per-transform flags                 |

- `from`: the source text, index into `elem` section.
- `to`: sequence of Unicode codepoints that replace `from`. May be the null
  string for `bksp` entries.
- `before`: optional look-behind text occurring before `from`, index into `elem`
  section
- `flags`: a 32-bit bitfield defined as below:

  | Bit position | Meaning  |  Description         |
  |--------------|----------|----------------------|
  |       0      | error    | 1: `error="fail"`    |

### C7043.2.12 `vkey`—VKey Map

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0 |  32  | ident   | `vkey`                                   |
| 4 |  32  | size    | int: Length of section                   |
| 8 |  32  | count   | int: Number of vkeys                     |
|12 |  32  | reserved| padding                                  |

The keys are sorted in binary order based on the `vkey` field.

For each key:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|16+|  32  | vkey    | int: source vkey ID (0…255)              |
|20+|  32  | target  | int: target vkey ID (0…255)              |

- `vkey`: Is the standard vkey, 0-255
- `target`: Is the target (resolved) vkey, 0-255.

### C7043.2.13 `layr`—Layers list

Represents layers on the keyboard.
- Each key entry corresponds to a key on the row

| ∆ | Bits | Name       | Description                              |
|---|------|------------|------------------------------------------|
| 0 |  32  | ident      | `layr`                                   |
| 4 |  32  | size       | int: Length of section                   |
| 8 |  32  | listCount  | int: Number of layer lists               |
|12 |  32  | layerCount | int: number of layer entries             |
|16 |  32  | rowCount   | int: number of row entries               |
|20 |  32  | keyCount   | int: number of key entries               |
|24 |  64  | reserved   | padding                                  |

Each layer list corresponds to one `<layers>` element.
There are `listCount` total lists.

| ∆ | Bits | Name       | Description                                |
|---|------|------------|--------------------------------------------|
| 0+|  32  | flags      | int: per-layers options                    |
| 4+|  32  | hardware   | str: layout (`us`,`iso`,`jis`,`abnt2`)     |
| 8+|  32  | layer      | int: index to first layer element          |
|12+|  32  | count      | int: number of layer elements in this list |

- `flags`: a 32-bit bitfield defined as below:

  | Bit position | Meaning  |  Description         |
  |--------------|----------|----------------------|
  |       0      | form     | 0: hardware          |
  |       0      | form     | 1: touch             |

Each layer entry corresponds to one `<layer>` element
There are `layerCount` total layer entries.

| ∆ | Bits | Name       | Description                              |
|---|------|------------|------------------------------------------|
| 0+|  32  | id         | str: layer id such as `base` or `shift`  |
| 4+|  32  | modifier   | str: modifier string                     |
| 8+|  32  | row        | int: index into rows area (next section) |
|12+|  32  | count      | int: number of `rows` elements for this layer  |

Each row entry corresponds to one `<row>` element
There are `rowCount` total row entries.

| ∆ | Bits | Name       | Description                            |
|---|------|------------|----------------------------------------|
| 0+|  32  | key        | int: index into key element            |
| 4+|  32  | count      | int: count of key elements in this row |

Each key entry corresponds to a key in the row.
There are `keyCount` total key entries.

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0+|  32  | vkey    | int: vkey ID                             |

### C7043.2.14 `disp`—Display list

| ∆ | Bits | Name          | Description                              |
|---|------|---------------|------------------------------------------|
| 0 |  32  | ident         | `disp`                                   |
| 4 |  32  | size          | int: Length of section                   |
| 8 |  32  | count         | int: Total number of disp elements       |
|12 |  32  | baseCharacter | str: If non-null, default base.          |
|16 | 128  | reserved      | padding (future displayOptions)          |

The default `baseCharacter` is U+25CC, if `baseCharacter` is null.

For each element:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|32+|  32  | to      | str: to string                           |
|36+|  32  | display | str: output display string               |

Entries are sorted in a binary codepoint sort on the `to` field.

## TODO-LDML: various things that need to be completed here or fixed in-spec

> * UnicodeSets
> * spec: reference to `after` in reorders; various other @after refs
> * spec: ABNT2 key has hex value 0xC1 (even if kbdus.dll doesn't produce that)
> * `keys.key.mod`: TODO define this.  0 for no modifiers.
