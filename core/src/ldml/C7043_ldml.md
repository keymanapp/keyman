# KMX Plus Binary Format #7043

- copied from <https://github.com/keymanapp/keyman/issues/7043>

- Authors: MD SL

## C7043.0 Introduction

This document discusses the binary format for KMXPlus, which contains keyboards converted from source LDML data.

Draft spec PR: <https://github.com/unicode-org/cldr/pull/1847>

## C7043.1 Principles

- The data described here is located at byte offset `dpKMXPlus`.
- All integer values are unsigned 32-bit little-endian unless otherwise specified.
- All strings are UTF-16LE unless otherwise specified. (See the `strs` section.) String data items are identified with `str:`, indicating a 32 bit index into the `strs` table.
- All offsets are 32-bit little-endian values.  For all sections except for the `'sect'` section (which see), offsets are relative to the beginning of each section.

## C7043.2 Sections

- Data is divided into several sections. The very first section is the `'sect'` section which is the table of contents.
- All sections, including the first, begin on a 128-bit boundary with zero padding as needed.
- All sections begin with a 32-bit (four character) section identifier, and a 32-bit section size.
- Other than the `sect` table itself, the rest of the sections follow in binary order in the file.  In other words, the binary ordering of the section identifiers determines the order of the file layout.

### C7043.2.1 `sect`—Section Table of contents

The very first section is a table of contents listing the rest of the sections.  The table of contents does not list itself.

This is the only section where all byte offsets are relative to the value of `dpKMXPlus`.

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

### C7043.2.2 `strs`—Strings

All strings are stored in the Strings section.

| ∆ | Bits | Name          | Description                         |
|---|------|---------------|-------------------------------------|
| 0 |  32  | ident         | `strs`                              |
| 4 |  32  | size          | int: Length of section              |
| 8 |  32  | count         | int: Number of strings              |
|12 |  32  | reserved         | Padding              |

Then for each string:

| ∆ | Bits | Name          | Description                                   |
|---|------|---------------|-----------------------------------------------|
|16+|  32  | offset        | off: Offset to string                         |
|20+|  32  | length        | int: Length of string in UTF-16LE code units  |

After the string offset table comes the actual UTF-16LE data. There is a null (\u0000) after each string, which is _not_ included in the string length.

The string offset table, and then strings themselves, are sorted according to a binary codepoint sort, not including the null.

### C7043.2.3 `meta`—Metadata

| ∆ | Bits | Name          | Description                         |
|---|------|---------------|-------------------------------------|
| 0 |  32  | ident         | `meta`                              |
| 4 |  32  | size          | int: Length of section              |
| 8 |  32  | name          | str: Keyboard name                  |
|12 |  32  | author        | str: Keyboard author                |
|16 |  32  | conform       | str: CLDR 'conformsTo' version      |
|20 |  32  | layout        | str: layout type                    |
|24 |  32  | normalization | str: normalization mode             |
|28 |  32  | indicator     | str: indicator                      |
|32 |  32  | settings      | int: keyboard settings              |

The `settings` is a 32-bit bitfield as below:

| Bit position | Meaning  |  Description                                |
|--------------|----------|---------------------------------------------|
|       0      | fallback | fallback=omit                               |
|       1      | transformFailure | transformFailure=omit                               |
|       2      | transformPartial | transformPartial=hide                               |


### C7043.2.4 `loca`—Locales


| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0 |  32  | ident   | `loca`                                   |
| 4 |  32  | size    | int: Length of section                   |
| 8 |  32  | count   | int: Number of locales                   |

`count` is always ≥1, because a keyboard always has a primary locale identifier.

For each locale ID in `count`

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|12+|  32  | locale  | str: Locale ID in BCP47 format           |

The first locale ID is always the primary locale identifier.  The rest of the locale IDs (starting at offset 16) are in sorted binary order.

### C7043.2.5 `keys`—Keybag

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0 |  32  | ident   | `keys`                                   |
| 4 |  32  | size    | int: Length of section                   |
| 8 |  32  | count   | int: Number of keys                      |
|12 |  32  | reserved       | reserved                      |

The keys are sorted in binary order based on the `vkey` and `mod` fields.

For each key:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|16+|  32  | vkey    | int: vkey ID                             |
|20+|  32  | mod     | int: modifier key flags                  |
|24+|  32  | to      | str: output string OR UTF-32LE codepoint |
|28+|  32  | flags   | int: per-key flags                       |

- `vkey`: If this is 0-255, it is the resolved standard/predefined vkey (K_A, etc.). It is resolved because the `vkeyMap` from LDML has already been applied.  If this is 256 or above, it is a custom touch layout vkey generated by the compiler.
- `mod`: TODO define this.  0 for no modifiers.
- `flags`: Flags is a 32-bit bitfield defined as below:

| Bit position | Meaning  |  Description                                |
|--------------|----------|---------------------------------------------|
|       0      | extend   | 0: `to` is a char, 1: `to` is a string      |

- `to`: If `extend` is 0, `to` is a UTF-32LE codepoint. If `extend` is 1, `to` is a 32 bit index into the `strs` table.

### C7043.2.6 `vkey`—VKey Map

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0 |  32  | ident   | `vkey`                                   |
| 4 |  32  | size    | int: Length of section                   |
| 8 |  32  | count   | int: Number of vkeys                      |

The keys are sorted in binary order based on the `vkey` field.

For each key:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|12+|  32  | vkey    | int: source vkey ID (0…255)              |
|16+|  32  | target  | int: target vkey ID (0…255)              |

- `vkey`: Is the standard vkey, 0-255
- `target`: Is the target (resolved) vkey, 0-255.

### C7043.2.7 Transforms and friends

> TODO: transforms
