# KMX Plus Binary Format #7043

- copied from <https://github.com/keymanapp/keyman/issues/7043>

- Authors: MD SRL

## C7043.0 Introduction

This document discusses the binary format for KMXPlus, which contains keyboards
converted from source LDML data.

Draft spec PR: <https://github.com/unicode-org/cldr/pull/1847>

## C7043.1 Principles

- When embedded in a .kmx file, the data described here is located at byte
  offset `COMP_KEYBOARD_KMXPLUS.dpKMXPlus`. See
  [KMX file format](kmx-file-format.md) for a full description of the .kmx
  format.
- All integer values are unsigned 32-bit little-endian unless otherwise
  specified.
- All strings are UTF-16LE unless otherwise specified. (See the `strs` section.)
  String data items are identified with `str:`, indicating a 32 bit index into
  the `strs` table.
- All offsets are 32-bit little-endian values.  For all sections except for the
  `'sect'` section (which see), offsets are relative to the beginning of each
  section and must fall within the `size` of the section.

## C7043.2 Sections

- Data is divided into several sections. The very first section is the `'sect'`
  section which is the table of contents.
- All sections begin with a 32-bit (four character) section identifier, and a
  32-bit section size.
- Other than the `sect` table itself, the rest of the sections follow in binary
  order in the file.  In other words, the binary ordering of the section
  identifiers determines the order of the file layout.
- All sections other than the `sect` table are optional

### C7043.2.1 `sect`—Section Table of contents

The very first section is a table of contents listing the rest of the sections.
The table of contents does not list itself.

Unlike all other sections, all byte offsets are relative to start of the `sect`
section and do not have to be located within the `size` of the section.

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

See [C7043.2.11](#c7043211-tran-and-bksptransforms).

### C7043.2.3 `elem`—Transform, Variable, and Reorder element strings

| ∆ | Bits | Name     | Description                              |
|---|------|----------|------------------------------------------|
| 0 |  32  | ident    | `elem`                                   |
| 4 |  32  | size     | int: Length of section                   |
| 8 |  32  | count    | int: Number of element string entries    |

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

- `element`: either a UnicodeSet stored in a `strs` section entry, a UTF-32LE
  codepoint, or a `uset` section entry. see also `type` flag.
- `flags`: a 32-bit bitfield defined as below:

  | Bit position | Meaning       | Description                               |
  |--------------|---------------|-------------------------------------------|
  |      0-1     | type          | `element` is 0: UTF-32LE, 1: str, 2: uset |
  |       2      | tertiary_base | 1: tertiary_base is true                  |
  |       3      | prebase       | 1: prebase is true                        |
  |      4-15    | reserved      | reserved                                  |
  |     16-23    | order         | signed int: -128 to +127                  |
  |     24-31    | tertiary      | signed int: -128 to +127                  |

  For transforms and sets, only `flags.type` will be used. The remaining flags are
  used for reorders, `from` attribute only.

### C7043.2.4 Removed: `finl`

### C7043.2.5 Removed: old-format `keys`

_this section has been merged into the new `keys.kmap`_

### C7043.2.6 `loca`—Locales

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0 |  32  | ident   | `loca`                                   |
| 4 |  32  | size    | int: Length of section                   |
| 8 |  32  | count   | int: Number of locales                   |

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
|20 |  32  | name          | str: keyboard nme                   |
|24 |  32  | indicator     | str: indicator                      |
|28 |  32  | version       | str: keyboard version               |
|32 |  32  | settings      | int: keyboard settings              |

The `settings` is a 32-bit bitfield as below:

| Bit position | Meaning          |  Description                 |
|--------------|------------------|------------------------------|
|       0      | normalization    | normalization=disabled       |

### C7043.2.8 `name`—Names

_Removed. See `meta.name`._

### C7043.2.9 Removed: `ordr`

_Reorder entries are now a subtable of `tran`/`bksp`, see [`tran`/`bksp`](#c70432112-tranreorders-subtable)_

### C7043.2.10 `strs`—Strings

All strings are stored in the Strings section.

| ∆ | Bits | Name          | Description                         |
|---|------|---------------|-------------------------------------|
| 0 |  32  | ident         | `strs`                              |
| 4 |  32  | size          | int: Length of section              |
| 8 |  32  | count         | int: Number of strings              |

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

### C7043.2.11 `tran`, and `bksp`—Transforms

These tables represent the `<transforms>` element.

Both of these tables have the same format. and differ only in their
identity. The `simple` transform table has the ident `tran`
and the `backspace` table has the ident `bksp`.


| ∆ | Bits | Name           | Description                              |
|---|------|----------------|------------------------------------------|
| 0 |  32  | ident          | `tran` / `bksp`                          |
| 4 |  32  | size           | int: Length of section                   |
| 8 |  32  | groupCount     | int: Number of transformGroups           |
|12 |  32  | transformCount | int: Number of transforms                |
|16 |  32  | reorderCount   | int: Number of reorders                  |
|20+|  -   | groups         | transformGroup subtable                  |
| - |  -   | transforms     | transforms subtable                      |
| - |  -   | reorders       | reorders subtable                        |

The transformGroups are in file order.

Each transformGroup is either a `transform` group or a `reorder` group.

### C7043.2.11.1 `tran.groups` subtable

For each transformGroup:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0+|  32  | type    | int: type of transformgroup              |
| 4+|  32  | count   | int: number of items in this group       |
| 8+|  32  | index   | int: index into subtable                 |

- `type`: one of the following:

  - `0`: `transform` elements beginning at 'index'
  - `1`: `reorder` elements beginning at 'index'

The transforms are sorted in binary order based on the `from` field.

### C7043.2.11.1 `tran.transforms` subtable

For each transform in the subtable:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0+|  32  | from    | str: processed regex of from= side       |
| 4+|  32  | to      | str: output pattern                      |
| 8+|  32  | mapFrom | str: name of set variable for 'from' $1  |
| 8+|  32  | mapTo   | str: name of set variable for 'to'  $1   |

- `from`: the source text, index into `elem` section.
- `to`: sequence of Unicode codepoints that replace `from`. May be the null
  string for `bksp` entries.
- `mapFrom` and `mapTo` work as a pair. If present, implementation must:
  - use `from` as the regex to match against
  - search `mapFrom` for the value of captured group 1 ($1)
  - replace the entire matched `from` regex with the same indexed value in `mapTo`
  - `to` will be null in this case
  - Debugging note: The variables table can be searched for matching `elem` pointers.
  - For example, from="($[upper])" to="$[1:lower]" will result in:
    mapFrom: "upper"
    mapTo:   "lower"

### C7043.2.11.2 `tran.reorders` subtable

For each reorder item:

| ∆ | Bits | Name     | Description                                              |
|---|------|----------|----------------------------------------------------------|
| 0+|  32  | elements | elem: string of elements, index into `elem` section      |
| 4+|  32  | before   | elem: look-behind required match, index into `elem`      |

- `elements`: index into the `elem` section, coding `from`, `order`, `tertiary`,
  `tertiaryBase`, and `preBase` properties.
- `before`: follows `reorder/@before`

### C7043.2.12 `vkey`—VKey Map

_Removed._

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
|24 | var  | lists      | layer list sub-table                     |
| - | var  | layers     | layers sub-table                         |
| - | var  | rows       | rows sub-table                           |
| - | var  | keys       | keys sub-table                           |

### `layr.lists` subtable

Each layer list corresponds to one `<layers>` element.
There are `listCount` total lists.

| ∆ | Bits | Name             | Description                                |
|---|------|------------------|--------------------------------------------|
| 0+|  32  | hardware         | str: name of hardware layout.              |
| 4+|  32  | layer            | int: index to first layer element          |
| 8+|  32  | count            | int: number of layer elements in this list |
|12+|  32  | minDeviceWidth   | int: min device width in millimeters, or 0 |

- `hardware` is the name of a form, or the string `touch`

See UTS #35 section 7 for details about these values.

Layer lists are sorted by `hardware` string, then minDeviceWidth ascending.

### `layr.layers` subtable

Each layer entry corresponds to one `<layer>` element
There are `layerCount` total layer entries.
Note that comma-separated modifiers in the XML will result in duplicate `layers` entries.
For example, `id="abc" modifiers="none, shift caps"`  will result in two `layers` elements,
both with `id="abc"`, but one with key flags of 0x0000 and one with keyflags of 0x0110.

| ∆ | Bits | Name       | Description                                    |
|---|------|------------|------------------------------------------------|
| 0+|  32  | id         | str: layer id such as `base` or `shift`        |
| 4+|  32  | mod        | int: modifier key flags                        |
| 8+|  32  | row        | int: index into rows area (next section)       |
|12+|  32  | count      | int: number of `rows` elements for this layer  |

- for `mod` see `keys.key.mod`

### `layr.rows` subtable

Each row entry corresponds to one `<row>` element
There are `rowCount` total row entries.

| ∆ | Bits | Name       | Description                            |
|---|------|------------|----------------------------------------|
| 0+|  32  | key        | int: index into key element            |
| 4+|  32  | count      | int: count of key elements in this row |

### `layr.keys` subtable

Each key entry corresponds to a key in the row.
There are `keyCount` total key entries.

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0+|  32  | key     | str: key id                              |

### C7043.2.14 `disp`—Display list

| ∆ | Bits | Name          | Description                              |
|---|------|---------------|------------------------------------------|
| 0 |  32  | ident         | `disp`                                   |
| 4 |  32  | size          | int: Length of section                   |
| 8 |  32  | count         | int: Total number of disp elements       |
|12 |  32  | baseCharacter | str: If non-null, default base.          |

The default `baseCharacter` is U+25CC, if `baseCharacter` is null.

For each element:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|32+|  32  | to      | str: to string                           |
|36+|  32  | id      | str: id string                           |
|40+|  32  | display | str: output display string               |

Either `to` or `id` must be set, not both.
Entries with an `to` field are sorted in a binary codepoint sort on the `to` field,
followed by entries with an `id` field set sorted in a binary codepoint sort on the `id` field.


### C7043.2.15 `key2`—Extended keybag

| ∆ | Bits | Name        | Description                              |
|---|------|-------------|------------------------------------------|
| 0 |  32  | ident       | `key2`                                   |
| 4 |  32  | size        | int: Length of section                   |
| 8 |  32  | keyCount    | int: Number of keys                      |
|12 |  32  | flicksCount | int: Number of flick lists               |
|16 |  32  | flickCount  | int: Number of flick elements            |
|20 |  32  | kmapCount   | int: Number of kmap elements             |
|24 | var  | keys        | keys sub-table                           |
| - | var  | flicks      | flick lists sub-table                    |
| - | var  | flick       | flick elements sub-table                 |
| - | var  | kmap        | key map sub-table                        |

#### `key2.keys` subtable

For each key:

| ∆ | Bits | Name             | Description                                              |
|---|------|----------------  |----------------------------------------------------------|
| 0+|  32  | to               | str: output string OR UTF-32LE codepoint                 |
| 4+|  32  | flags            | int: per-key flags                                       |
| 8+|  32  | id               | str: key id                                              |
|12+|  32  | switch           | str: layer id to switch to                               |
|16+|  32  | width            | int: key width*10 (supports 0.1 as min width)            |
|20+|  32  | longPress        | list: index into `list` section with longPress key id list or 0 |
|24+|  32  | longPressDefault | str: default longpress key id or 0                           |
|28+|  32  | multiTap         | list: index into `list` section with multiTap key id list or 0  |
|32+|  32  | flicks           | int: index into `key2.flicks` subtable                   |

- `id`: The original string id from XML. This may be 0 to save space (i.e. omit the string id).
- `flags`: Flags is a 32-bit bitfield defined as below:

| Bit position | Meaning   |  Description                                |
|--------------|-----------|---------------------------------------------|
|       0      | extend    | 0: `to` is a char, 1: `to` is a string      |
|       1      | gap       | 1 if the key is a gap                       |

- `to`: If `extend` is 0, `to` is a UTF-32LE codepoint. If `extend` is 1, `to`
  is a 32 bit index into the `strs` table. The string may be zero-length.
- `longPress`, `longPressDefault`, and `multiTap` refer to key ids or lists of key ids in this same `key2.keys` subtable.

#### `key2.flicks` flick list subtable

For each flicks in the flick list:

| ∆ | Bits | Name             | Description                                              |
|---|------|----------------  |----------------------------------------------------------|
| 0+|  32  | count            | int: number of flick elements in this flick              |
|12+|  32  | flick            | int: index into `flick` subtable for first flick element |
|16+|  32  | id               | str: flick id                                            |

- `id`: The original string id from XML. This may be 0 to save space (i.e. omit the string id).

Elements are ordered by the string id.

If this section is present, it must have a 'flicks' in the list at position zero with count=0, index=0 and id=0 meaning 'no flicks'.
#### `key2.flick` flick element subtable

For each flick element:

| ∆ | Bits | Name             | Description                                              |
|---|------|----------------  |----------------------------------------------------------|
| 0+|  32  | directions       | list: index into `list` section with direction list      |
| 8+|  32  | keyId            | str: id of key                                           |

If this section is present, it must have a 'flick element' at position zero with directions=0, flags=0, and to=0 meaning 'no flick'.

There is not a 'null' flick element at the end of each list.

Elements are ordered by the `flicks.keyId`, and secondarily by the directions list id.


#### `key2.kmap` key map subtable

This table (formerly the `keys` section)
The keys are sorted in ascending order based on the `vkey`, `mod` fields.

For each key:

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
|16+|  32  | vkey    | int: vkey ID                             |
|20+|  32  | mod     | int: modifier key flags                  |
|24+|  32  | key     | int: index into `key` sibling subtable   |

- `vkey`: If this is 0-255, it is the resolved standard/predefined vkey (K_A,
  etc.). It is resolved because the `vkeyMap` from LDML has already been
  applied.  If this is 256 or above, it is a custom touch layout vkey generated
  by the compiler.
- `mod`: 32-bit bitfield defined as below. Little endian values.

|  Value   | Meaning   |`kmx_file.h`        | Comment                                       |
|----------|-----------|--------------------|-----------------------------------------------|
|  0x0000  | `none`    |                    | All zeros = no modifiers                      |
|  0x0001  | `ctrlL`   | `LCTRLFLAG`        | Left Control                                  |
|  0x0002  | `ctrlR`   | `RCTRLFLAG`        | Right Control                                 |
|  0x0004  | `altL`    | `LALTFLAG`         | Left Alt                                      |
|  0x0008  | `altR`    | `RALTFLAG`         | Right Alt                                     |
|  0x0010  | `shift`   | `K_SHIFTFLAG`      | Either Shift                                  |
|  0x0020  | `ctrl`    | `K_CTRLFLAG`       | Either Control                                |
|  0x0040  | `alt`     | `K_ALTFLAG`        | Either Alt                                    |
|  0x0100  | `caps`    | `CAPITALFLAG`      | Caps lock                                     |
|  0x10000 | `other` | n/a                  | Other (not used in conjunction with others)   |

TODO-LDML: Note that conforming to other keyman values, left versus right shift
cannot be distinguished.

### C7043.2.16 `list`—String lists

| ∆ | Bits | Name          | Description                              |
|---|------|---------------|------------------------------------------|
| 0 |  32  | ident         | `list`                                   |
| 4 |  32  | size          | int: Length of section                   |
| 8 |  32  | listCount     | int: Total number of lists elements      |
|12 |  32  | indexCount    | int: Total number of index elements      |
|16 | var  | lists         | list sub-table                           |
| - | var  | indices       | index sub-table                          |

#### `list.lists` sub-table

The lists entry at location 0 is defined to have index=0 and count=0,
representing a 0-length list.

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0+|  32  | index   | int: Index into 'indices' subtable       |
| 4+|  32  | count   | int: Number of indices in this list      |

This data should be sorted in binary lexical order of the substrings.

#### `list.indices` sub-table

The index entry at location 0 is defined to have str=0,
representing a 0-length string.

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0+|  32  | str     | str: string index into `strs` table      |

These indices are a pool of indexes into the string table.
The strings order are significant.  There is not a 'null' string at the end of each list.

### C7043.2.17 `vars`—Variables

| ∆ | Bits | Name          | Description                              |
|---|------|---------------|------------------------------------------|
| 0 |  32  | ident         | `vars`                                   |
| 4 |  32  | size          | int: Length of section                   |
| 8 |  32  | markers       | list: Index to marker list, or 0         |
|12 |  32  | varCount      | int: Total number of variable elements   |
| - | var  | varEntries    | variables sub-table                      |

#### C7043.2.17.1 `vars.varEntries` subtable

For each variable,

| ∆ | Bits | Name    | Description                              |
|---|------|---------|------------------------------------------|
| 0+|  32  | type    | int: Type of variable                    |
| 4+|  32  | id      | str: Variable's id                       |
| 8+|  32  | value   | str: Value in string form                |
|12+|  32  | elem    | elem: Set as array                       |

- `type` is per below:
  -  0 : string
  -  1 : set
  -  2 : unicodeSet

TODO-LDML: note that at present, unicodeSet variables are not stored using the `uset` type

Items are sorted by id

### C7043.2.18 `uset` UnicodeSets table

This table contains serialized [UnicodeSet](http://www.unicode.org/reports/tr35/#Unicode_Sets)s,
together with their original patterns.  Each UnicodeSet is converted into an array of ranges.
Per the Keyboard spec, sets with multi-character strings are not allowed.

For example, `[a-c q]` would be converted into the two ranges (U+0061-U+0063) and (U+0071-U+0071),
the latter being the single codepoint `q`.

| ∆ | Bits | Name          | Description                              |
|---|------|---------------|------------------------------------------|
| 0 |  32  | ident         | `uset`                                   |
| 4 |  32  | size          | int: Length of section                   |
| 8 |  32  | usetCount     | int: Total number of range lists         |
|12 |  32  | rangeCount    | int: Total number of range elements      |
|16 | var  | usets         | uset list sub-table                      |
| - | var  | ranges        | range sub-table                          |

#### `uset.usets` sub-table

Each entry in this subtable represents a UnicodeSet, and is referenced by
index by other tables.

| ∆ | Bits | Name    | Description                         |
|---|------|---------|------------------------------------ |
| 0+|  32  | range   | int: Index into 'ranges' subtable   |
| 4+|  32  | count   | int: Number of ranges in this set   |
| 8+|  32  | pattern | str: UnicodeSet as string           |

The usets are sorted in order of their pattern string's binary
order.

#### `uset.ranges` sub-table

Each represents a UnicodeSet

| ∆ | Bits | Name    | Description                         |
|---|------|---------|------------------------------------ |
| 0+|  32  | start   | int: UTF-32 char start              |
| 4+|  32  | end     | int: UTF-32 char end                |

`start` is always <= `end`. `start` and `end` may be equal if a single codepoint
is represented.

## TODO-LDML: various things that need to be completed here or fixed in-spec
> * spec: ABNT2 key has hex value 0xC1 (even if kbdus.dll doesn't produce that)
