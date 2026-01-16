---
title: Mapping from .kvk to LDML
---

Note that counts and headers will need to merge both .kvks and .keyman-touch-layout.

# .kvks notes

```
visualkeyboard
  header
    version: ignore
    kbdname: ignore
    flags
      key102: determines lay2.forms.hardware `us` or `iso`, and the number of keys in row 4 of hardware
      displayunderlying: lay2.forms.flags.showBaseLayout
      usealtgr: lay2.forms.flags.chiralSeparate
      useunderlying: ignore (unused)
    layout: ignore
  encoding
    @name: only recognize 'unicode' encoding, ignore 'ansi'
    fontname, fontsize: meta.fontFaceName, meta.fontFaceSize
    layer
      @shift: see layr.layers discussion
      key
        @vkey: map to string
```

# LDML table notes

## `key2`

### C7043.2.15 `key2`—Extended keybag

| ∆ | Bits | Name        | Description                              | Content
|---|------|-------------|------------------------------------------|-----------------------
| 0 |  32  | ident       | `key2`                                   | `key2`
| 4 |  32  | size        | int: Length of section                   | size in bytes
| 8 |  32  | keyCount    | int: Number of keys                      | # keys from .kvks
|12 |  32  | flicksCount | int: Number of flick lists               | `0`
|16 |  32  | flickCount  | int: Number of flick elements            | `0`
|20 |  32  | kmapCount   | int: Number of kmap elements             | `0`?
|24 | var  | keys        | keys sub-table                           |
| - | var  | flicks      | flick lists sub-table                    |
| - | var  | flick       | flick elements sub-table                 |
| - | var  | kmap        | key map sub-table                        |

#### `key2.keys` subtable

For each `key` element in .kvks:

| ∆ | Bits | Name             | Description                                              | Content
|---|------|----------------  |----------------------------------------------------------|-----------------------
| 0+|  32  | to               | str: output string OR UTF-32LE codepoint                 | content of the `key` element
| 4+|  32  | flags            | int: per-key flags                                       | (optimization only)
| 8+|  32  | id               | str: key id                                              | key vk name + modifier
|12+|  32  | switch           | str: layer id to switch to                               | `0`
|16+|  32  | width            | int: key width*10 (supports 0.1 as min width)            | `1`
|20+|  32  | longPress        | list: index into `list` with longPress key id list or 0  | `0`
|24+|  32  | longPressDefault | str: default longpress key id or 0                       | `0`
|28+|  32  | multiTap         | list: index into `list` sect multiTap key id list or 0   | `0`
|32+|  32  | flicks           | int: index into `key2.flicks` subtable                   | `0`

Graphical keys should have a corresponding element added to `dis2`.

### `layr.layers`

Each layer corresponds to a .kvks `visualkeyboard/encoding[@name=unicode]/layer` element.

The `layer[@shift]` mapping is from [`VisualKeyboardLegalShiftStates[].name`](https://github.com/keymanapp/keyman/blob/7ac6bfc189333c5758fb14ef1cc0c810e1460b59/common/web/types/src/kvk/visual-keyboard.ts#L63)
to keys.key.mod and `K_MODIFIERFLAG` bitmask (this is not the same as the KVK bitmasks,
which shall be deprecated)

The compiler starts by filling out a blank representation of each modifier layer, then
iterates through the .kvk `visualkeyboard/encoding/layer/key`, keying off the attribute `vkey`.

| ∆ | Bits | Name       | Description                                    | Content
|---|------|------------|------------------------------------------------|-----------------------
| 0+|  32  | id         | str: layer id such as `base` or `shift`        | <-- modifier name
| 4+|  32  | mod        | int: modifier key flags                        | <-- `K_MODIFIERFLAG` bitmask
| 8+|  32  | row        | int: index into rows area (next section)       | index to rows
|12+|  32  | count      | int: number of `rows` elements for this layer  | # rows

### `layr.rows`

| ∆ | Bits | Name       | Description                            | Content
|---|------|------------|----------------------------------------|-----------------------
| 0+|  32  | key        | int: index into key element            | index to keys
| 4+|  32  | count      | int: count of key elements in this row | # keys

Each row should have the full set of 'white' keys, even if they are not all used. Note that `us`
and `iso` keyboards have different row counts for rows 2-4.

### `layr.keys`

| ∆ | Bits | Name    | Description                              | Content
|---|------|---------|------------------------------------------|-----------------------
| 0+|  32  | key     | str: key id                              | key id

? this implies that key.id is required, though key2.keys subtable says it is optional.
