---
title: Mapping from .keyman-touch-layout to LDML
---

.keyman-touch-layout is referred to as .ktl for short

Note that counts and headers will need to merge both .kvks and .keyman-touch-layout.

# .keyman-touch-layout notes

```
touch-layout
  tablet|phone|desktop -> determines minDeviceWidth and hardware
    font: meta.fontFaceName
    fontsize: meta.fontSize
    displayUnderlying: lay2.forms.flags.showBaseLayout
    defaultHint: resolved at compile time to generate hints for each key
    layer[]
      id: layr.layers.id
      row[]
        id  (numeric)
        key[]
          id: key2.keys.id (along with `layer`)
          text: key2.keys.text / dis2.display.display
          layer: key2.keys.id
          nextlayer: key2.switch
          font: not supported
          fontsize: not supported
          sp: map to various 'special' keys by id; see below
          pad: adds a "gap" key2.keys
          width: map to key2.keys.width / 10
          sk[]
            id: key2.keys.id
            text: key2.keys.to
            layer: key2.keys.id
            nextlayer: key2.switch
            font: not supported
            fontsize: not supported
            sp: map to various 'special' keys by id; see below
            pad: adds a "gap" key2.keys
            width: map to key2.keys.width / 10
          flick{} - see sk
          multitap[] - see sk
          hint: add to dis2 for the given key
```

## `tablet` | `phone` | `desktop` element

The name of the element drives the selection of the corresponding form data:

  * `tablet` -> sets `minDeviceWidth` to `100` (mm), `hardware` = `touch`
  * `phone` -> sets `minDeviceWidth` to `0` (mm), `hardware` = `touch`
  * `desktop` -> ignore, as this is not really supported in Keyman at present;
    alternative: `hardware` = `us` (4 keyboards in repo that currently use this:
    karambolpoular, orma, sxava, sxava_eo)

## `id` and `layer` properties

These two properties together will form the identifier of the key. This
identifier will have semantic meaning, and will be used in event generation.

## `nextlayer` property

This can be mapped to `key2.switch`.

## `sp` property

The value of the `sp` property will drive slightly different behaviors:

  * `0` Default -> Default key type, no additional metadata
  * `1` Special -> Sets `dis2.display.flags.isFrameKey` flag
  * `2` Special (active) -> Sets `dis2.display.flags.isHighlighted` and
    `dis2.display.flags.isFrameKey` flags
  * `8`: Deadkey -> Sets `dis2.display.flags.isDeadKey` flag (styling only)
  * `9`: Blank -> Sets `dis2.display.flags.isBlankKey` flag (styling and
    interactivity)
  * `10`: Spacer -> Adds a `gap` key with the required width

# LDML table notes

## `key2`

### C7043.2.15 `key2`—Extended keybag

| ∆ | Bits | Name        | Description                              | Content
|---|------|-------------|------------------------------------------|-----------------------
| 0 |  32  | ident       | `key2`                                   | `key2`
| 4 |  32  | size        | int: Length of section                   | size in bytes
| 8 |  32  | keyCount    | int: Number of keys                      | # keys from .ktl
|12 |  32  | flicksCount | int: Number of flick lists               |
|16 |  32  | flickCount  | int: Number of flick elements            |
|20 |  32  | kmapCount   | int: Number of kmap elements             | `0`
|24 | var  | keys        | keys sub-table                           |
| - | var  | flicks      | flick lists sub-table                    |
| - | var  | flick       | flick elements sub-table                 |
| - | var  | kmap        | key map sub-table                        |

#### `key2.keys` subtable

For each `key` element in .ktl:

| ∆ | Bits | Name             | Description                                              | Content
|---|------|----------------  |----------------------------------------------------------|-----------------------
| 0+|  32  | to               | str: output string OR UTF-32LE codepoint                 | `key.text`*
| 4+|  32  | flags            | int: per-key flags                                       | bit 1 set if gap
| 8+|  32  | id               | str: key id                                              | `key.layer` + ':' + `key.id`?
|12+|  32  | switch           | str: layer id to switch to                               | `key.nextlayer`
|16+|  32  | width            | int: key width*10 (supports 0.1 as min width)            | `key.width / 10`
|20+|  32  | longPress        | list: index into `list` with longPress key id list or 0  |
|24+|  32  | longPressDefault | str: default longpress key id or 0                       |
|28+|  32  | multiTap         | list: index into `list` sect multiTap key id list or 0   |
|32+|  32  | flicks           | int: index into `key2.flicks` subtable                   |

* specials will be mapped to the corresponding `dis2`.flags.specialKeyCap value


## `layr`

| ∆ | Bits | Name       | Description                              | Content
|---|------|------------|------------------------------------------|-----------------------
| 0 |  32  | ident      | `layr`                                   | `layr`
| 4 |  32  | size       | int: Length of section                   | size in bytes
| 8 |  32  | listCount  | int: Number of layer lists               | # layer lists
|12 |  32  | layerCount | int: number of layer entries             | # layer entries
|16 |  32  | rowCount   | int: number of row entries               | # row entries
|20 |  32  | keyCount   | int: number of key entries               | # key entries
|24 | var  | lists      | layer list sub-table                     |
| - | var  | layers     | layers sub-table                         |
| - | var  | rows       | rows sub-table                           |
| - | var  | keys       | keys sub-table                           |

### `layr.lists`

| ∆ | Bits | Name             | Description                                | Content
|---|------|------------------|--------------------------------------------|-----------------------
| 0+|  32  | hardware         | str: name of hardware layout.              | `us` (default) or `iso` <- kvkh102
| 4+|  32  | layer            | int: index to first layer element          | `0`
| 8+|  32  | count            | int: number of layer elements in this list | # layer elements <- modifiers
|12+|  32  | minDeviceWidth   | int: min device width in millimeters, or 0 | `0`

### `layr.layers`

Each layer corresponds to a .kvks `visualkeyboard/encoding[@name=unicode]/layer` element.

The `layer[@shift]` mapping is from [`VisualKeyboardLegalShiftStates\[\].name`](https://github.com/keymanapp/keyman/blob/7ac6bfc189333c5758fb14ef1cc0c810e1460b59/common/web/types/src/kvk/visual-keyboard.ts#L63) to keys.key.mod
and `K_MODIFIERFLAG` bitmask (this is not the same as the KVK bitmasks, which shall be deprecated)

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
