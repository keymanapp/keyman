# KMX Binary Format

This document describes the binary format for KMX, compiled from Keyman .kmn
source files.

## Principles

- All integer values are unsigned 32-bit little-endian unless otherwise
  specified.
- All offsets are 32-bit little-endian values. All offsets are relative to the
  beginning of the data (normally start of file). A zero offset indicates that
  the value is not present, equivalent to `NULL` -- no offset can ever point to
  the start of the file.
- All strings are UTF-16LE unless otherwise specified and null terminated.
  Strings are referenced by offset (see above).
- The file format has a header section followed by data. There is no specific
  defined order for data in the file. Empty space is permitted but should be
  zeroed. Note that `COMP_KEYBOARD_KMXPLUS` must follow immediately after
  `COMP_KEYBOARD` with no empty space (that is at byte 0x40).

## `COMP_KEYBOARD`: KMX header

The very first section is a header that describes the remainder of the file.

| ∆ | Bits | Name                 | Description                                                                       |
|---|------|----------------------|-----------------------------------------------------------------------------------|
| 0 |  32  | `dwIdentifier`       | `KXTS` 0x5354584B, `kmx.FILEID_COMPILED`                                          |
| 4 |  32  | `dwFileVersion`      | minimum Keyman version that can read the file, `KMX_Version` enum                 |
| 8 |  32  | `dwCheckSum`         | deprecated in 16.0, `0x0`; CRC32 checksum of entire file                          |
|12 |  32  | `KeyboardID`         | deprecated in 10.0, `0x0`; Windows LANGID for keyboard                            |
|16 |  32  | `IsRegistered`       | deprecated in 10.0, `0x1`; registration status of compiler                        |
|20 |  32  | `version`            | deprecated, `0x0`; was version of keyboard, see `TSS_KEYBOARDVERSION`             |
|24 |  32  | `cxStoreArray`       | number of `COMP_STORE` entries in `COMP_KEYBOARD.dpStoreArray`                    |
|28 |  32  | `cxGroupArray`       | number of `COMP_GROUP` entries in `COMP_KEYBOARD.dpGroupArray`                    |
|32 |  32  | `dpStoreArray`       | offset of first `COMP_STORE` entry                                                |
|36 |  32  | `dpGroupArray`       | offset of first `COMP_GROUP` entry                                                |
|40 |  32  | `StartGroup_ANSI`    | deprecated; index of starting non-Unicode `COMP_GROUP`, `0xFFFFFFFF` means unused |
|44 |  32  | `StartGroup_Unicode` | index of starting Unicode `COMP_GROUP`, `0xFFFFFFFF` means unused                 |
|48 |  32  | `dwFlags`            | global flags for the keyboard, see description below                              |
|52 |  32  | `dwHotKey`           | default hotkey for keyboard, from [`&Hotkey`], see description below              |
|56 |  32  | `dpBitmapOffset`     | offset of keyboard icon, `0x0` if not present                                     |
|60 |  32  | `dwBitmapSize`       | size in bytes of keyboard icon, `0x0` if not present                              |

This structure is present at the start of every .kmx file. The `KXTS` identifier
can be used as a 'magic' to determine whether a binary file is a .kmx file.

### `dwFileVersion`: Minimum version of Keyman required to read the file

The `dwFileVersion` property should be checked before attempting to read any
other data in the file. If it is not a recognized version, then no further
attempt should be made to read the file. The recognized versions are listed in
the `KMX_Version` enum:

| Value        | Identifier    | Description                                                      |
|--------------|---------------|------------------------------------------------------------------|
| `0x00000300` | `VERSION_30`  | Keyman 3.0. Non-Unicode, Windows 3.1 only, not supported         |
| `0x00000301` | `VERSION_31`  | Keyman 3.1. Non-Unicode, Windows 3.1 only, not supported         |
| `0x00000302` | `VERSION_32`  | Keyman 3.2. Non-Unicode, Windows 3.1, 95, 98 only, not supported |
| `0x00000400` | `VERSION_40`  | Keyman 4.0. Non-Unicode, Windows 3.1, 95, 98 only, not supported |
| `0x00000500` | `VERSION_50`  | Keyman 5.0. Unicode, Windows only                                |
| `0x00000501` | `VERSION_501` | Keyman 5.1, added support for `call()` statement                 |
| `0x00000600` | `VERSION_60`  | Keyman 6.0                                                       |
| `0x00000700` | `VERSION_70`  | Keyman 7.0                                                       |
| `0x00000800` | `VERSION_80`  | Keyman 8.0                                                       |
| `0x00000900` | `VERSION_90`  | Keyman 9.0                                                       |
| `0x00000A00` | `VERSION_100` | Keyman 10.0, first true multi-platform version                   |
| `0x00000E00` | `VERSION_140` | Keyman 14.0 (versions 11.0-13.0 had no binary format changes)    |
| `0x00000F00` | `VERSION_150` | Keyman 15.0                                                      |
| `0x00001000` | `VERSION_160` | Keyman 16.0                                                      |
| `0x00001100` | `VERSION_170` | Keyman 17.0                                                      |

### `StartGroup`

Keyboards may have up to 4 starting groups, defined by [`begin`] statements in
the source .kmn file.. The `begin ANSI` group and `begin Unicode` groups are
defined in the `COMP_KEYBOARD` header, however as they were introduced into the
format later, the `begin NewContext` and `begin PostKeystroke` groups are
defined as system stores.

### `dwFlags`

The following flags are defined for .kmx keyboards:

| Bit value    | Name                  | Description                                                                                 |
|--------------|-----------------------|---------------------------------------------------------------------------------------------|
| `0x00000001` | `KF_SHIFTFREESCAPS`   | Pressing <kbd>Shift</kbd> releases <kbd>Caps Lock</kbd> (see [`&ShiftFreesCaps`])           |
| `0x00000002` | `KF_CAPSONONLY`       | <kbd>Caps Lock</kbd> switches on only (see [`&CapsOnOnly`])                                 |
| `0x00000004` | `KF_CAPSALWAYSOFF`    | <kbd>Caps Lock</kbd> state is disabled (see [`&CapsAlwaysOff`])                             |
| `0x00000008` | `KF_LOGICALLAYOUT`    | Unused, should never be set                                                                 |
| `0x00000010` | `KF_AUTOMATICVERSION` | The compiler determined the minimum version of Keyman automatically (see [`&Version`])      |
| `0x00000020` | `KF_KMXPLUS`          | 16.0+: A `COMP_KEYBOARD_KMXPLUSINFO` structure is present immediately after `COMP_KEYBOARD` |

### `dwHotKey`: keyboard hotkeys

This field is `0x0` if no default hotkey is specified. The value is a bitmask:

| Bit mask     | Name        | Description                              |
|--------------|-------------|------------------------------------------|
| `0x000000FF` | virtual key | [Keyman virtual key] value of the hotkey |
| `0x00010000` | `HK_ALT`    | Set if the hotkey uses <kbd>Alt</kbd>    |
| `0x00020000` | `HK_CTRL`   | Set if the hotkey uses <kbd>Ctrl</kbd>   |
| `0x00040000` | `HK_SHIFT`  | Set if the hotkey uses <kbd>Shift</kbd>  |

### Keyboard icon

The `COMP_KEYBOARD.dpBitmapOffset` and `COMP_KEYBOARD.dwBitmapSize` members
describe the keyboard icon, if present. If not present, both members should have
the value `0x0`. The keyboard icon may be a 16x16 Windows .bmp format file, 4,
8, 24, or 32 bit color, or a Windows .ico format file, which may have multiple
icons present at different bit depths and resolutions. Refer to the Windows API
documentation for .bmp and .ico binary formats.

By convention, the compiler stores the bitmap image at the end of the KMX
data, but this is not a requirement of the file format.

## `COMP_KEYBOARD_KMXPLUSINFO`: KMX+ secondary header

| ∆ | Bits | Name            | Description                |
|---|------|-----------------|----------------------------|
| 0 |  32  | `dpKMXPlus`     | offset of KMX+ data        |
| 4 |  32  | `dwKMXPlusSize` | size in bytes of KMX+ data |

The `COMP_KEYBOARD_KMXPLUSINFO` structure is present only if
`COMP_KEYBOARD.dwFlags` flag has bit `KF_KMXPLUS` (`0x20`) set. If present, it
must be located immediately after the `COMP_KEYBOARD` structure, that is, at
byte offset `0x40`.

See [KMX+ File Format] for details of the KMX+ file format.

## `COMP_STORE`

| ∆ | Bits | Name         | Description                                                            |
|---|------|--------------|------------------------------------------------------------------------|
| 0 |  32  | `dwSystemID` | `0x0` for a normal store, or a system store value                      |
| 4 |  32  | `dpName`     | offset to name of the store from keyboard source, `0x0` if not present |
| 8 |  32  | `dpString`   | offset to value of the store                                           |

The `COMP_STORE` structure describes a [`store`] in the keyboard source file.
`COMP_STORE` structures are also used for additional metadata such as compiler
version.

Store names are compiled into keyboards if debug information is included (see
[Debug information for keyboards](#debug-information-for-keyboards)), or if the store is used for [keyboard
options]. If the store name is not included, `dpName` will have a `0x0` value.

### System Stores

If `dwSystemID` is non-zero, then it will be one of the following values:

| Value | Identifier                      | Count | Version | Description                                                                              |
|-------|---------------------------------|-------|---------|------------------------------------------------------------------------------------------|
| 0     | `TSS_NONE`                      | 0+    | 5.0     | (Not a system store, normal keyboard store data)                                         |
| 1     | `TSS_BITMAP`                    | 0-1   | 5.0     | [`&Bitmap`] store, used during compile only; see `COMP_KEYBOARD.dpBitmapOffset`          |
| 2     | `TSS_COPYRIGHT`                 | 0-1   | 5.0     | [`&Copyright`] store                                                                     |
| 3     | `TSS_HOTKEY`                    | 0-1   | 5.0     | [`&Hotkey`] store (see also `COMP_KEYBOARD.dwHotkey`)                                    |
| 4     | `TSS_LANGUAGE`                  | 0-1   | 5.0     | Deprecated, [`&Language`] store                                                          |
| 5     | `TSS_LAYOUT`                    | 0-1   | 5.0     | Deprecated, [`LAYOUT`] statement                                                         |
| 6     | `TSS_MESSAGE`                   | 0-1   | 5.0     | [`&Message`] store                                                                       |
| 7     | `TSS_NAME`                      | 0-1   | 5.0     | [`&Name`] store, public name of keyboard                                                 |
| 8     | `TSS_VERSION`                   | 0-1   | 5.0     | [`&Version`] store; use `COMP_KEYBOARD.dwFileVersion`                                    |
| 9     | `TSS_CAPSONONLY`                | 0-1   | 5.0     | [`&CapsOnOnly`] store; use `TF_CAPSONONLY`                                               |
| 10    | `TSS_CAPSALWAYSOFF`             | 0-1   | 5.0     | [`&CapsAlwaysOff`] store; use `TF_CAPSALWAYSOFF`                                         |
| 11    | `TSS_SHIFTFREESCAPS`            | 0-1   | 5.0     | [`&ShiftFreesCaps`] store; use `TF_SHIFTFREESCAPS`                                       |
| 12    | `TSS_LANGUAGENAME`              | 0-1   | 5.0     | Deprecated, `LANGUAGENAME` statement                                                     |
| 13    | `TSS_CALLDEFINITION`            | 0+    | 5.0     | Definition for an IMX call                                                               |
| 14    | `TSS_CALLDEFINITION_LOADFAILED` | 0     | 5.0     | Deprecated, unused, was used internally in KMX processor                                 |
| 15    | `TSS_ETHNOLOGUECODE`            | 0-1   | 5.0     | Deprecated, [`&EthnologueCode`] store                                                    |
| 16    | `TSS_DEBUG_LINE`                | 0+    | 5.0     | [Debug information for keyboards](#debug-information-for-keyboards)                      |
| 17    | `TSS_MNEMONIC`                  | 0-1   | 5.0     | [`&MnemonicLayout`] store, `'0'` or `'1'`                                                |
| 18    | `TSS_INCLUDECODES`              | 0-1   | 5.0     | [`&IncludeCodes`] store, used during compile only                                        |
| 19    | `TSS_OLDCHARPOSMATCHING`        | 0-1   | 5.0     | Deprecated, [`&OldCharPosMatching`] store                                                |
| 20    | `TSS_COMPILEDVERSION`           | 0-1   | 5.0     | Version of compiler used, may be omitted if `--no-compiler-version` passed to kmc        |
| 21    | `TSS_KEYMANCOPYRIGHT`           | 0-1   | 5.0     | Copyright message from compiler, may be omitted if `--no-compiler-version` passed to kmc |
| 22    | `TSS_CUSTOMKEYMANEDITION`       | 1     | 5.0     | Deprecated, always `'0'`, always present                                                 |
| 23    | `TSS_CUSTOMKEYMANEDITIONNAME`   | 1     | 5.0     | Deprecated, always `'Keyman'`, always present                                            |
| 24    | `TSS_VISUALKEYBOARD`            | 0-1   | 7.0     | [`&VisualKeyboard`] store                                                                |
| 25    | `TSS_KMW_RTL`                   | 0-1   | 7.0     | [`&KMW_RTL`] store, `'0'` or `'1'`                                                       |
| 26    | `TSS_KMW_HELPFILE`              | 0-1   | 7.0     | [`&KMW_HelpFile`] store, used during compile only                                        |
| 27    | `TSS_KMW_HELPTEXT`              | 0-1   | 7.0     | [`&KMW_HelpText`] store, used during compile only                                        |
| 28    | `TSS_KMW_EMBEDJS`               | 0-1   | 7.0     | [`&KMW_EmbedJS`] store, used during compile only                                         |
| 29    | `TSS_WINDOWSLANGUAGES`          | 0-1   | 7.0     | Deprecated, [`&WindowsLanguages`] store                                                  |
| 30    | `TSS_COMPARISON`                | 0+    | 8.0     | Generated by compiler for use in `if()` statements                                       |
| 31    | `TSS_PLATFORM`                  | 0-1   | 9.0     | Cannot be used in `COMP_STORE`; used for [`if(&platform)`]. [`platform()`] only          |
| 32    | `TSS_BASELAYOUT`                | 0-1   | 9.0     | Cannot be used in `COMP_STORE`; used for [`if(&baselayout)`], [`baselayout()`] only      |
| 33    | `TSS_LAYER`                     | 0     | 9.0     | Cannot be used in `COMP_STORE`; used for [`if(&layer)`] and [`set(&layer)`]              |
| 34    | `TSS_VKDICTIONARY`              | 0-1   | 9.0     | Dictionary of `T_` and `U_` virtual key names for touch layouts, generated by compiler   |
| 35    | `TSS_LAYOUTFILE`                | 0-1   | 9.0     | Name of touch layout file, used during compile only                                      |
| 36    | `TSS_KEYBOARDVERSION`           | 0-1   | 9.0     | [`&KeyboardVersion`] store                                                               |
| 37    | `TSS_KMW_EMBEDCSS`              | 0-1   | 9.0     | [`&KMW_EmbedCSS`] store, used during compile only                                        |
| 38    | `TSS_TARGETS`                   | 0-1   | 9.0     | [`&Targets`] store                                                                       |
| 39    | `TSS_CASEDKEYS`                 | 0-1   | 14.0    | [`&CasedKeys`] store, used during compile only                                           |
| 40    | `TSS_BEGIN_NEWCONTEXT`          | 0-1   | 15.0    | zero-based index of group referenced in [`begin NewContext`]                             |
| 41    | `TSS_BEGIN_POSTKEYSTROKE`       | 0-1   | 15.0    | zero-based index of group referenced in [`begin PostKeystroke`]                          |
| 42    | `TSS_NEWLAYER`                  | 0     | 15.0    | Cannot be used in `COMP_STORE`; used for testing layer switch events                     |
| 43    | `TSS_OLDLAYER`                  | 0     | 15.0    | Cannot be used in `COMP_STORE`; used for testing layer switch events                     |
| 44    | `TSS_DISPLAYMAP`                | 0-1   | 17.0    | [`&DisplayMap`] store                                                                    |

## `COMP_GROUP`

| ∆  | Bits | Name         | Description                                                                 |
|----|------|--------------|-----------------------------------------------------------------------------|
| 0  |  32  | `dpName`     | offset to name of the group from keyboard source, `0x0` if not present      |
| 4  |  32  | `dpKeyArray` | offset to array of `COMP_KEY` entries                                       |
| 8  |  32  | `dpMatch`    | offset to string for the [`match`] statement output, `0x0` if not present   |
| 12 |  32  | `dpNoMatch`  | offset to string for the [`nomatch`] statement output, `0x0` if not present |
| 16 |  32  | `cxKeyArray` | number of `COMP_KEY` entries in `COMP_GROUP.dpKeyArray`                     |
| 20 |  32  | `fUsingKeys` | `0x1` if group is `using keys`, `0x0` otherwise                             |

The `COMP_GROUP` structure describes a [`group`] in the keyboard source file.
Note that the `readonly` property of a group is only applicable in the source
file and is never written to the binary format.

## `COMP_KEY`

| ∆  | Bits | Name         | Description                                                                                    |
|----|------|--------------|------------------------------------------------------------------------------------------------|
| 0  |  16  | `Key`        | Keyman virtual key for the rule, 0 if unused                                                   |
| 2  |  16  | (unused)     | padding, reserved                                                                              |
| 4  |  32  | `Line`       | Line number for the rule, 0 if not compiled with debug information                             |
| 8  |  32  | `ShiftFlags` | Modifier flags for the key, see description below                                              |
| 12 |  32  | `dpOutput`   | offset to string for the output of the rule, always present even for zero-length output        |
| 16 |  32  | `dpContext`  | offset to string for the context part of the rule, always present even for zero-length context |

The `COMP_KEY` structure describes a single rule in a keyboard source file. The
compiler expands `any()` statements found in the key part of a rule into
multiple rules -- so the key part of the rule is always only ever matching a
single key combination.

## Debug information for keyboards

When debug information is present, it will be found in four places in compiled
keyboards:

* `COMP_STORE.dpName`: the name of the store from the .kmn file
* `COMP_GROUP.dpName`: the name of the group from the .kmn file
* `COMP_KEY.Line`: the line number for the rule
* System stores with `COMP_STORE.dwSystemID` value of `TSS_DEBUG_LINE`

When a keyboard is compiled with debug data, the `TSS_DEBUG_LINE` system store
contains debug information about each source line in the file. In each entry,
`COMP_STORE.dpName` contains debug metadata, and `COMP_STORE.dpString` contains
only the stringified .kmn source line number where the metadata is found.

The name of the `TSS_DEBUG_LINE` system store starts with a single character
which describes the rest of the debug metadata in that store:

| Character | Debug data type       | Metadata                                                 |
|-----------|-----------------------|----------------------------------------------------------|
| `'B'`     | [`begin`] statement   | `Unicode` or `ANSI`                                      |
| `'M'`     | [`match`] statement   | Stringified index of containing group, space, group name |
| `'N'`     | [`nomatch`] statement | Stringified index of containing group, space, group name |
| `'G'`     | [`group`] statement   | Stringified index of group, space, group name            |
| `'D'`     | [`deadkey`] statement | Stringified index of deadkey, space, deadkey name        |

<!-- -------------------------- -->
<!-- references                 -->
<!-- -------------------------- -->

[KMX+ File Format]: kmx-plus-file-format.md
[`begin`]: https://help.keyman.com/developer/language/reference/begin
[`deadkey`]: https://help.keyman.com/developer/language/reference/deadkey
[`group`]: https://help.keyman.com/developer/language/reference/group
[`match`]: https://help.keyman.com/developer/language/reference/match
[`nomatch`]: https://help.keyman.com/developer/language/reference/nomatch
[`store`]: https://help.keyman.com/developer/language/reference/store
[Keyman virtual key]: https://help.keyman.com/developer/language/guide/virtual-keys
[keyboard options]: https://help.keyman.com/developer/language/guide/variable-stores
[`&Bitmap`]: https://help.keyman.com/developer/language/reference/bitmap
[`&Copyright`]: https://help.keyman.com/developer/language/reference/copyright
[`&Hotkey`]: https://help.keyman.com/developer/language/reference/hotkey
[`&Language`]: https://help.keyman.com/developer/language/reference/language
[`LAYOUT`]: https://help.keyman.com/developer/language/reference/layout
[`&Message`]: https://help.keyman.com/developer/language/reference/message
[`&Name`]: https://help.keyman.com/developer/language/reference/name
[`&Version`]: https://help.keyman.com/developer/language/reference/version
[`&CapsOnOnly`]: https://help.keyman.com/developer/language/reference/caps
[`&CapsAlwaysOff`]: https://help.keyman.com/developer/language/reference/caps
[`&ShiftFreesCaps`]: https://help.keyman.com/developer/language/reference/caps
[`&EthnologueCode`]: https://help.keyman.com/developer/language/reference/ethnologuecode
[`&MnemonicLayout`]: https://help.keyman.com/developer/language/reference/mnemoniclayout
[`&IncludeCodes`]: https://help.keyman.com/developer/language/reference/includecodes
[`&OldCharPosMatching`]: https://help.keyman.com/developer/language/reference/oldcharposmatching
[`if(&layer)`]: https://help.keyman.com/developer/language/reference/if
[`set(&layer)`]: https://help.keyman.com/developer/language/reference/set
[`&VisualKeyboard`]: https://help.keyman.com/developer/language/reference/visualkeyboard
[`&KMW_RTL`]: https://help.keyman.com/developer/language/reference/kmw_rtl
[`&KMW_HelpFile`]: https://help.keyman.com/developer/language/reference/kmw_helpfile
[`&KMW_HelpText`]: https://help.keyman.com/developer/language/reference/kmw_helptext
[`&KMW_EmbedJS`]: https://help.keyman.com/developer/language/reference/kmw_embedjs
[`&WindowsLanguages`]: https://help.keyman.com/developer/language/reference/windowslanguages
[`if(&platform)`]: https://help.keyman.com/developer/language/reference/if
[`platform()`]: https://help.keyman.com/developer/language/reference/platform
[`if(&baselayout)`]: https://help.keyman.com/developer/language/reference/if
[`baselayout()`]: https://help.keyman.com/developer/language/reference/baselayout
[`&KeyboardVersion`]: https://help.keyman.com/developer/language/reference/keyboardversion
[`&KMW_EmbedCSS`]: https://help.keyman.com/developer/language/reference/kmw_embedcss
[`&Targets`]: https://help.keyman.com/developer/language/reference/targets
[`&CasedKeys`]: https://help.keyman.com/developer/language/reference/casedkeys
[`begin NewContext`]: https://help.keyman.com/developer/language/reference/begin
[`begin PostKeystroke`]: https://help.keyman.com/developer/language/reference/begin
[`&DisplayMap`]: https://help.keyman.com/developer/language/reference/displaymap
