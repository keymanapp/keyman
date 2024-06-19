meta:
  id: kvk
  title: Keyman Visual Keyboard
  file-extension: kvk
  license: MIT
  ks-version: 0.9
  endian: le
  bit-endian: le
doc: |
  KVK is the binary file format for Keyman Visual Keyboard
  files. KVKS is the equivalent XML source file format
doc-ref:
  - https://github.com/keymanapp/keyman/
seq:
  - id: header
    type: header
  - id: keys
    type: keys
types:
  header:
    seq:
      - id: identifier
        contents: 'KVKF'
        doc: Magic file identifier, always KVKF
      - id: version
        contents: [0, 6, 0, 0]
        doc: Version number of KVK file format, always 0x00000600
      - id: flag
        type: header_flags
      - id: associated_keyboard
        type: string
      - id: ansi_font
        type: font
      - id: unicode_font
        type: font

  header_flags:
    seq:
      - id: display_102
        type: b1
        doc: kvkh102, Keyboard should display 102nd key
      - id: display_underlying
        type: b1
        doc: kvkhDisplayUnderlying, Keyboard should display underlying characters
      - id: use_underlying
        type: b1
        doc: kvkhUseUnderlying,
      - id: altgr
        type: b1
        doc: kvkhAltGr, Keyboard should treat left/right Ctrl and Alt separately

  keys:
    seq:
      - id: count
        type: u4
      - id: key
        type: key
        repeat: expr
        repeat-expr: count
  key:
    seq:
      - id: flags
        type: key_flags
      - id: modifiers
        type: key_modifiers
      - id: vkey
        type: u2
      - id: text
        type: string
      - id: bitmap
        type: u4

  key_modifiers:
    seq:
      - id: shift
        type: b1
      - id: ctrl
        type: b1
      - id: alt
        type: b1
      - id: lctrl
        type: b1
      - id: rctrl
        type: b1
      - id: lalt
        type: b1
      - id: ralt
        type: b1
      - id: padding
        type: b1
        doc: reserved,
      - id: zeropad
        contents: [0]

  key_flags:
    seq:
      - id: bitmap
        type: b1
      - id: unicode
        type: b1
      - id: padding
        type: b6

  font:
    seq:
      - id: name
        type: string
      - id: size
        type: u4
      - id: color
        type: u4

  string:
    seq:
      - id: len
        type: u2
      - id: str
        type: str
        size: len*2 - 2
        encoding: utf-16
      - id: zero_terminator
        contents: [0,0]
