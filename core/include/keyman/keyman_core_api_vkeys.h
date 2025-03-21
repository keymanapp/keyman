/*
  Copyright:    © 2018 SIL International.
  Description:  API declarations for modifier keys, handy access masks and
                Keyman VKEY names.  These follow the same keytop->code
                associations as the Windows API. This is a separate header to
                maintain readability of the primary API header.
  Create Date:  17 Oct 2018
  Authors:      Tim Eves (TSE)
  History:      17 Oct 2018 - TSE - Moved & refactored km_core_modifier_state
                                    from keyman_core_api.h.
                                  - Added VKey and mask definitions.
                 6 Oct 2018 - TSE - Move into keyman folder.

*/

#pragma once

enum km_core_modifier_state {
  KM_CORE_MODIFIER_NONE        = 0,
  KM_CORE_MODIFIER_LCTRL       = 1 << 0,
  KM_CORE_MODIFIER_RCTRL       = 1 << 1,
  KM_CORE_MODIFIER_LALT        = 1 << 2,
  KM_CORE_MODIFIER_RALT        = 1 << 3,
  KM_CORE_MODIFIER_SHIFT       = 1 << 4,
  KM_CORE_MODIFIER_CTRL        = 1 << 5,
  KM_CORE_MODIFIER_ALT         = 1 << 6,
  /*
    KM_CORE_MODIFIER_META        = 1 << 7,    // Either Meta-key flag (tentative).  Not usable by keyboards currently
                                             // Used internally (currently, only by KMW) to ensure Meta-key
                                             // shortcuts safely bypass rules
                                             // Meta key = Command key on macOS, Windows key on Windows
  */
  KM_CORE_MODIFIER_CAPS        = 1 << 8,
  KM_CORE_MODIFIER_NOCAPS      = 1 << 9,
  /*
    KM_CORE_MODIFIER_NUMLOCK     = 1 << 10,
    KM_CORE_MODIFIER_NONUMLOCK   = 1 << 11,
    KM_CORE_MODIFIER_SCROLLOCK   = 1 << 12,
    KM_CORE_MODIFIER_NOSCROLLOCK = 1 << 13,
    KM_CORE_MODIFIER_VIRTUALKEY  = 1 << 14,
  */
};

enum km_core_modifier_mask {
  KM_CORE_MODIFIER_MASK_ALL         = 0x7f,
  KM_CORE_MODIFIER_MASK_ALT_GR_SIM  = KM_CORE_MODIFIER_LCTRL|KM_CORE_MODIFIER_LALT,
  KM_CORE_MODIFIER_MASK_CHIRAL      = 0x1f,
  KM_CORE_MODIFIER_MASK_IS_CHIRAL   = 0x0f,
  KM_CORE_MODIFIER_MASK_NON_CHIRAL  = 0x7f,
  KM_CORE_MODIFIER_MASK_CAPS        = 0x0300,
/*KM_CORE_MODIFIER_MASK_NUMLOCK     = 0x0C00,
  KM_CORE_MODIFIER_MASK_SCROLLLOCK  = 0x3000,*/
};

// These are Windows API VKEYs, using Keyman VKEY names.
// Underlying values from winuser.h (https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes)
enum km_kpb_virtual_key {
  KM_CORE_VKEY__00,
  KM_CORE_VKEY_LBUTTON,           // 0x01
  KM_CORE_VKEY_RBUTTON,           // 0x02
  KM_CORE_VKEY_CANCEL,            // 0x03
  KM_CORE_VKEY_MBUTTON,           // 0x04
  KM_CORE_VKEY_XBUTTON1,          // 0x05
  KM_CORE_VKEY_XBUTTON2,          // 0x06
  KM_CORE_VKEY__07,               // 0x07 - Reserved
  KM_CORE_VKEY_BKSP,              // 0x08 - VK_BACK
  KM_CORE_VKEY_TAB,               // 0x09
  KM_CORE_VKEY__0A,               // 0x0a - Reserved
  KM_CORE_VKEY__0B,               // 0x0b - Reserved
  KM_CORE_VKEY_KP5,               // 0x0c - VK_CLEAR
  KM_CORE_VKEY_ENTER,             // 0x0d - VK_RETURN
  KM_CORE_VKEY__0E,               // 0x0e - Reserved
  KM_CORE_VKEY__0F,               // 0x0f - Reserved
  KM_CORE_VKEY_SHIFT,             // 0x10
  KM_CORE_VKEY_CONTROL,           // 0x11
  KM_CORE_VKEY_ALT,               // 0x12 - VK_MENU
  KM_CORE_VKEY_PAUSE,             // 0x13
  KM_CORE_VKEY_CAPS,              // 0x14 - VK_CAPITAL
  KM_CORE_VKEY_KANA,              // 0x15
  KM_CORE_VKEY_HANGUL,            // 0x16
  KM_CORE_VKEY_JUNJA,             // 0x17
  KM_CORE_VKEY_FINAL,             // 0x18
  KM_CORE_VKEY_HANJA,             // 0x19 - or VK_KANJI
  KM_CORE_VKEY_IMEOFF,            // 0x1a - VK_IME_OFF
  KM_CORE_VKEY_ESC,               // 0x1b - VK_ESCAPE
  KM_CORE_VKEY_CONVERT,           // 0x1c
  KM_CORE_VKEY_NONCONVERT,        // 0x1d
  KM_CORE_VKEY_ACCEPT,            // 0x1e
  KM_CORE_VKEY_MODECHANGE,        // 0x1f
  KM_CORE_VKEY_SPACE,             // 0x20
  KM_CORE_VKEY_PGUP,              // 0x21 - VK_PRIOR
  KM_CORE_VKEY_PGDN,              // 0x22 - VK_NEXT
  KM_CORE_VKEY_END,               // 0x23
  KM_CORE_VKEY_HOME,              // 0x24
  KM_CORE_VKEY_LEFT,              // 0x25
  KM_CORE_VKEY_UP,                // 0x26
  KM_CORE_VKEY_RIGHT,             // 0x27
  KM_CORE_VKEY_DOWN,              // 0x28
  KM_CORE_VKEY_SEL,               // 0x29 - VK_SELECT
  KM_CORE_VKEY_PRINT,             // 0x2a
  KM_CORE_VKEY_EXEC,              // 0x2b - VK_EXECUTE
  KM_CORE_VKEY_PRTSCN,            // 0x2c - VK_SNAPSHOT
  KM_CORE_VKEY_INS,               // 0x2d - VK_INSERT
  KM_CORE_VKEY_DEL,               // 0x2e - VK_DELETE
  KM_CORE_VKEY_HELP,              // 0x2f
  KM_CORE_VKEY_0,                 // 0x30
  KM_CORE_VKEY_1,                 // 0x31
  KM_CORE_VKEY_2,                 // 0x32
  KM_CORE_VKEY_3,                 // 0x33
  KM_CORE_VKEY_4,                 // 0x34
  KM_CORE_VKEY_5,                 // 0x35
  KM_CORE_VKEY_6,                 // 0x36
  KM_CORE_VKEY_7,                 // 0x37
  KM_CORE_VKEY_8,                 // 0x38
  KM_CORE_VKEY_9,                 // 0x39
  KM_CORE_VKEY__3A,               // 0x3a - undefined
  KM_CORE_VKEY__3B,               // 0x3b - undefined
  KM_CORE_VKEY__3C,               // 0x3c - undefined
  KM_CORE_VKEY__3D,               // 0x3d - undefined
  KM_CORE_VKEY__3E,               // 0x3e - undefined
  KM_CORE_VKEY__3F,               // 0x3f - undefined
  KM_CORE_VKEY__40,               // 0x40 - undefined
  KM_CORE_VKEY_A,                 // 0x41
  KM_CORE_VKEY_B,                 // 0x42
  KM_CORE_VKEY_C,                 // 0x43
  KM_CORE_VKEY_D,                 // 0x44
  KM_CORE_VKEY_E,                 // 0x45
  KM_CORE_VKEY_F,                 // 0x46
  KM_CORE_VKEY_G,                 // 0x47
  KM_CORE_VKEY_H,                 // 0x48
  KM_CORE_VKEY_I,                 // 0x49
  KM_CORE_VKEY_J,                 // 0x4a
  KM_CORE_VKEY_K,                 // 0x4b
  KM_CORE_VKEY_L,                 // 0x4c
  KM_CORE_VKEY_M,                 // 0x4d
  KM_CORE_VKEY_N,                 // 0x4e
  KM_CORE_VKEY_O,                 // 0x4f
  KM_CORE_VKEY_P,                 // 0x50
  KM_CORE_VKEY_Q,                 // 0x51
  KM_CORE_VKEY_R,                 // 0x52
  KM_CORE_VKEY_S,                 // 0x53
  KM_CORE_VKEY_T,                 // 0x54
  KM_CORE_VKEY_U,                 // 0x55
  KM_CORE_VKEY_V,                 // 0x56
  KM_CORE_VKEY_W,                 // 0x57
  KM_CORE_VKEY_X,                 // 0x58
  KM_CORE_VKEY_Y,                 // 0x59
  KM_CORE_VKEY_Z,                 // 0x5a
  KM_CORE_VKEY_LWIN,              // 0x5b
  KM_CORE_VKEY_RWIN,              // 0x5c
  KM_CORE_VKEY_APPS,              // 0x5d
  KM_CORE_VKEY__5E,               // 0x5e - reserved
  KM_CORE_VKEY_SLEEP,             // 0x5f
  KM_CORE_VKEY_NP0,               // 0x60 - VK_NUMPAD0
  KM_CORE_VKEY_NP1,               // 0x61 - VK_NUMPAD1
  KM_CORE_VKEY_NP2,               // 0x62 - VK_NUMPAD2
  KM_CORE_VKEY_NP3,               // 0x63 - VK_NUMPAD3
  KM_CORE_VKEY_NP4,               // 0x64 - VK_NUMPAD4
  KM_CORE_VKEY_NP5,               // 0x65 - VK_NUMPAD5
  KM_CORE_VKEY_NP6,               // 0x66 - VK_NUMPAD6
  KM_CORE_VKEY_NP7,               // 0x67 - VK_NUMPAD7
  KM_CORE_VKEY_NP8,               // 0x68 - VK_NUMPAD8
  KM_CORE_VKEY_NP9,               // 0x69 - VK_NUMPAD9
  KM_CORE_VKEY_NPSTAR,            // 0x6a - VK_MULTIPLY
  KM_CORE_VKEY_NPPLUS,            // 0x6b - VK_ADD
  KM_CORE_VKEY_SEPARATOR,         // 0x6c
  KM_CORE_VKEY_NPMINUS,           // 0x6d - VK_SUBTRACT
  KM_CORE_VKEY_NPDOT,             // 0x6e - VK_DECIMAL
  KM_CORE_VKEY_NPSLASH,           // 0x6f - VK_DIVIDE
  KM_CORE_VKEY_F1,                // 0x70
  KM_CORE_VKEY_F2,                // 0x71
  KM_CORE_VKEY_F3,                // 0x72
  KM_CORE_VKEY_F4,                // 0x73
  KM_CORE_VKEY_F5,                // 0x74
  KM_CORE_VKEY_F6,                // 0x75
  KM_CORE_VKEY_F7,                // 0x76
  KM_CORE_VKEY_F8,                // 0x77
  KM_CORE_VKEY_F9,                // 0x78
  KM_CORE_VKEY_F10,               // 0x79
  KM_CORE_VKEY_F11,               // 0x7a
  KM_CORE_VKEY_F12,               // 0x7b
  KM_CORE_VKEY_F13,               // 0x7c
  KM_CORE_VKEY_F14,               // 0x7d
  KM_CORE_VKEY_F15,               // 0x7e
  KM_CORE_VKEY_F16,               // 0x7f
  KM_CORE_VKEY_F17,               // 0x80
  KM_CORE_VKEY_F18,               // 0x81
  KM_CORE_VKEY_F19,               // 0x82
  KM_CORE_VKEY_F20,               // 0x83
  KM_CORE_VKEY_F21,               // 0x84
  KM_CORE_VKEY_F22,               // 0x85
  KM_CORE_VKEY_F23,               // 0x86
  KM_CORE_VKEY_F24,               // 0x87
  KM_CORE_VKEY__88,               // 0x88 - reserved
  KM_CORE_VKEY__89,               // 0x89 - reserved
  KM_CORE_VKEY__8A,               // 0x8a - reserved
  KM_CORE_VKEY__8B,               // 0x8b - reserved
  KM_CORE_VKEY__8C,               // 0x8c - reserved
  KM_CORE_VKEY__8D,               // 0x8d - reserved
  KM_CORE_VKEY__8E,               // 0x8e - reserved
  KM_CORE_VKEY__8F,               // 0x8f - reserved
  KM_CORE_VKEY_NUMLOCK,           // 0x90
  KM_CORE_VKEY_SCROLL,            // 0x91
  KM_CORE_VKEY__92,               // 0x92 - OEM specific
  KM_CORE_VKEY__93,               // 0x93 - OEM specific
  KM_CORE_VKEY__94,               // 0x94 - OEM specific
  KM_CORE_VKEY__95,               // 0x95 - OEM specific
  KM_CORE_VKEY__96,               // 0x96 - OEM specific
  KM_CORE_VKEY__97,               // 0x97 - unassigned
  KM_CORE_VKEY__98,               // 0x98 - unassigned
  KM_CORE_VKEY__99,               // 0x99 - unassigned
  KM_CORE_VKEY__9A,               // 0x9a - unassigned
  KM_CORE_VKEY__9B,               // 0x9b - unassigned
  KM_CORE_VKEY__9C,               // 0x9c - unassigned
  KM_CORE_VKEY__9D,               // 0x9d - unassigned
  KM_CORE_VKEY__9E,               // 0x9e - unassigned
  KM_CORE_VKEY__9F,               // 0x9f - unassigned
  KM_CORE_VKEY_LSHIFT,            // 0xa0
  KM_CORE_VKEY_RSHIFT,            // 0xa1
  KM_CORE_VKEY_LCONTROL,          // 0xa2
  KM_CORE_VKEY_RCONTROL,          // 0xa3
  KM_CORE_VKEY_LMENU,             // 0xa4
  KM_CORE_VKEY_RMENU,             // 0xa5
  KM_CORE_VKEY_BROWSERBACK,       // 0xa6 - VK_BROWSER_BACK
  KM_CORE_VKEY_BROWSERFORWARD,    // 0xa7 - VK_BROWSER_FORWARD
  KM_CORE_VKEY_BROWSERREFRESH,    // 0xa8 - VK_BROWSER_REFRESH
  KM_CORE_VKEY_BROWSERSTOP,       // 0xa9 - VK_BROWSER_STOP
  KM_CORE_VKEY_BROWSERSEARCH,     // 0xaa - VK_BROWSER_SEARCH
  KM_CORE_VKEY_BROWSERFAVORITES,  // 0xab - VK_BROWSER_FAVORITES
  KM_CORE_VKEY_BROWSERHOME,       // 0xac - VK_BROWSER_HOME
  KM_CORE_VKEY_VOLUMEMUTE,        // 0xad - VK_VOLUME_MUTE
  KM_CORE_VKEY_VOLUMEDOWN,        // 0xae - VK_VOLUME_DOWN
  KM_CORE_VKEY_VOLUMEUP,          // 0xaf - VK_VOLUME_UP
  KM_CORE_VKEY_MEDIANEXT,         // 0xb0 - VK_MEDIA_NEXT_TRACK
  KM_CORE_VKEY_MEDIAPREV,         // 0xb1 - VK_MEDIA_PREV_TRACK
  KM_CORE_VKEY_MEDIASTOP,         // 0xb2 - VK_MEDIA_STOP
  KM_CORE_VKEY_MEDIAPLAY,         // 0xb3 - VK_MEDIA_PLAY_PAUSE
  KM_CORE_VKEY_MAIL,              // 0xb4 - VK_LAUNCH_MAIL
  KM_CORE_VKEY_MEDIA,             // 0xb5 - VK_LAUNCH_MEDIA_SELECT
  KM_CORE_VKEY_APP1,              // 0xb6 - VK_LAUNCH_APP1
  KM_CORE_VKEY_APP2,              // 0xb7 - VK_LAUNCH_APP2
  KM_CORE_VKEY__B8,               // 0xb8 - reserved
  KM_CORE_VKEY__B9,               // 0xb9 - reserved
  KM_CORE_VKEY_COLON,             // 0xba - VK_OEM_1
  KM_CORE_VKEY_EQUAL,             // 0xbb - VK_OEM_PLUS
  KM_CORE_VKEY_COMMA,             // 0xbc - VK_OEM_COMMA
  KM_CORE_VKEY_HYPHEN,            // 0xbd - VK_OEM_MINUS
  KM_CORE_VKEY_PERIOD,            // 0xbe - VK_OEM_PERIOD
  KM_CORE_VKEY_SLASH,             // 0xbf - VK_OEM_2
  KM_CORE_VKEY_BKQUOTE,           // 0xc0 - VK_OEM_3
  KM_CORE_VKEY__C1,               // 0xc1 - reserved
  KM_CORE_VKEY__C2,               // 0xc2 - reserved
  KM_CORE_VKEY__C3,               // 0xc3 - reserved
  KM_CORE_VKEY__C4,               // 0xc4 - reserved
  KM_CORE_VKEY__C5,               // 0xc5 - reserved
  KM_CORE_VKEY__C6,               // 0xc6 - reserved
  KM_CORE_VKEY__C7,               // 0xc7 - reserved
  KM_CORE_VKEY__C8,               // 0xc8 - reserved
  KM_CORE_VKEY__C9,               // 0xc9 - reserved
  KM_CORE_VKEY__CA,               // 0xca - reserved
  KM_CORE_VKEY__CB,               // 0xcb - reserved
  KM_CORE_VKEY__CC,               // 0xcc - reserved
  KM_CORE_VKEY__CD,               // 0xcd - reserved
  KM_CORE_VKEY__CE,               // 0xce - reserved
  KM_CORE_VKEY__CF,               // 0xcf - reserved
  KM_CORE_VKEY__D0,               // 0xd0 - reserved
  KM_CORE_VKEY__D1,               // 0xd1 - reserved
  KM_CORE_VKEY__D2,               // 0xd2 - reserved
  KM_CORE_VKEY__D3,               // 0xd3 - reserved
  KM_CORE_VKEY__D4,               // 0xd4 - reserved
  KM_CORE_VKEY__D5,               // 0xd5 - reserved
  KM_CORE_VKEY__D6,               // 0xd6 - reserved
  KM_CORE_VKEY__D7,               // 0xd7 - reserved
  KM_CORE_VKEY__D8,               // 0xd8 - reserved
  KM_CORE_VKEY__D9,               // 0xd9 - reserved
  KM_CORE_VKEY__DA,               // 0xda - reserved
  KM_CORE_VKEY_LBRKT,             // 0xdb - VK_OEM_4
  KM_CORE_VKEY_BKSLASH,           // 0xdc - VK_OEM_5
  KM_CORE_VKEY_RBRKT,             // 0xdd - VK_OEM_6
  KM_CORE_VKEY_QUOTE,             // 0xde - VK_OEM_7
  KM_CORE_VKEY_oDF,               // 0xdf - VK_OEM_8
  KM_CORE_VKEY_oE0,               // 0xe0 - reserved
  KM_CORE_VKEY_oE1,               // 0xe1 - reserved
  KM_CORE_VKEY_oE2,               // 0xe2 - VK_OEM_102 - 102nd key on European layouts
  KM_CORE_VKEY_oE3,               // 0xe3 - OEM specific
  KM_CORE_VKEY_oE4,               // 0xe4 - OEM specific
  KM_CORE_VKEY_PROCESSKEY,        // 0xe5
  KM_CORE_VKEY_oE6,               // 0xe6 - OEM specific
  KM_CORE_VKEY_PACKET,            // 0xe7
  KM_CORE_VKEY__E8,               // 0xe8 - unassigned
  KM_CORE_VKEY_oE9,               // 0xe9 - OEM specific
  KM_CORE_VKEY_oEA,               // 0xea - OEM specific
  KM_CORE_VKEY_oEB,               // 0xeb - OEM specific
  KM_CORE_VKEY_oEC,               // 0xec - OEM specific
  KM_CORE_VKEY_oED,               // 0xed - OEM specific
  KM_CORE_VKEY_oEE,               // 0xee - OEM specific
  KM_CORE_VKEY_oEF,               // 0xef - OEM specific
  KM_CORE_VKEY_oF0,               // 0xf0 - OEM specific
  KM_CORE_VKEY_oF1,               // 0xf1 - OEM specific
  KM_CORE_VKEY_oF2,               // 0xf2 - OEM specific
  KM_CORE_VKEY_oF3,               // 0xf3 - OEM specific
  KM_CORE_VKEY_oF4,               // 0xf4 - OEM specific
  KM_CORE_VKEY_oF5,               // 0xf5 - OEM specific
  KM_CORE_VKEY_ATTN,              // 0xf6
  KM_CORE_VKEY_CRSEL,             // 0xf7
  KM_CORE_VKEY_EXSEL,             // 0xf8
  KM_CORE_VKEY_EREOF,             // 0xf9
  KM_CORE_VKEY_PLAY,              // 0xfa
  KM_CORE_VKEY_ZOOM,              // 0xfb
  KM_CORE_VKEY_NONAME,            // 0xfc
  KM_CORE_VKEY_PA1,               // 0xfd
  KM_CORE_VKEY_OEMCLEAR,          // 0xfe
  KM_CORE_VKEY__FF,               // 0xff
};
