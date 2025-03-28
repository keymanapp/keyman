#ifndef __KEYCODES_H__
#define __KEYCODES_H__

#include <keyman/keyman_core_api.h>

// from android/KMEA/app/src/main/java/com/tavultesoft/kmea/KMScanCodeMap.java
// uses kernel keycodes which are (X11 keycode - 8)
// see /usr/include/linux/input-event-codes.h
static km_core_virtual_key const keycode_to_vk[256] = {
    0,                             // 0x00 - KEY_RESERVED
    KM_CORE_VKEY_ESC,              // 0x01 - KEY_ESC
    KM_CORE_VKEY_1,                // 0x02 - KEY_1
    KM_CORE_VKEY_2,                // 0x03 - KEY_2
    KM_CORE_VKEY_3,                // 0x04 - KEY_3
    KM_CORE_VKEY_4,                // 0x05 - KEY_4
    KM_CORE_VKEY_5,                // 0x06 - KEY_5
    KM_CORE_VKEY_6,                // 0x07 - KEY_6
    KM_CORE_VKEY_7,                // 0x08 - KEY_7
    KM_CORE_VKEY_8,                // 0x09 - KEY_8
    KM_CORE_VKEY_9,                // 0x0A - KEY_9
    KM_CORE_VKEY_0,                // 0x0B - KEY_0
    KM_CORE_VKEY_HYPHEN,           // 0x0C - KEY_MINUS
    KM_CORE_VKEY_EQUAL,            // 0x0D - KEY_EQUALS
    KM_CORE_VKEY_BKSP,             // 0x0E - KEY_BACKSPACE
    KM_CORE_VKEY_TAB,              // 0x0F - KEY_TAB
    KM_CORE_VKEY_Q,                // 0x10 - KEY_Q
    KM_CORE_VKEY_W,                // 0x11 - KEY_W
    KM_CORE_VKEY_E,                // 0x12 - KEY_E
    KM_CORE_VKEY_R,                // 0x13 - KEY_R
    KM_CORE_VKEY_T,                // 0x14 - KEY_T
    KM_CORE_VKEY_Y,                // 0x15 - KEY_Y
    KM_CORE_VKEY_U,                // 0x16 - KEY_U
    KM_CORE_VKEY_I,                // 0x17 - KEY_I
    KM_CORE_VKEY_O,                // 0x18 - KEY_O
    KM_CORE_VKEY_P,                // 0x19 - KEY_P
    KM_CORE_VKEY_LBRKT,            // 0x1A - KEY_LEFTBRACE
    KM_CORE_VKEY_RBRKT,            // 0x1B - KEY_RIGHTBRACE
    KM_CORE_VKEY_ENTER,            // 0x1C - KEY_ENTER
    KM_CORE_VKEY_CONTROL,          // 0x1D - KEY_LEFTCTRL
    KM_CORE_VKEY_A,                // 0x1E - KEY_A
    KM_CORE_VKEY_S,                // 0x1F - KEY_S
    KM_CORE_VKEY_D,                // 0x20 - KEY_D
    KM_CORE_VKEY_F,                // 0x21 - KEY_F
    KM_CORE_VKEY_G,                // 0x22 - KEY_G
    KM_CORE_VKEY_H,                // 0x23 - KEY_H
    KM_CORE_VKEY_J,                // 0x24 - KEY_J
    KM_CORE_VKEY_K,                // 0x25 - KEY_K
    KM_CORE_VKEY_L,                // 0x26 - KEY_L
    KM_CORE_VKEY_COLON,            // 0x27 - KEY_SEMICOLON
    KM_CORE_VKEY_QUOTE,            // 0x28 - KEY_APOSTROPHE
    KM_CORE_VKEY_BKQUOTE,          // 0x29 - KEY_GRAVE
    KM_CORE_VKEY_SHIFT,            // 0x2A - KEY_LEFTSHIFT
    KM_CORE_VKEY_BKSLASH,          // 0x2B - KEY_BACKSLASH
    KM_CORE_VKEY_Z,                // 0x2C - KEY_Z
    KM_CORE_VKEY_X,                // 0x2D - KEY_X
    KM_CORE_VKEY_C,                // 0x2E - KEY_C
    KM_CORE_VKEY_V,                // 0x2F - KEY_V
    KM_CORE_VKEY_B,                // 0x30 - KEY_B
    KM_CORE_VKEY_N,                // 0x31 - KEY_N
    KM_CORE_VKEY_M,                // 0x32 - KEY_M
    KM_CORE_VKEY_COMMA,            // 0x33 - KEY_COMMA
    KM_CORE_VKEY_PERIOD,           // 0x34 - KEY_DOT
    KM_CORE_VKEY_SLASH,            // 0x35 - KEY_SLASH
    KM_CORE_VKEY_SHIFT,            // 0x36 - KEY_RIGHTSHIFT
    KM_CORE_VKEY_NPSTAR,           // 0x37 - KEY_KPASTERISK
    KM_CORE_VKEY_ALT,              // 0x38 - KEY_LEFTALT
    KM_CORE_VKEY_SPACE,            // 0x39 - KEY_SPACE
    KM_CORE_VKEY_CAPS,             // 0x3A - KEY_CAPSLOCK
    KM_CORE_VKEY_F1,               // 0x3B - KEY_F1
    KM_CORE_VKEY_F2,               // 0x3C - KEY_F2
    KM_CORE_VKEY_F3,               // 0x3D - KEY_F3
    KM_CORE_VKEY_F4,               // 0x3E - KEY_F4
    KM_CORE_VKEY_F5,               // 0x3F - KEY_F5
    KM_CORE_VKEY_F6,               // 0x40 - KEY_F6
    KM_CORE_VKEY_F7,               // 0x41 - KEY_F7
    KM_CORE_VKEY_F8,               // 0x42 - KEY_F8
    KM_CORE_VKEY_F9,               // 0x43 - KEY_F9
    KM_CORE_VKEY_F10,              // 0x44 - KEY_F10
    KM_CORE_VKEY_NUMLOCK,          // 0x45 - KEY_NUMLOCK
    KM_CORE_VKEY_SCROLL,           // 0x46 - KEY_SCROLLLOCK
    KM_CORE_VKEY_NP7,              // 0x47 - KEY_KP7
    KM_CORE_VKEY_NP8,              // 0x48 - KEY_KP8
    KM_CORE_VKEY_NP9,              // 0x49 - KEY_KP9
    KM_CORE_VKEY_NPMINUS,          // 0x4A - KEY_KPMINUS
    KM_CORE_VKEY_NP4,              // 0x4B - KEY_KP4
    KM_CORE_VKEY_NP5,              // 0x4C - KEY_KP5
    KM_CORE_VKEY_NP6,              // 0x4D - KEY_KP6
    KM_CORE_VKEY_NPPLUS,           // 0x4E - KEY_KPPLUS
    KM_CORE_VKEY_NP1,              // 0x4F - KEY_KP1
    KM_CORE_VKEY_NP2,              // 0x50 - KEY_KP2
    KM_CORE_VKEY_NP3,              // 0x51 - KEY_KP3
    KM_CORE_VKEY_NP0,              // 0x52 - KEY_KP0
    KM_CORE_VKEY_NPDOT,            // 0x53 - KEY_KPDOT
    0,                             // padding 0x54;
    KM_CORE_VKEY__KANA__,          // 0x55 - KEY_ZENKAKUHANKAKU
    KM_CORE_VKEY_oE2,              // 0x56 - KEY_102ND
    KM_CORE_VKEY_F11,              // 0x57 - KEY_F11
    KM_CORE_VKEY_F12,              // 0x58 - KEY_F12
    0,                             // 0x59 - KEY_RO - ?
    KM_CORE_VKEY__KANA__,          // 0x5a - KEY_KATAKANA
    KM_CORE_VKEY__KANA__,          // 0x5b - KEY_HIRAGANA
    KM_CORE_VKEY__KANA__,          // 0x5c - KEY_HENKAN
    KM_CORE_VKEY__KANA__,          // 0x5d - KEY_KATAKANAHIRAGANA
    KM_CORE_VKEY__KANA__,          // 0x5e - KEY_MUHENKAN
    KM_CORE_VKEY_COMMA,            // 0x5f - KEY_KPJPCOMMA
    KM_CORE_VKEY_ENTER,            // 0x60 - KEY_KPENTER
    KM_CORE_VKEY_CONTROL,          // 0x61 - KEY_RIGHTCTRL
    KM_CORE_VKEY_SLASH,            // 0x62 - KEY_KPSLASH
    KM_CORE_VKEY_PRTSCN,           // 0x63 - KEY_SYSRQ
    KM_CORE_VKEY_ALT,              // 0x64 - KEY_RIGHTALT
    0,                             // 0x65 - KEY_LINEFEED - ?
    KM_CORE_VKEY_HOME,             // 0x66 - KEY_HOME
    KM_CORE_VKEY_UP,               // 0x67 - KEY_UP
    KM_CORE_VKEY_PGUP,             // 0x68 - KEY_PAGEUP
    KM_CORE_VKEY_LEFT,             // 0x69 - KEY_LEFT
    KM_CORE_VKEY_RIGHT,            // 0x6A - KEY_RIGHT
    KM_CORE_VKEY_END,              // 0x6B - KEY_END
    KM_CORE_VKEY_DOWN,             // 0x6C - KEY_DOWN
    KM_CORE_VKEY_PGDN,             // 0x6D - KEY_PAGEDOWN
    KM_CORE_VKEY_INS,              // 0x6E - KEY_INSERT
    KM_CORE_VKEY_DEL,              // 0x6F - KEY_DELETE
    0,                             // 0x70 - KEY_MACRO - ?
    KM_CORE_VKEY__VOLUMEMUTE__,    // 0x71 - KEY_MUTE
    KM_CORE_VKEY__VOLUMEDOWN__,    // 0x72 - KEY_VOLUMEDOWN
    KM_CORE_VKEY__VOLUMEUP__,      // 0x73 - KEY_VOLUMEUP
    0,                             // 0x74 - KEY_POWER
    KM_CORE_VKEY_EQUAL,            // 0x75 - KEY_KPEQUAL
    KM_CORE_VKEY_NPPLUS,           // 0x76 - KEY_KPPLUSMINUS
    KM_CORE_VKEY_PAUSE,            // 0x77 - KEY_PAUSE
    KM_CORE_VKEY__ZOOM__,          // 0x78 - KEY_SCALE
    KM_CORE_VKEY_NPDOT,            // 0x79 - KEY_KPCOMMA
    KM_CORE_VKEY__HANGUL__,        // 0x7a - KEY_HANGEUL
    KM_CORE_VKEY__HANJA__,         // 0x7b - KEY_HANJA
    0,                             // 0x7c - KEY_YEN - ?
    KM_CORE_VKEY__LWIN__,          // 0x7d - KEY_LEFTMETA
    KM_CORE_VKEY__RWIN__,          // 0x7e - KEY_RIGHTMETA
    0,                             // 0x7f - KEY_COMPOSE - ?
    KM_CORE_VKEY__BROWSERSTOP__,   // 0x80 - KEY_STOP
    KM_CORE_VKEY__BROWSERREFRESH__,// 0x81 - KEY_AGAIN
    0,                             // 0x82 - KEY_PROPS - ?
    0,                             // 0x83 - KEY_UNDO - ?
    0,                             // 0x84 - KEY_FRONT - ?
    0,                             // 0x85 - KEY_COPY - ?
    0,                             // 0x86 - KEY_OPEN - ?
    0,                             // 0x87 - KEY_PASTE - ?
    KM_CORE_VKEY__BROWSERSEARCH__, // 0x88 - KEY_FIND
    0,                             // 0x89 - KEY_CUT - ?
    KM_CORE_VKEY_HELP,             // 0x8a - KEY_HELP
    KM_CORE_VKEY_ALT,              // 0x8b - KEY_MENU
    0,                             // 0x8c - KEY_CALC - ?
    0,                             // 0x8d - KEY_SETUP - ?
    KM_CORE_VKEY__SLEEP__,         // 0x8e - KEY_SLEEP
    0,                             // 0x8f - KEY_WAKEUP - ?
    0,                             // 0x90 - KEY_FILE - ?
    0,                             // 0x91 - KEY_SENDFILE - ?
    0,                             // 0x92 - KEY_DELETEFILE - ?
    0,                             // 0x93 - KEY_XFER - ?
    KM_CORE_VKEY__APP1__,          // 0x94 - KEY_PROG1
    KM_CORE_VKEY__APP2__,          // 0x95 - KEY_PROG2
    KM_CORE_VKEY__BROWSERHOME__,   // 0x96 - KEY_WWW
    0,                             // 0x97 - KEY_MSDOS - ?
    0,                             // 0x98 - KEY_COFFEE, KEY_SCREENLOCK - ?
    0,                             // 0x99 - KEY_ROTATE_DISPLAY, KEY_DIRECTION - ?
    0,                             // 0x9a - KEY_CYCLEWINDOWS - ?
    KM_CORE_VKEY__MAIL__,          // 0x9b - KEY_MAIL
    0,                             // 0x9c - KEY_BOOKMARKS - ?
    0,                             // 0x9d - KEY_COMPUTER - ?
    KM_CORE_VKEY__BROWSERBACK__,   // 0x9e - KEY_BACK
    KM_CORE_VKEY__BROWSERFORWARD__,// 0x9f - KEY_FORWARD
    0,                             // 0xa0 - KEY_CLOSECD - ?
    0,                             // 0xa1 - KEY_EJECTCD - ?
    0,                             // 0xa2 - KEY_EJECTCLOSECD - ?
    KM_CORE_VKEY__MEDIANEXT__,     // 0xa3 - KEY_NEXTSONG
    KM_CORE_VKEY__MEDIAPLAY__,     // 0xa4 - KEY_PLAYPAUSE
    KM_CORE_VKEY__MEDIAPREV__,     // 0xa5 - KEY_PREVIOUSSONG
    0,                             // 0xa6 - KEY_STOPCD - ?
    0,                             // 0xa7 - KEY_RECORD - ?
    0,                             // 0xa8 - KEY_REWIND - ?
    KM_CORE_VKEY__MEDIA__,         // 0xa9 - KEY_PHONE
    0,                             // 0xaa - KEY_ISO - ?
    0,                             // 0xab - KEY_CONFIG - ?
    KM_CORE_VKEY__BROWSERHOME__,   // 0xac - KEY_HOMEPAGE
    KM_CORE_VKEY__BROWSERREFRESH__,// 0xad - KEY_REFRESH
    0,                             // 0xae - KEY_EXIT - ?
    0,                             // 0xaf - KEY_MOVE - ?
    0,                             // 0xb0 - KEY_EDIT - ?
    0,                             // 0xb1 - KEY_SCROLLUP - ?
    0,                             // 0xb2 - KEY_SCROLLDOWN - ?
    0,                             // 0xb3 - KEY_KPLEFTPAREN - ?
    0,                             // 0xb4 - KEY_KPRIGHTPAREN - ?
    0,                             // 0xb5 - KEY_NEW - ?
    0,                             // 0xb6 - KEY_REDO - ?
    KM_CORE_VKEY_F13,              // 0xb7 - KEY_F13
    KM_CORE_VKEY_F14,              // 0xb8 - KEY_F14
    KM_CORE_VKEY_F15,              // 0xb9 - KEY_F15
    KM_CORE_VKEY_F16,              // 0xba - KEY_F16
    KM_CORE_VKEY_F17,              // 0xbb - KEY_F17
    KM_CORE_VKEY_F18,              // 0xbc - KEY_F18
    KM_CORE_VKEY_F19,              // 0xbd - KEY_F19
    KM_CORE_VKEY_F20,              // 0xbe - KEY_F20
    KM_CORE_VKEY_F21,              // 0xbf - KEY_F21
    KM_CORE_VKEY_F22,              // 0xc0 - KEY_F22
    KM_CORE_VKEY_F23,              // 0xc1 - KEY_F23
    KM_CORE_VKEY_F24,              // 0xc2 - KEY_F24
    0,                             // 0xc3 - unassigned
    0,                             // 0xc4 - unassigned
    0,                             // 0xc5 - unassigned
    0,                             // 0xc6 - unassigned
    0,                             // 0xc7 - unassigned
    KM_CORE_VKEY__MEDIAPLAY__,     // 0xc8 - KEY_PLAYCD
    KM_CORE_VKEY__MEDIASTOP__,     // 0xc9 - KEY_PAUSECD
    0,                             // 0xca - KEY_PROG3 - ?
    0,                             // 0xcb - KEY_PROG4 - ?
    0,                             // 0xcc - KEY_ALL_APPLICATIONS, KEY_DASHBOARD - ?
    0,                             // 0xcd - KEY_SUSPEND - ?
    0,                             // 0xce - KEY_CLOSE - ?
    KM_CORE_VKEY__MEDIAPLAY__,     // 0xcf - KEY_PLAY
    0,                             // 0xd0 - KEY_FASTFORWARD - ?
    0,                             // 0xd1 - KEY_BASSBOOST - ?
    KM_CORE_VKEY_PRINT,            // 0xd2 - KEY_PRINT
    0,                             // 0xd3 - KEY_HP - ?
    0,                             // 0xd4 - KEY_CAMERA - ?
    0,                             // 0xd5 - KEY_SOUND - ?
    0,                             // 0xd6 - KEY_QUESTION - ?
    KM_CORE_VKEY__MAIL__,          // 0xd7 - KEY_EMAIL
    0,                             // 0xd8 - KEY_CHAT - ?
    0,                             // 0xd9 - KEY_SEARCH - ?
    0,                             // 0xda - KEY_CONNECT - ?
    0,                             // 0xdb - KEY_FINANCE - ?
    0,                             // 0xdc - KEY_SPORT - ?
    0,                             // 0xdd - KEY_SHOP - ?
    0,                             // 0xde - KEY_ALTERASE - ?
    0,                             // 0xdf - KEY_CANCEL - ?
    0,                             // 0xe0 - KEY_BRIGHTNESSDOWN - ?
    0,                             // 0xe1 - KEY_BRIGHTNESSUP - ?
    KM_CORE_VKEY__MEDIA__,         // 0xe2 - KEY_MEDIA
    0,                             // 0xe3 - KEY_SWITCHVIDEOMODE - ?
    0,                             // 0xe4 - KEY_KBDILLUMTOGGLE - ?
    0,                             // 0xe5 - KEY_KBDILLUMDOWN - ?
    0,                             // 0xe6 - KEY_KBDILLUMUP - ?
    0,                             // 0xe7 - KEY_SEND - ?
    0,                             // 0xe8 - KEY_REPLY - ?
    0,                             // 0xe9 - KEY_FORWARDMAIL - ?
    0,                             // 0xea - KEY_SAVE - ?
    0,                             // 0xeb - KEY_DOCUMENTS - ?
    0,                             // 0xec - KEY_BATTERY - ?
    0,                             // 0xed - KEY_BLUETOOTH - ?
    0,                             // 0xee - KEY_WLAN - ?
    0,                             // 0xef - KEY_UWB - ?
    0,                             // 0xf0 - KEY_UNKNOWN - ?
    0,                             // 0xf1 - KEY_VIDEO_NEXT - ?
    0,                             // 0xf2 - KEY_VIDEO_PREV - ?
    0,                             // 0xf3 - KEY_BRIGHTNESS_CYCLE - ?
    0,                             // 0xf4 - KEY_BRIGHTNESS_AUTO, KEY_BRIGHTNESS_ZERO - ?
    0,                             // 0xf5 - KEY_DISPLAY_OFF - ?
    0,                             // 0xf6 - KEY_WWAN, KEY_WIMAX - ?
    0,                             // 0xf7 - KEY_RFKILL - ?
    0,                             // 0xf8 - KEY_MICMUTE - ?
};

#endif // __KEYCODES_H__
