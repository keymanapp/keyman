#ifndef __KEYCODES_H__
#define __KEYCODES_H__

#include <keyman/keyman_core_api.h>

// from android/KMEA/app/src/main/java/com/tavultesoft/kmea/KMScanCodeMap.java
// uses kernel keycodes which are (X11 keycode - 8)
// see /usr/include/linux/input-event-codes.h
static km_core_virtual_key const keycode_to_vk[256] = {
    0,                    // KEY_RESERVED = 0x00;
    KM_CORE_VKEY_ESC,      // KEY_ESC = 0x01;
    KM_CORE_VKEY_1,        // KEY_1 = 0x02;
    KM_CORE_VKEY_2,        // KEY_2 = 0x03;
    KM_CORE_VKEY_3,        // KEY_3 = 0x04;
    KM_CORE_VKEY_4,        // KEY_4 = 0x05;
    KM_CORE_VKEY_5,        // KEY_5 = 0x06;
    KM_CORE_VKEY_6,        // KEY_6 = 0x07;
    KM_CORE_VKEY_7,        // KEY_7 = 0x08;
    KM_CORE_VKEY_8,        // KEY_8 = 0x09;
    KM_CORE_VKEY_9,        // KEY_9 = 0x0A;
    KM_CORE_VKEY_0,        // KEY_0 = 0x0B;
    KM_CORE_VKEY_HYPHEN,   // KEY_MINUS = 0x0C;
    KM_CORE_VKEY_EQUAL,    // KEY_EQUALS = 0x0D;
    KM_CORE_VKEY_BKSP,     // KEY_BACKSPACE = 0x0E;
    KM_CORE_VKEY_TAB,      // KEY_TAB = 0x0F;
    KM_CORE_VKEY_Q,        // KEY_Q = 0x10;
    KM_CORE_VKEY_W,        // KEY_W = 0x11;
    KM_CORE_VKEY_E,        // KEY_E = 0x12;
    KM_CORE_VKEY_R,        // KEY_R = 0x13;
    KM_CORE_VKEY_T,        // KEY_T = 0x14;
    KM_CORE_VKEY_Y,        // KEY_Y = 0x15;
    KM_CORE_VKEY_U,        // KEY_U = 0x16;
    KM_CORE_VKEY_I,        // KEY_I = 0x17;
    KM_CORE_VKEY_O,        // KEY_O = 0x18;
    KM_CORE_VKEY_P,        // KEY_P = 0x19;
    KM_CORE_VKEY_LBRKT,    // KEY_LEFTBRACE = 0x1A;
    KM_CORE_VKEY_RBRKT,    // KEY_RIGHTBRACE = 0x1B;
    KM_CORE_VKEY_ENTER,    // KEY_ENTER = 0x1C;
    KM_CORE_VKEY_CONTROL,  // KEY_LEFTCTRL = 0x1D;
    KM_CORE_VKEY_A,        // KEY_A = 0x1E;
    KM_CORE_VKEY_S,        // KEY_S = 0x1F;
    KM_CORE_VKEY_D,        // KEY_D = 0x20;
    KM_CORE_VKEY_F,        // KEY_F = 0x21;
    KM_CORE_VKEY_G,        // KEY_G = 0x22;
    KM_CORE_VKEY_H,        // KEY_H = 0x23;
    KM_CORE_VKEY_J,        // KEY_J = 0x24;
    KM_CORE_VKEY_K,        // KEY_K = 0x25;
    KM_CORE_VKEY_L,        // KEY_L = 0x26;
    KM_CORE_VKEY_COLON,    // KEY_SEMICOLON = 0x27;
    KM_CORE_VKEY_QUOTE,    // KEY_APOSTROPHE = 0x28;
    KM_CORE_VKEY_BKQUOTE,  // KEY_GRAVE = 0x29;
    KM_CORE_VKEY_SHIFT,    // KEY_LEFTSHIFT = 0x2A;
    KM_CORE_VKEY_BKSLASH,  // KEY_BACKSLASH = 0x2B;
    KM_CORE_VKEY_Z,        // KEY_Z = 0x2C;
    KM_CORE_VKEY_X,        // KEY_X = 0x2D;
    KM_CORE_VKEY_C,        // KEY_C = 0x2E;
    KM_CORE_VKEY_V,        // KEY_V = 0x2F;
    KM_CORE_VKEY_B,        // KEY_B = 0x30;
    KM_CORE_VKEY_N,        // KEY_N = 0x31;
    KM_CORE_VKEY_M,        // KEY_M = 0x32;
    KM_CORE_VKEY_COMMA,    // KEY_COMMA = 0x33;
    KM_CORE_VKEY_PERIOD,   // KEY_DOT = 0x34;
    KM_CORE_VKEY_SLASH,    // KEY_SLASH = 0x35;
    KM_CORE_VKEY_SHIFT,    // KEY_RIGHTSHIFT = 0x36;
    KM_CORE_VKEY_NPSTAR,   // KEY_KPASTERISK = 0x37;
    KM_CORE_VKEY_ALT,      // KEY_LEFTALT = 0x38;
    KM_CORE_VKEY_SPACE,    // KEY_SPACE = 0x39;
    KM_CORE_VKEY_CAPS,     // KEY_CAPSLOCK = 0x3A;
    KM_CORE_VKEY_F1,       // KEY_F1 = 0x3B;
    KM_CORE_VKEY_F2,       // KEY_F2 = 0x3C;
    KM_CORE_VKEY_F3,       // KEY_F3 = 0x3D;
    KM_CORE_VKEY_F4,       // KEY_F4 = 0x3E;
    KM_CORE_VKEY_F5,       // KEY_F5 = 0x3F;
    KM_CORE_VKEY_F6,       // KEY_F6 = 0x40;
    KM_CORE_VKEY_F7,       // KEY_F7 = 0x41;
    KM_CORE_VKEY_F8,       // KEY_F8 = 0x42;
    KM_CORE_VKEY_F9,       // KEY_F9 = 0x43;
    KM_CORE_VKEY_F10,      // KEY_F10 = 0x44;
    0,                    // KEY_NUMLOCK = 0x45;
    0,                    // KEY_SCROLLLOCK = 0x46;
    KM_CORE_VKEY_NP7,      // KEY_KP7 = 0x47;
    KM_CORE_VKEY_NP8,      // KEY_KP8 = 0x48;
    KM_CORE_VKEY_NP9,      // KEY_KP9 = 0x49;
    KM_CORE_VKEY_NPMINUS,  // KEY_KPMINUS = 0x4A;
    KM_CORE_VKEY_NP4,      // KEY_KP4 = 0x4B;
    KM_CORE_VKEY_NP5,      // KEY_KP5 = 0x4C;
    KM_CORE_VKEY_NP6,      // KEY_KP6 = 0x4D;
    KM_CORE_VKEY_NPPLUS,   // KEY_KPPLUS = 0x4E;
    KM_CORE_VKEY_NP1,      // KEY_KP1 = 0x4F;
    KM_CORE_VKEY_NP2,      // KEY_KP2 = 0x50;
    KM_CORE_VKEY_NP3,      // KEY_KP3 = 0x51;
    KM_CORE_VKEY_NP0,      // KEY_KP0 = 0x52;
    KM_CORE_VKEY_NPDOT,    // KEY_KPDOT = 0x53;
    0,                    // padding 0x54;
    0,                    // KEY_ZENKAKUHANKAKU = 0x55;
    KM_CORE_VKEY_oE2,      // KEY_102ND = 0x56;
    KM_CORE_VKEY_F11,      // KEY_F11 = 0x57;
    KM_CORE_VKEY_F12,      // KEY_F12 = 0x58;
    0,                    // KEY_RO = 0x59;
    0,                    // KEY_KATAKANA = 0x5a;
    0,                    // KEY_HIRAGANA = 0x5b;
    0,                    // KEY_HENKAN = 0x5c;
    0,                    // KEY_KATAKANAHIRAGANA = 0x5d;
    0,                    // KEY_MUHENKAN = 0x5e;
    0,                    // KEY_KPJPCOMMA = 0x5f;
    0,                    // KEY_KPENTER = 0x60;
    KM_CORE_VKEY_CONTROL,  // KEY_RIGHTCTRL = 0x61;
    0,                    // KEY_KPSLASH = 0x62;
    0,                    // KEY_SYSRQ = 0x63;
    KM_CORE_VKEY_ALT       // KEY_RIGHTALT = 0x64;

    // Many more KEYS currently not used by KMW...
};

#endif // __KEYCODES_H__
