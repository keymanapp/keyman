/*
 * Keyman Input Method for Fcitx
 *
 * Copyright (C) 2009-2020 SIL International
 * Copyright (C) 2021 Google LLC
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA
 *
 */
#ifndef _FCITX5_KMPDATA_H_
#define _FCITX5_KMPDATA_H_

#include <keyman/keyboardprocessor.h>

// from
// android/KMEA/app/src/main/java/com/tavultesoft/kmea/KMHardwareKeyboardInterpreter.java
// uses kernel keycodes which are X11 keycode - 8
// private static final
//  int scanCodeMap[] = {
static km_kbp_virtual_key const keycode_to_vk[256] = {
    0,                  //        padding = 0x00;
    KM_KBP_VKEY_ESC,    //        public static final int KEY_ESC = 0x01;
    KM_KBP_VKEY_1,      //        public static final int KEY_1 = 0x02;
    KM_KBP_VKEY_2,      //        public static final int KEY_2 = 0x03;
    KM_KBP_VKEY_3,      //        public static final int KEY_3 = 0x04;
    KM_KBP_VKEY_4,      //        public static final int KEY_4 = 0x05;
    KM_KBP_VKEY_5,      //        public static final int KEY_5 = 0x06;
    KM_KBP_VKEY_6,      //        public static final int KEY_6 = 0x07;
    KM_KBP_VKEY_7,      //        public static final int KEY_7 = 0x08;
    KM_KBP_VKEY_8,      //        public static final int KEY_8 = 0x09;
    KM_KBP_VKEY_9,      //        public static final int KEY_9 = 0x0A;
    KM_KBP_VKEY_0,      //        public static final int KEY_0 = 0x0B;
    KM_KBP_VKEY_HYPHEN, //        public static final int KEY_MINUS = 0x0C;
    KM_KBP_VKEY_EQUAL,  //        public static final int KEY_EQUALS = 0x0D;
    KM_KBP_VKEY_BKSP,   //        public static final int KEY_BACKSPACE = 0x0E;
    KM_KBP_VKEY_TAB,    //        public static final int KEY_TAB = 0x0F;
    KM_KBP_VKEY_Q,      //        public static final int KEY_Q = 0x10;
    KM_KBP_VKEY_W,      //        public static final int KEY_W = 0x11;
    KM_KBP_VKEY_E,      //        public static final int KEY_E = 0x12;
    KM_KBP_VKEY_R,      //        public static final int KEY_R = 0x13;
    KM_KBP_VKEY_T,      //        public static final int KEY_T = 0x14;
    KM_KBP_VKEY_Y,      //        public static final int KEY_Y = 0x15;
    KM_KBP_VKEY_U,      //        public static final int KEY_U = 0x16;
    KM_KBP_VKEY_I,      //        public static final int KEY_I = 0x17;
    KM_KBP_VKEY_O,      //        public static final int KEY_O = 0x18;
    KM_KBP_VKEY_P,      //        public static final int KEY_P = 0x19;
    KM_KBP_VKEY_LBRKT,  //        public static final int KEY_LEFTBRACE = 0x1A;
    KM_KBP_VKEY_RBRKT,  //        public static final int KEY_RIGHTBRACE = 0x1B;
    KM_KBP_VKEY_ENTER,  //        public static final int KEY_ENTER = 0x1C;
    0,                  //        public static final int KEY_LEFTCTRL = 0x1D;
    KM_KBP_VKEY_A,      //        public static final int KEY_A = 0x1E;
    KM_KBP_VKEY_S,      //        public static final int KEY_S = 0x1F;
    KM_KBP_VKEY_D,      //        public static final int KEY_D = 0x20;
    KM_KBP_VKEY_F,      //        public static final int KEY_F = 0x21;
    KM_KBP_VKEY_G,      //        public static final int KEY_G = 0x22;
    KM_KBP_VKEY_H,      //        public static final int KEY_H = 0x23;
    KM_KBP_VKEY_J,      //        public static final int KEY_J = 0x24;
    KM_KBP_VKEY_K,      //        public static final int KEY_K = 0x25;
    KM_KBP_VKEY_L,      //        public static final int KEY_L = 0x26;
    KM_KBP_VKEY_COLON,  //        public static final int KEY_SEMICOLON = 0x27;
    KM_KBP_VKEY_QUOTE,  //        public static final int KEY_APOSTROPHE = 0x28;
    KM_KBP_VKEY_BKQUOTE, //        public static final int KEY_GRAVE = 0x29;
    0,                   //        public static final int KEY_LEFTSHIFT = 0x2A;
    KM_KBP_VKEY_BKSLASH, //        public static final int KEY_BACKSLASH = 0x2B;
    KM_KBP_VKEY_Z,       //        public static final int KEY_Z = 0x2C;
    KM_KBP_VKEY_X,       //        public static final int KEY_X = 0x2D;
    KM_KBP_VKEY_C,       //        public static final int KEY_C = 0x2E;
    KM_KBP_VKEY_V,       //        public static final int KEY_V = 0x2F;
    KM_KBP_VKEY_B,       //        public static final int KEY_B = 0x30;
    KM_KBP_VKEY_N,       //        public static final int KEY_N = 0x31;
    KM_KBP_VKEY_M,       //        public static final int KEY_M = 0x32;
    KM_KBP_VKEY_COMMA,   //        public static final int KEY_COMMA = 0x33;
    KM_KBP_VKEY_PERIOD,  //        public static final int KEY_DOT = 0x34;
    KM_KBP_VKEY_SLASH,   //        public static final int KEY_SLASH = 0x35;
    0,                  //        public static final int KEY_RIGHTSHIFT = 0x36;
    KM_KBP_VKEY_NPSTAR, //        public static final int KEY_KPASTERISK = 0x37;
    0,                  //        public static final int KEY_LEFTALT = 0x38;
    KM_KBP_VKEY_SPACE,  //        public static final int KEY_SPACE = 0x39;
    0,                  //        public static final int KEY_CAPSLOCK = 0x3A;
    KM_KBP_VKEY_F1,     //        public static final int KEY_F1 = 0x3B;
    KM_KBP_VKEY_F2,     //        public static final int KEY_F2 = 0x3C;
    KM_KBP_VKEY_F3,     //        public static final int KEY_F3 = 0x3D;
    KM_KBP_VKEY_F4,     //        public static final int KEY_F4 = 0x3E;
    KM_KBP_VKEY_F5,     //        public static final int KEY_F5 = 0x3F;
    KM_KBP_VKEY_F6,     //        public static final int KEY_F6 = 0x40;
    KM_KBP_VKEY_F7,     //        public static final int KEY_F7 = 0x41;
    KM_KBP_VKEY_F8,     //        public static final int KEY_F8 = 0x42;
    KM_KBP_VKEY_F9,     //        public static final int KEY_F9 = 0x43;
    KM_KBP_VKEY_F10,    //        public static final int KEY_F10 = 0x44;
    0,                  //        public static final int KEY_NUMLOCK = 0x45;
    0,                  //        public static final int KEY_SCROLLLOCK = 0x46;
    KM_KBP_VKEY_NP7,    //        public static final int KEY_KP7 = 0x47;
    KM_KBP_VKEY_NP8,    //        public static final int KEY_KP8 = 0x48;
    KM_KBP_VKEY_NP9,    //        public static final int KEY_KP9 = 0x49;
    KM_KBP_VKEY_NPMINUS, //        public static final int KEY_KPMINUS = 0x4A;
    KM_KBP_VKEY_NP4,     //        public static final int KEY_KP4 = 0x4B;
    KM_KBP_VKEY_NP5,     //        public static final int KEY_KP5 = 0x4C;
    KM_KBP_VKEY_NP6,     //        public static final int KEY_KP6 = 0x4D;
    KM_KBP_VKEY_NPPLUS,  //        public static final int KEY_KPPLUS = 0x4E;
    KM_KBP_VKEY_NP1,     //        public static final int KEY_KP1 = 0x4F;
    KM_KBP_VKEY_NP2,     //        public static final int KEY_KP2 = 0x50;
    KM_KBP_VKEY_NP3,     //        public static final int KEY_KP3 = 0x51;
    KM_KBP_VKEY_NP0,     //        public static final int KEY_KP0 = 0x52;
    KM_KBP_VKEY_NPDOT,   //        public static final int KEY_KPDOT = 0x53;
    0,                   //        padding 0x54;
    0, //        public static final int KEY_ZENKAKUHANKAKU = 0x55;
    KM_KBP_VKEY_oE2, //        public static final int KEY_102ND = 0x56;
    // additional on linux
    KM_KBP_VKEY_F11, //        public static final int KEY_F11 = 0x57;
    KM_KBP_VKEY_F12  //        public static final int KEY_F12 = 0x58;

    // Many more KEYS currently not used by KMW...
};

#endif // _FCITX5_KMPDATA_H_
