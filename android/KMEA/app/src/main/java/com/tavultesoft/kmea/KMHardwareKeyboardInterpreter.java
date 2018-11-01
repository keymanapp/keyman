/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import android.content.Context;
import android.view.KeyEvent;

public class KMHardwareKeyboardInterpreter implements KeyEvent.Callback {

  /**
   * Maps relevant keycodes from android.view.keyevent to the standard keycodes used by Keyman
   * keyboards.  Is conveniently based at 0, else we might need to HashMap it instead.
   */

  /*
  // Deprecated for scanCodeMap
  private static final
  int keyCodeMap[] = {
    0,      //        public static final int KEYCODE_UNKNOWN = 0;
    0,      //        public static final int KEYCODE_SOFT_LEFT = 1;
    0,      //        public static final int KEYCODE_SOFT_RIGHT = 2;
    0,      //        public static final int KEYCODE_HOME = 3;
    0,      //        public static final int KEYCODE_BACK = 4;
    0,      //        public static final int KEYCODE_CALL = 5;
    0,      //        public static final int KEYCODE_ENDCALL = 6;
    '0',    //        public static final int KEYCODE_0 = 7;
    '1',    //        public static final int KEYCODE_1 = 8;
    '2',    //        public static final int KEYCODE_2 = 9;
    '3',    //        public static final int KEYCODE_3 = 10;
    '4',    //        public static final int KEYCODE_4 = 11;
    '5',    //        public static final int KEYCODE_5 = 12;
    '6',    //        public static final int KEYCODE_6 = 13;
    '7',    //        public static final int KEYCODE_7 = 14;
    '8',    //        public static final int KEYCODE_8 = 15;
    '9',    //        public static final int KEYCODE_9 = 16;
    0,      //        public static final int KEYCODE_STAR = 17;
    0,      //        public static final int KEYCODE_POUND = 18;
    0,      //        public static final int KEYCODE_DPAD_UP = 19;
    0,      //        public static final int KEYCODE_DPAD_DOWN = 20;
    0,      //        public static final int KEYCODE_DPAD_LEFT = 21;
    0,      //        public static final int KEYCODE_DPAD_RIGHT = 22;
    0,      //        public static final int KEYCODE_DPAD_CENTER = 23;
    0,      //        public static final int KEYCODE_VOLUME_UP = 24;
    0,      //        public static final int KEYCODE_VOLUME_DOWN = 25;
    0,      //        public static final int KEYCODE_POWER = 26;
    0,      //        public static final int KEYCODE_CAMERA = 27;
    0,      //        public static final int KEYCODE_CLEAR = 28;
    'A',    //        public static final int KEYCODE_A = 29;
    'B',    //        public static final int KEYCODE_B = 30;
    'C',    //        public static final int KEYCODE_C = 31;
    'D',    //        public static final int KEYCODE_D = 32;
    'E',    //        public static final int KEYCODE_E = 33;
    'F',    //        public static final int KEYCODE_F = 34;
    'G',    //        public static final int KEYCODE_G = 35;
    'H',    //        public static final int KEYCODE_H = 36;
    'I',    //        public static final int KEYCODE_I = 37;
    'J',    //        public static final int KEYCODE_J = 38;
    'K',    //        public static final int KEYCODE_K = 39;
    'L',    //        public static final int KEYCODE_L = 40;
    'M',    //        public static final int KEYCODE_M = 41;
    'N',    //        public static final int KEYCODE_N = 42;
    'O',    //        public static final int KEYCODE_O = 43;
    'P',    //        public static final int KEYCODE_P = 44;
    'Q',    //        public static final int KEYCODE_Q = 45;
    'R',    //        public static final int KEYCODE_R = 46;
    'S',    //        public static final int KEYCODE_S = 47;
    'T',    //        public static final int KEYCODE_T = 48;
    'U',    //        public static final int KEYCODE_U = 49;
    'V',    //        public static final int KEYCODE_V = 50;
    'W',    //        public static final int KEYCODE_W = 51;
    'X',    //        public static final int KEYCODE_X = 52;
    'Y',    //        public static final int KEYCODE_Y = 53;
    'Z',    //        public static final int KEYCODE_Z = 54;
    188,    //        public static final int KEYCODE_COMMA = 55;
    190,    //        public static final int KEYCODE_PERIOD = 56;
    0,      //        public static final int KEYCODE_ALT_LEFT = 57;
    0,      //        public static final int KEYCODE_ALT_RIGHT = 58;
    0,      //        public static final int KEYCODE_SHIFT_LEFT = 59;
    0,      //        public static final int KEYCODE_SHIFT_RIGHT = 60;
    0,      //        public static final int KEYCODE_TAB = 61;
    32,     //        public static final int KEYCODE_SPACE = 62;
    0,      //        public static final int KEYCODE_SYM = 63;
    0,      //        public static final int KEYCODE_EXPLORER = 64;
    0,      //        public static final int KEYCODE_ENVELOPE = 65;
    13,     //        public static final int KEYCODE_ENTER = 66;
    0,      //        public static final int KEYCODE_DEL = 67;
    192,    //        public static final int KEYCODE_GRAVE = 68;
    189,    //        public static final int KEYCODE_MINUS = 69;
    187,    //        public static final int KEYCODE_EQUALS = 70;
    219,    //        public static final int KEYCODE_LEFT_BRACKET = 71;
    221,    //        public static final int KEYCODE_RIGHT_BRACKET = 72;
    220,    //        public static final int KEYCODE_BACKSLASH = 73;
    186,    //        public static final int KEYCODE_SEMICOLON = 74;
    222,    //        public static final int KEYCODE_APOSTROPHE = 75;
    191,    //        public static final int KEYCODE_SLASH = 76;
    0,      //        public static final int KEYCODE_AT = 77;
    0,      //        public static final int KEYCODE_NUM = 78;
    0,      //        public static final int KEYCODE_HEADSETHOOK = 79;
    0,      //        public static final int KEYCODE_FOCUS = 80;
    0,      //        public static final int KEYCODE_PLUS = 81;
    0,      //        public static final int KEYCODE_MENU = 82;
    0,      //        public static final int KEYCODE_NOTIFICATION = 83;
    0       //        public static final int KEYCODE_SEARCH = 84;
  };
*/

  /**
   * From reference: https://source.android.com/devices/input#understanding-hid-usages-and-event-codes
   * The Android API sometimes refers to the Linux key code associated with a key as its "scan code".
   * This is technically incorrect in but it helps to distinguish Linux key codes from Android
   * key codes in the API.
   *
   * Maps relevant Linux key code (LKC) to the standard key codes used by Keyman keyboards
   * Table: https://source.android.com/devices/input/keyboard-devices
   *
   * Limitations: Intentionally not assigning number pad keys
   */
  private static final
  int scanCodeMap[] = {
    0,      //        padding = 0x00;
    0,      //        public static final int KEY_ESC = 0x01;
    '1',    //        public static final int KEY_1 = 0x02;
    '2',    //        public static final int KEY_2 = 0x03;
    '3',    //        public static final int KEY_3 = 0x04;
    '4',    //        public static final int KEY_4 = 0x05;
    '5',    //        public static final int KEY_5 = 0x06;
    '6',    //        public static final int KEY_6 = 0x07;
    '7',    //        public static final int KEY_7 = 0x08;
    '8',    //        public static final int KEY_8 = 0x09;
    '9',    //        public static final int KEY_9 = 0x0A;
    '0',    //        public static final int KEY_0 = 0x0B;
    189,    //        public static final int KEY_MINUS = 0x0C;
    187,    //        public static final int KEY_EQUALS = 0x0D;
    0,      //        public static final int KEY_BACKSPACE = 0x0E;
    0,      //        public static final int KEY_TAB = 0x0F;
    'Q',    //        public static final int KEY_Q = 0x10;
    'W',    //        public static final int KEY_W = 0x11;
    'E',    //        public static final int KEY_E = 0x12;
    'R',    //        public static final int KEY_R = 0x13;
    'T',    //        public static final int KEY_T = 0x14;
    'Y',    //        public static final int KEY_Y = 0x15;
    'U',    //        public static final int KEY_U = 0x16;
    'I',    //        public static final int KEY_I = 0x17;
    'O',    //        public static final int KEY_O = 0x18;
    'P',    //        public static final int KEY_P = 0x19;
    219,    //        public static final int KEY_LEFTBRACE = 0x1A;
    221,    //        public static final int KEY_RIGHTBRACE = 0x1B;
    13,     //        public static final int KEY_ENTER = 0x1C;
    0,      //        public static final int KEY_LEFTCTRL = 0x1D;
    'A',    //        public static final int KEY_A = 0x1E;
    'S',    //        public static final int KEY_S = 0x1F;
    'D',    //        public static final int KEY_D = 0x20;
    'F',    //        public static final int KEY_F = 0x21;
    'G',    //        public static final int KEY_G = 0x22;
    'H',    //        public static final int KEY_H = 0x23;
    'J',    //        public static final int KEY_J = 0x24;
    'K',    //        public static final int KEY_K = 0x25;
    'L',    //        public static final int KEY_L = 0x26;
    186,    //        public static final int KEY_SEMICOLON = 0x27;
    222,    //        public static final int KEY_APOSTROPHE = 0x28;
    192,    //        public static final int KEY_GRAVE = 0x29;
    0,      //        public static final int KEY_LEFTSHIFT = 0x2A;
    220,    //        public static final int KEY_BACKSLASH = 0x2B;
    'Z',    //        public static final int KEY_Z = 0x2C;
    'X',    //        public static final int KEY_X = 0x2D;
    'C',    //        public static final int KEY_C = 0x2E;
    'V',    //        public static final int KEY_V = 0x2F;
    'B',    //        public static final int KEY_B = 0x30;
    'N',    //        public static final int KEY_N = 0x31;
    'M',    //        public static final int KEY_M = 0x32;
    188,    //        public static final int KEY_COMMA = 0x33;
    190,    //        public static final int KEY_DOT = 0x34;
    191,    //        public static final int KEY_SLASH = 0x35;
    0,      //        public static final int KEY_RIGHTSHIFT = 0x36;
    0,      //        public static final int KEY_KPASTERISK = 0x37;
    0,      //        public static final int KEY_LEFTALT = 0x38;
    32,     //        public static final int KEY_SPACE = 0x39;
    0,      //        public static final int KEY_CAPSLOCK = 0x3A;
    0,      //        public static final int KEY_F1 = 0x3B;
    0,      //        public static final int KEY_F2 = 0x3C;
    0,      //        public static final int KEY_F3 = 0x3D;
    0,      //        public static final int KEY_F4 = 0x3E;
    0,      //        public static final int KEY_F5 = 0x3F;
    0,      //        public static final int KEY_F6 = 0x40;
    0,      //        public static final int KEY_F7 = 0x41;
    0,      //        public static final int KEY_F8 = 0x42;
    0,      //        public static final int KEY_F9 = 0x43;
    0,      //        public static final int KEY_F10 = 0x44;
    0,      //        public static final int KEY_NUMLOCK = 0x45;
    0,      //        public static final int KEY_SCROLLLOCK = 0x46;
    0,      //        public static final int KEY_KP7 = 0x47;
    0,      //        public static final int KEY_KP8 = 0x48;
    0,      //        public static final int KEY_KP9 = 0x49;
    0,      //        public static final int KEY_KPMINUS = 0x4A;
    0,      //        public static final int KEY_KP4 = 0x4B;
    0,      //        public static final int KEY_KP5 = 0x4C;
    0,      //        public static final int KEY_KP6 = 0x4D;
    0,      //        public static final int KEY_KPPLUS = 0x4E;
    0,      //        public static final int KEY_KP1 = 0x4F;
    0,      //        public static final int KEY_KP2 = 0x50;
    0,      //        public static final int KEY_KP3 = 0x51;
    0,      //        public static final int KEY_KP0 = 0x52;
    0,      //        public static final int KEY_KPDOT = 0x53;
    0,      //        padding 0x54;
    0,      //        public static final int KEY_ZENKAKUHANKAKU = 0x55;
    226,    //        public static final int KEY_102ND = 0x56;
    0,      //        public static final int KEY_F11 = 0x57;
    0,      //        public static final int KEY_F12 = 0x58;
    0,      //        public static final int KEY_RO =  0x59;
    0,      //        public static final int KEY_KATAKANA =  0x5A;
    0,      //        public static final int KEY_HIRGANA =  0x5B;
    0,      //        public static final int KEY_HENKAN =  0x5C;
    0,      //        public static final int KEY_KATAKANAHIRAGANA =  0x5D;
    0,      //        public static final int KEY_MUHENKAN =  0x5E;
    0,      //        public static final int KEY_KPJPCOMMA =  0x5F;
    0,      //        public static final int KEY_KPENTER = 0x60;
    0,      //        public static final int KEY_RIGHTCTRL = 0x61;
    0,      //        public static final int KEY_KPSLASH = 0x62;
    0,      //        public static final int KEY_SYSRQ = 0x63;
    0,      //        public static final int KEY_RIGHTALT = 0x64;
    0,      //        padding 0x55;
    0,      //        public static final int KEY_HOME = 0x66;
    0,      //        public static final int KEY_UP = 0x67;
    0,      //        public static final int KEY_PAGEUP = 0x68;
    0,      //        public static final int KEY_LEFT = 0x69;
    0,      //        public static final int KEY_RIGHT = 0x6A;
    0,      //        public static final int KEY_END = 0x6B;
    0,      //        public static final int KEY_DOWN = 0x6C;
    0       //        public static final int KEY_PAGEDOWN = 0x6D;

    // Many more KEYS currently not used by KMW...
  };

  private final Context context;
  private final KMManager.KeyboardType keyboardType;

  public KMHardwareKeyboardInterpreter(Context context, KMManager.KeyboardType keyboardType) {
    this.context = context;
    this.keyboardType = keyboardType;
  }

  @Override
  public boolean onKeyDown(int keyCode, KeyEvent event) {

    if (keyCode > KeyEvent.KEYCODE_SEARCH || keyCode < KeyEvent.KEYCODE_UNKNOWN) {
      // The key is outside the range of keys we understand
      return false;
    }

    boolean isChiral = KMManager.getKMKeyboard(this.keyboardType).getChirality();

    // States of modifier keys
    // KeyEvent.getModifiers() specifically masks out lock keys (KeyEvent.META_CAPS_LOCK_ON,
    // KeyEvent.META_SCROLL_LOCK_ON, KeyEvent.META_NUM_LOCK_ON), so get their states separately
    int androidModifiers = event.getModifiers(), keymanModifiers = 0;
    boolean capsOn = event.isCapsLockOn();
    boolean numOn = event.isNumLockOn();
    boolean scrollOn = event.isScrollLockOn();

    // By design, SHIFT is non-chiral
    if ((androidModifiers & KeyEvent.META_SHIFT_ON) != 0) {
      keymanModifiers |= KMModifierCodes.get("SHIFT");
    }
    if ((androidModifiers & KeyEvent.META_CTRL_LEFT_ON) != 0) {
      keymanModifiers |= isChiral ? KMModifierCodes.get("LCTRL") : KMModifierCodes.get("CTRL");
    }
    if ((androidModifiers & KeyEvent.META_CTRL_RIGHT_ON) != 0) {
      keymanModifiers |= isChiral ? KMModifierCodes.get("RCTRL") : KMModifierCodes.get("CTRL");
    }
    if ((androidModifiers & KeyEvent.META_ALT_LEFT_ON) != 0) {
      keymanModifiers |= isChiral ? KMModifierCodes.get("LALT") : KMModifierCodes.get("ALT");
    }
    if ((androidModifiers & KeyEvent.META_ALT_RIGHT_ON) != 0) {
      keymanModifiers |= isChiral ? KMModifierCodes.get("RALT") : KMModifierCodes.get("ALT");
    }

    int Lstates = 0;
    Lstates |= capsOn ? KMModifierCodes.get("CAPS") : KMModifierCodes.get("NO_CAPS");
    Lstates |= numOn ? KMModifierCodes.get("NUM_LOCK") : KMModifierCodes.get("NO_NUM_LOCK");
    Lstates |= scrollOn ? KMModifierCodes.get("SCROLL_LOCK") : KMModifierCodes.get("NO_SCROLL_LOCK");

    // CTRL-Tab triggers the Keyman language menu
    if (keyCode == KeyEvent.KEYCODE_TAB && ((androidModifiers & KeyEvent.META_CTRL_ON) != 0)) {
      KMManager.showKeyboardPicker(context, keyboardType);
      return false;
    }

    // Range check scanCode. If it's beyond our expected range, just use "0"
    int scanCode = event.getScanCode();
    int code = (scanCode < scanCodeMap.length) ? scanCodeMap[scanCode] : 0 ;
    if (code == 0) {
      // Not an alphanumeric, punctuation or enter/tab/space key
      return false;
    }

    // Send keystroke to KeymanWeb for processing: will return true to swallow the keystroke
    return KMManager.executeHardwareKeystroke(code, keymanModifiers, keyboardType, Lstates);
  }

  @Override
  public boolean onKeyUp(int keyCode, KeyEvent event) {
    return false; // Lets other event handlers process their stuff.
  }

  @Override
  public boolean onKeyMultiple(int keyCode, int count, KeyEvent event) {
    return false; // Lets other event handlers process their stuff.
  }

  @Override
  public boolean onKeyLongPress(int keyCode, KeyEvent event) {
    return false; // Lets other event handlers process their stuff.
  }
}
