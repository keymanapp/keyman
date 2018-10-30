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
    220,    //        public static final int KEYCODE_BACKSLASH = 73; Note special case below
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

  // Special case for Android US layout overloading AZERTY K_oE2 (102nd key)
  // Use scan code to distinguish from KEYCODE_BACKSLASH
  // Reference: https://source.android.com/devices/input/keyboard-devices
  private final int SCAN_CODE_KEY_102ND = 86;
  private final int CODE_KEY_102ND = 226; // code KMW expects for K_oE2

  private final Context context;
  private final KMManager.KeyboardType keyboardType;

  public KMHardwareKeyboardInterpreter(Context context, KMManager.KeyboardType keyboardType) {
    this.context = context;
    this.keyboardType = keyboardType;
  }

  @Override
  public boolean onKeyDown(int keyCode, KeyEvent event) {

    if (keyCode > 84 || keyCode < 0) {
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

    if (keyCodeMap[keyCode] == 0) {
      // Not an alphanumeric, punctuation or enter/tab/space key
      return false;
    }

    int code;
    if ((keyCode == KeyEvent.KEYCODE_BACKSLASH ) && (event.getScanCode() == SCAN_CODE_KEY_102ND) ) {
      code = CODE_KEY_102ND;
    } else {
      code = keyCodeMap[keyCode];
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
