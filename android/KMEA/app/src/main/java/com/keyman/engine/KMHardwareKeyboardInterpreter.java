/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.keyman.engine;

import com.keyman.engine.KMScanCodeMap;
import android.content.Context;
import android.view.KeyEvent;

public class KMHardwareKeyboardInterpreter implements KeyEvent.Callback {



  private final Context context;
  private final KMManager.KeyboardType keyboardType;

  public KMHardwareKeyboardInterpreter(Context context, KMManager.KeyboardType keyboardType) {
    this.context = context;
    this.keyboardType = keyboardType;
  }

  @Override
  public boolean onKeyDown(int keyCode, KeyEvent event) {

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
    int code = (scanCode >= 0 && scanCode < KMScanCodeMap.scanCodeMap.length) ? KMScanCodeMap.scanCodeMap[scanCode] : 0 ;
    if (code == 0) {
      // Not an alphanumeric, punctuation or enter/tab/space key
      return false;
    }

    // Send keystroke to KeymanWeb for processing: will return true to swallow the keystroke
    return KMManager.executeHardwareKeystroke(code, keymanModifiers, keyboardType, Lstates, androidModifiers);
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
