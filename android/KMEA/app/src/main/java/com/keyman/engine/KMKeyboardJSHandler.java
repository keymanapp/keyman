package com.keyman.engine;

import android.content.Context;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.Handler;
import android.os.Looper;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.HapticFeedbackConstants;
import android.view.KeyEvent;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;
import android.view.inputmethod.InputConnection;
import android.webkit.JavascriptInterface;

import static android.content.Context.VIBRATOR_SERVICE;

import com.keyman.engine.KMManager.EnterModeType;
import com.keyman.engine.KMManager.KeyboardType;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.util.CharSequenceUtil;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMString;

public class KMKeyboardJSHandler {
  private Context context;
  private KMKeyboard k = null;
  private static int KM_VIBRATE_DURATION = 100; // milliseconds
  private static String TAG = "KMKeyboardJSHandler";

  KMKeyboardJSHandler(Context context, KMKeyboard k) {
    this.context = context;
    this.k = k;
  }

  // This annotation is required in Jelly Bean and later:
  @JavascriptInterface
  public String getDeviceType() {
    return context.getResources().getString(R.string.device_type);
  }

  // This annotation is required in Jelly Bean and later:
  @JavascriptInterface
  public int getDefaultBannerHeight() {
    return (int) context.getResources().getDimension(R.dimen.banner_height);
  }

  // Get the keyboard height
  // This annotation is required in Jelly Bean and later:
  @JavascriptInterface
  public int getKeyboardHeight() {
    int kbHeight = KMManager.getKeyboardHeight(context);
    return kbHeight;
  }

  // This annotation is required in Jelly Bean and later:
  @JavascriptInterface
  public int getKeyboardWidth() {
    DisplayMetrics dms = context.getResources().getDisplayMetrics();
    int kbWidth = (int) (dms.widthPixels / dms.density);
    return kbWidth;
  }

  @JavascriptInterface
  public String initialKeyboard() {
    // Note:  KMManager.getCurrentKeyboard() (and similar) will throw errors until the host-page is first fully
    // loaded and has set a keyboard.  To allow the host-page to have earlier access, we instead get the stored
    // keyboard index directly.
    SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    Keyboard kbd = Keyboard.getDefaultKeyboard(context);
    int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, -1);
    if (index >= 0) {
      kbd = KMManager.getKeyboardInfo(this.context, index);
    }
    return kbd.toStub(context);
  }

  // This annotation is required in Jelly Bean and later:
  @JavascriptInterface
  public void beepKeyboard() {
    Vibrator v = (Vibrator) context.getSystemService(VIBRATOR_SERVICE);
    if (v != null && v.hasVibrator()) {
      if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
        VibrationEffect effect = VibrationEffect.createOneShot(KM_VIBRATE_DURATION, VibrationEffect.DEFAULT_AMPLITUDE);
        v.vibrate(effect);
      } else {
        v.vibrate(KM_VIBRATE_DURATION);
      }
    }
  }

  /**
   * Inserts the selected string <i>s</i>
   * @param dn  Number of pre-caret code points (UTF+8 characters) to delete
   * @param s   Text to insert
   * @param dr  Number of post-caret code points to delete.
   */
  @JavascriptInterface
  public void insertText(final int dn, final String s, final int dr, final boolean executingHardwareKeystroke) {
    Handler mainLoop = new Handler(Looper.getMainLooper());
    mainLoop.post(new Runnable() {
      public void run() {
        if (k == null) {
          KMLog.LogError(TAG, "insertText failed: Keyboard is null");
          return;
        }

        if (!isInappKMTextViewValid(k.keyboardType)) {
          return;
        }

        InputConnection ic = KMManager.getInputConnection(k.keyboardType);
        if (ic == null) {
          KMLog.LogError(TAG, "insertText failed: InputConnection is null");
          return;
        }

        ic.beginBatchEdit();

        int deleteLeft = dn;

        // Delete any existing selected text.
        ExtractedText icText = ic.getExtractedText(new ExtractedTextRequest(), 0);
        if (icText != null) { // This can be null if the input connection becomes invalid.
          int start = icText.startOffset + icText.selectionStart;
          int end = icText.startOffset + icText.selectionEnd;
          if (end < start) {
            // Swap start/end for backward selection
            int temp = start;
            start = end;
            end = temp;
          }
          if (end > start) {
            k.setShouldIgnoreSelectionChange(true);
            if (s.length() == 0) {
              ic.setSelection(start, start);
              ic.deleteSurroundingText(0, end - start);
              ic.endBatchEdit();
              return;
            } else {
              ic.setSelection(start, start);
              ic.deleteSurroundingText(0, end - start);
            }

            // KeymanWeb tells us how to delete the selection, but we don't
            // want to do that twice
            deleteLeft = 0;
          }
        }

        if (s.length() > 0 && s.charAt(0) == '\n') {
          if (k.keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
            // Special handling of ENTER key
            switch (KMManager.enterMode) {
              // Go action
              case GO :
                ic.performEditorAction(EditorInfo.IME_ACTION_GO);
                break;

              // Search action
              case SEARCH :
                ic.performEditorAction(EditorInfo.IME_ACTION_SEARCH);
                break;

              // Send action
              case SEND :
                ic.performEditorAction(EditorInfo.IME_ACTION_SEND);
                break;

              // Next action
              case NEXT :
                ic.performEditorAction(EditorInfo.IME_ACTION_NEXT);
                break;

              // Done action
              case DONE :
                ic.performEditorAction(EditorInfo.IME_ACTION_DONE);
                break;

              // Previous action
              case PREVIOUS :
                ic.performEditorAction(EditorInfo.IME_ACTION_PREVIOUS);
                break;

              // Messaging apps
              case NEWLINE :
                // Send newline and advance cursor
                ic.commitText("\n", 1);
                break;

              // Default ENTER action
              default:
                keyDownUp(KeyEvent.KEYCODE_ENTER, 0);
            }
          } else {
            // In-app keyboard uses default ENTER action
            keyDownUp(KeyEvent.KEYCODE_ENTER, 0);
          }
          ic.endBatchEdit();
          return;
        }

        // Perform left-deletions
        if (deleteLeft > 0) {
          if (k.keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
            k.setShouldIgnoreTextChange(true);
            k.setShouldIgnoreSelectionChange(true);
          }
          if (k.keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM &&
              icText != null && icText.selectionStart == 0 && icText.selectionEnd == 0) {
            // If cursor at the start of a line, let the app handle the BACKSPACE
            keyDownUp(KeyEvent.KEYCODE_DEL, 0);
          } else {
            performLeftDeletions(ic, deleteLeft);
          }
        }

        // Perform right-deletions
        for (int i = 0; i < dr; i++) {
          CharSequence chars = ic.getTextAfterCursor(1, 0);
          if (chars != null && chars.length() > 0) {
            char c = chars.charAt(0);
            k.setShouldIgnoreSelectionChange(true);
            if (Character.isHighSurrogate(c)) {
              ic.deleteSurroundingText(0, 2);
            } else {
              ic.deleteSurroundingText(0, 1);
            }
          }
        }

        if (s.length() > 0) {
          k.setShouldIgnoreSelectionChange(true);
          // Commit the string s. Use newCursorPosition 1 so cursor will end up after the string.
          ic.commitText(s, 1);
        }

        k.dismissHelpBubble();
        k.setShouldShowHelpBubble(false);

        ic.endBatchEdit();
        ViewGroup parent = (ViewGroup) k.getParent();
        if (parent != null && KMManager.getHapticFeedback() && !executingHardwareKeystroke) {
          parent.performHapticFeedback(HapticFeedbackConstants.VIRTUAL_KEY, HapticFeedbackConstants.FLAG_IGNORE_GLOBAL_SETTING);
        }
      }
    });
  }

  // Store the current keyboard chirality status from KMW in the Keyboard
  @JavascriptInterface
  public void setIsChiral(boolean isChiral) {
    if (k != null) {
      k.setChirality(isChiral);
    }
  }

  @JavascriptInterface
  public boolean dispatchKey(final int code, final int eventModifiers) {
    Handler mainLoop = new Handler(Looper.getMainLooper());
    mainLoop.post(new Runnable() {
      public void run() {
        if (k == null) {
          KMLog.LogError(TAG, "dispatchKey failed: Keyboard is null");
          return;
        }

        if (!isInappKMTextViewValid(k.keyboardType)) {
          return;
        }

        InputConnection ic = KMManager.getInputConnection(k.keyboardType);
        if (ic == null) {
          KMLog.LogError(TAG, "dispatchKey failed: InputConnection is null");
          return;
        }

        k.dismissHelpBubble();
        k.setShouldShowHelpBubble(false);

        // Handle tab or enter since KMW didn't process it
        if (KMManager.isDebugMode()) {
          Log.d(TAG, "dispatchKey called with code: " + code + ", eventModifiers: " + eventModifiers);
        }
        if (code == KMScanCodeMap.scanCodeMap[KMScanCodeMap.KEY_TAB]) {
          keyDownUp(KeyEvent.KEYCODE_TAB, eventModifiers);
        } else if (code == KMScanCodeMap.scanCodeMap[KMScanCodeMap.KEY_ENTER]) {
          keyDownUp(KeyEvent.KEYCODE_ENTER, eventModifiers);
        }
      }
    });
    return true;
  }

  private void keyDownUp(int keyEventCode, int eventModifiers) {
    if (k.keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      KMTextView textView = (KMTextView)KMTextView.activeView;
      if (keyEventCode == KeyEvent.KEYCODE_TAB) {
        KeyEvent event = new KeyEvent(0, 0, 0, KeyEvent.KEYCODE_TAB, 0, eventModifiers, 0, 0, 0);
        textView.dispatchKeyEvent(event);
      } else {
        textView.keyDownUp(keyEventCode);
      }
    } else if (k.keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      InputConnection ic = KMManager.getInputConnection(KeyboardType.KEYBOARD_TYPE_SYSTEM);
      ic.sendKeyEvent(new KeyEvent(KeyEvent.ACTION_DOWN, keyEventCode));
      ic.sendKeyEvent(new KeyEvent(KeyEvent.ACTION_UP, keyEventCode));
    }
  }

  /*
  // Chromium up until version M81 had a bug where deleteSurroundingText deletes an entire
  // grapheme cluster instead of one code-point. See Chromium issue #1024738
  // https://bugs.chromium.org/p/chromium/issues/detail?id=1024738
  //
  // We'll retrieve up to (dn*2+16) characters before the cursor to collect enough characters
  // for surrogate pairs + a long grapheme cluster.
  // This buffer will be used to put back characters as-needed
  */
  private static void performLeftDeletions(InputConnection ic, int dn) {
    int originalBufferLength = dn*2 + 16; // characters
    CharSequence charsBackup = getCharacterSequence(ic, originalBufferLength);

    int lastIndex = charsBackup.length()-1;

    // Exit if there's no context to delete
    if (lastIndex < 0) {
      return;
    }

    // Count the number of characters which are surrogate pairs
    int numPairs = CharSequenceUtil.countSurrogatePairs(charsBackup, dn);

    // Chop dn+numPairs code points from the end of charsBackup
    // subSequence indices are start(inclusive) to end(exclusive)
    int start = 0;
    int end = charsBackup.length() - (dn + numPairs);
    CharSequence expectedChars;
    try {
      expectedChars = charsBackup.subSequence(start, end);
    } catch (IndexOutOfBoundsException e) {
      KMLog.LogException(TAG,
        KMString.format("Bad subSequence of start %d, end is %d, length %d, dn %d, numPairs %d",
        start, end, charsBackup.length(), dn, numPairs), e);
      expectedChars = "";
    }
    ic.deleteSurroundingText(dn + numPairs, 0);
    // Shorten the retrieved context by exactly as many characters as were just deleted.
    CharSequence newContext = getCharacterSequence(ic, originalBufferLength - dn - numPairs);

    CharSequence charsToRestore = CharSequenceUtil.restoreChars(expectedChars, newContext);
    if (charsToRestore.length() > 0) {
      // Restore expectedChars that Chromium deleted.
      // Use newCursorPosition 1 so cursor will be after the inserted string
      ic.commitText(charsToRestore, 1);
    }
  }

  /**
   * Get a character sequence from the InputConnection.
   * Sometimes the WebView can split a surrogate pair at either end,
   * so chop that and update the cursor
   * @param ic - the InputConnection
   * @param length - number of characters to get
   * @return CharSequence
   */
  private static CharSequence getCharacterSequence(InputConnection ic, int length) {
    if (ic == null || length <= 0) {
      return "";
    }

    CharSequence sequence = ic.getTextBeforeCursor(length, 0);
    if (sequence == null || sequence.length() <= 0) {
      return "";
    }

    // Move the cursor back if there's a split surrogate pair
    if (Character.isHighSurrogate(sequence.charAt(sequence.length()-1))) {
      ic.commitText("", -1);
      sequence = ic.getTextBeforeCursor(length, 0);
    }

    if (sequence == null || sequence.length() <= 0) {
      return "";
    }

    if (Character.isLowSurrogate(sequence.charAt(0))) {
      // Adjust if the first char is also a split surrogate pair
      // subSequence indices are start(inclusive) to end(exclusive)
      int start = 1;
      int end = sequence.length();
      try {
        sequence = sequence.subSequence(start, end);
      } catch (IndexOutOfBoundsException e) {
        KMLog.LogException(TAG,
          KMString.format("Bad subSequence of start %d, end is %d",
          start, end), e);
      }      
    }

    return sequence;
  }

  /**
   * If the keyboard type is KEYBOARD_TYPE_INAPP, check if the KMTextView is valid.
   * For KEYBOARD_TYPE_SYSTEM, returns true.
   * @param keyboardType
   * @return boolean - false if keyboard type is INAPP and KMTextView is invalid. Otherwise true
   */
  private static boolean isInappKMTextViewValid(KeyboardType keyboardType) {
    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (KMTextView.activeView == null) {
        if (KMManager.isDebugMode()) {
          Log.w(TAG, "activeView is null");
        }
        return false;
      }
      if (KMTextView.activeView.getClass() != KMTextView.class) {
        return false;
      }
    }

    return true;
  }
}
