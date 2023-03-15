package com.keyman.engine;

import android.content.Context;
import android.os.Build;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.util.DisplayMetrics;
import android.util.Log;
import android.webkit.JavascriptInterface;
import android.widget.RelativeLayout;

import static android.content.Context.VIBRATOR_SERVICE;

public abstract class KMKeyboardJSHandler {
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
        if (SystemKeyboard == null) {
          KMLog.LogError(TAG, "insertText failed: SystemKeyboard is null");
          return;
        }

        if (SystemKeyboard.subKeysWindow != null) {
          return;
        }

        InputConnection ic = IMService.getCurrentInputConnection();
        if (ic == null) {
          if (isDebugMode()) {
            Log.w(HANDLER_TAG, "insertText failed: InputConnection is null");
          }
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
            if (s.length() == 0) {
              ic.setSelection(start, start);
              ic.deleteSurroundingText(0, end - start);
              ic.endBatchEdit();
              return;
            } else {
              SystemKeyboardShouldIgnoreSelectionChange = true;
              ic.setSelection(start, start);
              ic.deleteSurroundingText(0, end - start);
            }

            // KeymanWeb tells us how to delete the selection, but we don't
            // want to do that twice
            deleteLeft = 0;
          }
        }

        if (s.length() > 0 && s.charAt(0) == '\n') {
          keyDownUp(KeyEvent.KEYCODE_ENTER);
          ic.endBatchEdit();
          return;
        }

        // Perform left-deletions
        if (deleteLeft > 0) {
          performLeftDeletions(ic, deleteLeft);
        }

        // Perform right-deletions
        for (int i = 0; i < dr; i++) {
          CharSequence chars = ic.getTextAfterCursor(1, 0);
          if (chars != null && chars.length() > 0) {
            char c = chars.charAt(0);
            SystemKeyboardShouldIgnoreSelectionChange = true;
            if (Character.isHighSurrogate(c)) {
              ic.deleteSurroundingText(0, 2);
            } else {
              ic.deleteSurroundingText(0, 1);
            }
          }
        }

        if (s.length() > 0) {
          SystemKeyboardShouldIgnoreSelectionChange = true;

          // Commit the string s. Use newCursorPosition 1 so cursor will end up after the string.
          ic.commitText(s, 1);
        }

        SystemKeyboard.dismissHelpBubble();
        SystemKeyboard.setShouldShowHelpBubble(false);

        ic.endBatchEdit();
        ViewGroup parent = (ViewGroup) SystemKeyboard.getParent();
        if (parent != null && mayHaveHapticFeedback && !executingHardwareKeystroke) {
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
  public  abstract boolean dispatchKey(final int code, final int eventModifiers);

  /**
   * Inserts the selected string <i>s</i>
   * @param dn  Number of pre-caret code points (UTF+8 characters) to delete
   * @param s   Text to insert
   * @param dr  Number of post-caret code points to delete.
   */
  @JavascriptInterface
  public abstract void insertText(final int dn, final String s, final int dr, final boolean executingHardwareKeystroke);
}
