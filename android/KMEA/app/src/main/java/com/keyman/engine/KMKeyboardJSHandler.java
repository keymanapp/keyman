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
