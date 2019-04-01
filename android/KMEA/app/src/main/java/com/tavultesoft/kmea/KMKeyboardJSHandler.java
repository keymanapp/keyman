package com.tavultesoft.kmea;

import android.content.Context;
import android.os.Build;
import android.os.VibrationEffect;
import android.os.Vibrator;
import android.util.DisplayMetrics;
import android.webkit.JavascriptInterface;

import static android.content.Context.VIBRATOR_SERVICE;

public abstract class KMKeyboardJSHandler {
  private Context context;
  private KMKeyboard k = null;
  private static int KM_VIBRATE_DURATION = 100; // milliseconds

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
  public int getBannerHeight() {
    int bannerHeight = context.getResources().getDimensionPixelSize(R.dimen.banner_height);
    //bannerHeight -= bannerHeight % 20;
    return bannerHeight;
  }

  // This annotation is required in Jelly Bean and later:
  @JavascriptInterface
  public int getKeyboardHeight() {
    int kbHeight = context.getResources().getDimensionPixelSize(R.dimen.keyboard_height);
    kbHeight -= kbHeight % 20;
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

  // Insert the selected string s
  @JavascriptInterface
  public abstract void insertText(final int dn, final String s);
}
