/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.keyman.engine;

import android.content.Context;
import android.content.ContextWrapper;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.LocaleList;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.widget.Toast;

import androidx.activity.EdgeToEdge;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsControllerCompat;
import androidx.preference.PreferenceManager;

import com.keyman.engine.util.ContextUtils;
import com.keyman.engine.util.KMLog;

import java.util.Locale;

public class BaseActivity extends AppCompatActivity {
  private static final String TAG = "BaseActivity";

  static ContextWrapper localeUpdatedContext;

  /**
   * Some classes aren't an AppCompatActivity and need this helper to localize Toast notifications
   * in the updated locale.
   * @param defaultContext - the context to fallback if localeUpdatedContext is null
   * @param resID - resource ID of the string
   * @param duration - length of the Toast notification (Toast.LENGTH_LONG or Toast.LENGTH_SHORT)
   * @param args - optional format parameters for the string
   */
  public static void makeToast(Context defaultContext, int resID, int duration, Object... args) {
    Context context = (localeUpdatedContext != null) ? localeUpdatedContext : defaultContext;
    String msg = context.getString(resID);
    Toast.makeText(context, String.format(msg, args), duration).show();
  }

  /**
   * Some classes aren't an AppCompatActivity and need this helper to send Toast notifications
   * @param defaultContext - the context to fallback if localeUpdatedContext is null
   * @param msg - Toast notification string
   * @param duration - length of the Toast notification (Toast.LENGTH_LONG or Toast.LENGTH_SHORT)
   */
  public static void makeToast(Context defaultContext, String msg, int duration) {
    Context context = (localeUpdatedContext != null) ? localeUpdatedContext : defaultContext;
    if (context != null) {
      Toast.makeText(context, msg, duration).show();
    }
  }

  /**
   * Some classes aren't an AppCompatActivity and need this helper to retrieve localized Strings
   * in the updated locale.
   * @param defaultContext - the context to fallback if localUpdatedContext is null
   * @param resID - the resource ID of the string
   * @return String - localized string
   */
  public static String getString(Context defaultContext, int resID) {
    Context context = (localeUpdatedContext != null) ? localeUpdatedContext : defaultContext;
    if (context != null) {
      return context.getString(resID);
    };
    // Shouldn't be here
    KMLog.LogError(TAG, "context null for getString()");
    return "";
  }


  /**
   * Setup the activity for edge-to-edge
   * https://developer.android.com/develop/ui/views/layout/edge-to-edge-manually
   * https://stackoverflow.com/questions/57293449/go-edge-to-edge-on-android-correctly-with-windowinsets
   * @param layoutID - Top level ID of the view
   */
  public void setupEdgeToEdge(int layoutID) {
    EdgeToEdge.enable(this);
    View anchor = findViewById(layoutID);
    ViewCompat.setOnApplyWindowInsetsListener(anchor,
      (view, windowInsets) -> {
        // Allocate insets for system bars and display cutout (notch)
        Insets insets = windowInsets.getInsets(
          WindowInsetsCompat.Type.systemBars() | WindowInsetsCompat.Type.displayCutout());

        view.setPadding(
          insets.left,
          insets.top,
          insets.right,
          insets.bottom);

        KMManager.applyInsetsToKeyboard(
          KMManager.KeyboardType.KEYBOARD_TYPE_INAPP, insets.left, insets.right, insets.bottom);
        return windowInsets;
      });
  }

  /**
   * If Android API < 35, apply colors to the status bar and navigation bar
   * @param statusBarcolor - int value of the color to use on the status bar.
   * @param navigationBarColor - int value of the color to use on the navigation bar.
   */
  public void setupStatusBarColors(int statusBarColor, int navigationBarColor) {
    // Set status bar colors
    // https://stackoverflow.com/a/79706054
    Window window = getWindow();
    window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
    window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
      WindowInsetsControllerCompat windowInsetsController =
        WindowCompat.getInsetsController(window, window.getDecorView());
      windowInsetsController.setAppearanceLightStatusBars(true);

      if (Build.VERSION.SDK_INT < Build.VERSION_CODES.VANILLA_ICE_CREAM) {
        window.setStatusBarColor(getColor(statusBarColor));
        window.setNavigationBarColor(getColor(navigationBarColor));
      }
    }
  }

  @Override
  protected void attachBaseContext(Context newBase) {
    // Override the app locale using the BCP 47 tag from shared preferences
    // Using PreferenceManager because this is before onCreate()
    SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(newBase);
    String languageTag  = prefs.getString(DisplayLanguages.displayLanguageKey, DisplayLanguages.unspecifiedLocale);
    Locale localeToSwitchTo;
    if (languageTag == null || languageTag.equals(DisplayLanguages.unspecifiedLocale)) {
      // If display language not specified, use the default locale
      if (Build.VERSION.SDK_INT < Build.VERSION_CODES.N) {
        super.attachBaseContext(newBase);
        return;
      }
      localeToSwitchTo = LocaleList.getDefault().get(0);
    } else {
      localeToSwitchTo = Locale.forLanguageTag(languageTag);
    }
    this.localeUpdatedContext = ContextUtils.updateLocale(newBase, localeToSwitchTo);
    super.attachBaseContext(localeUpdatedContext);
  }

}
