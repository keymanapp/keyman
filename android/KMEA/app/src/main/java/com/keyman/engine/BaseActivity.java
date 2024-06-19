/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.keyman.engine;

import android.content.Context;
import android.content.ContextWrapper;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.LocaleList;
import android.widget.Toast;


import androidx.appcompat.app.AppCompatActivity;
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
