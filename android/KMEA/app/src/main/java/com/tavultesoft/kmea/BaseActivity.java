/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import android.content.Context;
import android.content.ContextWrapper;
import android.content.SharedPreferences;
import android.os.Build;
import android.os.LocaleList;

import androidx.appcompat.app.AppCompatActivity;
import androidx.preference.PreferenceManager;

import com.tavultesoft.kmea.util.ContextUtils;

import java.util.Locale;

public class BaseActivity extends AppCompatActivity {

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
    ContextWrapper localeUpdatedContext = ContextUtils.updateLocale(newBase, localeToSwitchTo);
    super.attachBaseContext(localeUpdatedContext);
  }

}
