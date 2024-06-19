/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */
package com.keyman.engine.util;

import android.content.Context;
import android.content.ContextWrapper;
import android.content.res.Configuration;
import android.content.res.Resources;
import android.os.Build;
import android.os.LocaleList;

import java.util.Locale;

/**
 *  Class to programmatically change the application locale (#4236)
 *  References
 *  Tutorial from https://lokalise.com/blog/android-app-localization/#Change_Application_Locale_Programmatically
 *  https://github.com/nirm2009/lokalise/tree/master/android-i18n
 *  https://stackoverflow.com/questions/39705739/android-n-change-language-programmatically
 */
public class ContextUtils extends ContextWrapper {
  public ContextUtils(Context base) {
    super(base);
  }

  public static ContextWrapper updateLocale(Context context, Locale localeToSwitchTo) {
    Resources resources = context.getResources();
    Configuration configuration = resources.getConfiguration();

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
      LocaleList localeList = new LocaleList(localeToSwitchTo);
      LocaleList.setDefault(localeList);
      configuration.setLocales(localeList);
    } else {
      configuration.setLocale(localeToSwitchTo);
    }

    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N_MR1) {
      context = context.createConfigurationContext(configuration);
    } else {
      resources.updateConfiguration(configuration, resources.getDisplayMetrics());
    }

    return new ContextUtils(context);
  }

}
