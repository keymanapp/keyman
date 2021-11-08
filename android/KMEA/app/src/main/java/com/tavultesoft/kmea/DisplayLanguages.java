/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import android.content.Context;

public class DisplayLanguages {
  private static final String TAG = "DisplayLanguages";
  public static final String displayLanguageKey = "DisplayLanguage";
  public static final String unspecifiedLocale = ""; // empty string for app to use device's default locale

  public static class DisplayLanguageType {
    String languageTag;
    String languageName;

    public DisplayLanguageType(String languageTag, String languageName) {
      this.languageTag = languageTag;
      this.languageName = languageName;
    }

    public String getLanguageTag() {
      return this.languageTag;
    }

    public String getLanguageName() {
      return this.languageName;
    }
  }

  // Display Language types (as named in translate.keyman.com)
  // Order doesn't matter since we're storing BCP-47 tags in the settings preference
  // Additional notes:
  // Java uses deprecated two-letter code "in" for Indonesian
  //
  // Spanish (Latin America) folder is b+es+419 but the locale uses es-419
  // Reference: https://developer.android.com/guide/topics/resources/multilingual-support#postN
  public static final DisplayLanguageType[] getDisplayLanguages(Context context) {
    DisplayLanguageType[] languages = {
      new DisplayLanguageType(unspecifiedLocale, context.getString(R.string.default_locale)),
      new DisplayLanguageType("am-ET", "አማርኛ (Amharic)"),
      new DisplayLanguageType("ar", "(Arabic) العربية"),
      new DisplayLanguageType("az-AZ", "Azərbaycanca (Azəricə)"),
      new DisplayLanguageType("en", "English"),
      new DisplayLanguageType("es-419", "Español (Spanish - Latin America)"),
      new DisplayLanguageType("fr-FR", "French"),
      new DisplayLanguageType("ha-HG", "Hausa"),
      new DisplayLanguageType("in-ID", "Indonesian"),
      new DisplayLanguageType("de-DE", "German"),
      new DisplayLanguageType("km-KH", "Khmer"),
      new DisplayLanguageType("ann", "Obolo"),
      new DisplayLanguageType("ff-ZA", "Pulaar-Fulfulde"), // or Fulah
      new DisplayLanguageType("shu-latn", "Shuwa (Latin)"),
      new DisplayLanguageType("zh-CN", "中文(简体) (Simplified Chinese)")
    };
    return languages;
  }
}
