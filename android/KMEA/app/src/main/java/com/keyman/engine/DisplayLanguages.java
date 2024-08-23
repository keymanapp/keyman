/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.keyman.engine;

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
      new DisplayLanguageType("ar-SA", "(Arabic) العربية"),
      new DisplayLanguageType("az-AZ", "Azərbaycanca (Azəricə)"),
      new DisplayLanguageType("bwr-NG", "Bura-Pabir"),
      new DisplayLanguageType("cs-CZ", "čeština (Czech)"),
      new DisplayLanguageType("ff-NG", "Ɗemngal Fulfulde"),
      new DisplayLanguageType("en", "English"),
      new DisplayLanguageType("es-ES", "Español (Spanish)"),
      new DisplayLanguageType("es-419", "Español (Spanish - Latin America)"),
      new DisplayLanguageType("fr-FR", "French"),
      new DisplayLanguageType("ha-HG", "Hausa"),
      new DisplayLanguageType("in-ID", "Indonesian"),
      new DisplayLanguageType("it-IT", "Italiana"),
      new DisplayLanguageType("de-DE", "German"),
      new DisplayLanguageType("kn-IN", "ಕನ್ನಡ (Kannada)"),
      new DisplayLanguageType("kr-NG", "Kanuri"),
      new DisplayLanguageType("km-KH", "Khmer"),
      new DisplayLanguageType("ckl-NG", "Kibaku"),
      new DisplayLanguageType("mfi-NG", "Mandara (Wandala)"),
      new DisplayLanguageType("mrt-NG", "Marghi"),
      new DisplayLanguageType("mnw-MM", "မန် (Mon)"),
      new DisplayLanguageType("nl-NL", "Nederlands (Dutch)"),
      new DisplayLanguageType("ann", "Obolo"),
      new DisplayLanguageType("pl-PL", "Polski (Polish)"),
      new DisplayLanguageType("el", "Polytonic Greek"),
      new DisplayLanguageType("pt-PT", "Português do Portugal"),
      new DisplayLanguageType("ff-ZA", "Pulaar-Fulfulde"), // or Fulah
      new DisplayLanguageType("ru-RU", "Pyccĸий (Russian)"),
      new DisplayLanguageType("shu-latn", "Shuwa (Latin)"),
      new DisplayLanguageType("sv-SE", "svenska (Swedish)"),
      new DisplayLanguageType("uk-UA", "Українська (Ukrainian)"),
      new DisplayLanguageType("hia-NG", "Waha"),
      new DisplayLanguageType("zh-CN", "中文(简体) (Simplified Chinese)")
    };
    return languages;
  }
}
