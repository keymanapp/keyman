/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

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
  public static final DisplayLanguageType[] DisplayLanguages =
  {
    new DisplayLanguageType(unspecifiedLocale, "Default Locale"),
    new DisplayLanguageType("km-KH", "Khmer"),
    new DisplayLanguageType("ann", "Obolo")
  };

}