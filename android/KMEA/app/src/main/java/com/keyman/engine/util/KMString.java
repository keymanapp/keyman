package com.keyman.engine.util;

import java.util.Locale;

/**
 * Utility to do String.format with Locale.ENGLISH.
 * This is needed for formatting strings used in Javascript calls
 */
public final class KMString {
  private static final String TAG = "KMString";
  private static Locale ENGLISH_LOCALE = Locale.ENGLISH;

  public static String format(String format, Object... args) {
    String value = String.format(ENGLISH_LOCALE, format, args);
    return value;
  }

}