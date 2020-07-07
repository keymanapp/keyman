/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea.util;

import android.util.Log;
import io.sentry.core.Sentry;
import io.sentry.core.SentryLevel;

import org.json.JSONArray;

public final class KMLog {

  /**
   * Utility to log error and send to Sentry
   * @param tag String of the caller
   * @param msg String of the error message
   */
  public static void LogError(String tag, String msg) {
    if (msg != null && !msg.isEmpty()) {
      Log.e(tag, msg);

      if (Sentry.isEnabled()) {
        Sentry.captureMessage(msg, SentryLevel.ERROR);
      }
    }
  }

  /**
   * Utility to log exceptions and send to Sentry
   * @param tag String of the caller
   * @param msg String of the exception message
   * @param e Throwable exception
   */
  public static void LogException(String tag, String msg, Throwable e) {
    if (msg != null && !msg.isEmpty()) {
      Log.e(tag, msg + "\n" + e);
    } else if (e != null) {
      Log.e(tag, e.getMessage(), e);
    }

    if (Sentry.isEnabled()) {
      if (msg != null && !msg.isEmpty()) {
        Sentry.addBreadcrumb(msg);
      }
      Sentry.captureException(e);
    }
  }

  /**
   * Utility to include JSONArray in the log exception sent to Sentry
   * @param tag String of the caller
   * @param msg String of the exception message
   * @param array JSONArray of the installed keyboards_list.json file
   * @param e Throwable exception
   */
  public static void LogJSONException(String tag, String msg, JSONArray array, Throwable e) {
    if (array != null && Sentry.isEnabled()) {
      String formattedList = null;
      try {
        // Try to pretty-print the installed keyboards list
        final int indentSpaces = 2;
        formattedList = array.toString(indentSpaces);
        Sentry.setExtra("keyboards_list.json", formattedList);
      } catch (Exception innerE) {
        // Throwaway the formatting exception and just report the original one
        Sentry.setExtra("keyboards_list.json", array.toString());
      }
      LogException(tag, msg, e);
    }
  }
}
