/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea.util;

import android.util.Log;
import io.sentry.Sentry;
import io.sentry.SentryLevel;

public final class KMLog {
  private static final String TAG = "KMLog";

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
   * Utility to print an Object in the log exception sent to Sentry
   * @param tag String of the caller
   * @param msg String of the exception message
   * @param objName String of the object name
   * @param obj Object which will be formatted as a string
   * @param e Throwable exception
   */
  public static void LogExceptionWithData(String tag, String msg,
                                          String objName, Object obj, Throwable e) {
    if (obj != null && Sentry.isEnabled()) {
      String objStr = null;
      try {
        objStr = obj.toString();
        Sentry.setExtra(objName, objStr);
      } catch (Exception innerE) {
        LogException(TAG, "Sentry.setExtra failed for " + objName, innerE);
      }
      // Report the original exception
      LogException(tag, msg, e);
    }
  }
}
