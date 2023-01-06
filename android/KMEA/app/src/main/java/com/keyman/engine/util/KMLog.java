/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea.util;

import android.util.Log;
import android.widget.Toast;

import com.tavultesoft.kmea.BaseActivity;
import com.tavultesoft.kmea.BuildConfig;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.util.DependencyUtil;
import com.tavultesoft.kmea.util.DependencyUtil.LibraryType;

import io.sentry.Sentry;
import io.sentry.SentryLevel;

public final class KMLog {
  private static final String TAG = "KMLog";

  /**
   * Utility to log info and send to Sentry
   * @param tag String of the caller
   * @param msg String of the info message
   */
  public static void LogInfo(String tag, String msg) {
    if (msg != null && !msg.isEmpty()) {
      Log.i(tag, msg);

      if (DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled()) {
        Sentry.captureMessage(msg, SentryLevel.INFO);
      }
    }
  }

  /**
   * Utility to log error and send to Sentry
   * @param tag String of the caller
   * @param msg String of the error message
   */
  public static void LogError(String tag, String msg) {
    if (msg != null && !msg.isEmpty()) {
      Log.e(tag, msg);

      if (KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME) != KMManager.Tier.STABLE) {
        BaseActivity.makeToast(null, msg, Toast.LENGTH_LONG);
      }

      if (DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled()) {
        Sentry.captureMessage(msg, SentryLevel.ERROR);
      }
    }
  }

  /**
   * Utility to log exceptions and send to Sentry
   * @param tag String of the caller
   * @param msg String of the exception message (maybe localized)
   * @param e Throwable exception
   */
  public static void LogException(String tag, String msg, Throwable e) {
    String errorMsg = "";
    if (msg != null && !msg.isEmpty()) {
      errorMsg = msg + "\n" + e;
    } else if (e != null) {
      errorMsg = e.getMessage();
    }
    Log.e(tag, errorMsg, e);

    if (KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME) != KMManager.Tier.STABLE) {
      BaseActivity.makeToast(null, errorMsg, Toast.LENGTH_LONG);
    }

    if (DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled()) {
      Sentry.addBreadcrumb(errorMsg);
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
    if (obj != null && DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled()) {
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
