/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.keyman.engine.util;

import android.util.Log;
import android.widget.Toast;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.BuildConfig;
import com.keyman.engine.KMManager;
import com.keyman.engine.util.DependencyUtil;
import com.keyman.engine.util.DependencyUtil.LibraryType;

import io.sentry.Breadcrumb;
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
   * Utility to log info and add as a Sentry breadcrumb, rather than
   * as an independent message
   * @param tag String of the caller
   * @param msg String of the info message
   */
  public static void LogBreadcrumb(String tag, String msg, boolean addStackTrace) {
    if (msg == null || msg.isEmpty()) {
      return;
    }

    Log.i(tag, msg);

    if (!DependencyUtil.libraryExists(LibraryType.SENTRY) || !Sentry.isEnabled()) {
      return;
    }

    Breadcrumb crumb = new Breadcrumb();
    crumb.setMessage(msg);
    crumb.setLevel(SentryLevel.INFO);

    if(addStackTrace) {
      StackTraceElement[] rawTrace = Thread.currentThread().getStackTrace();

      // The call that gets us the stack-trace above... shows up in the
      // stack trace, so we'll skip the first few (redundant) entries.
      int skipCount = 3;

      // Sentry does limit the size of messages... so let's just
      // keep 10 entries and call it a day.
      int limit = Math.min(rawTrace.length, 10 + skipCount);
      if(rawTrace.length > skipCount) {
        String[] trace = new String[limit - skipCount];
        for (int i = skipCount; i < limit; i++) {
          trace[i-skipCount] = rawTrace[i].toString();
        }
        crumb.setData("stacktrace", trace);
      }
    }
    Sentry.addBreadcrumb(crumb);
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
