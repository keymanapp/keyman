/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.keyman.engine.util;

import static com.keyman.engine.KMManager.KMKey_LexicalModelID;

import android.util.Log;
import android.widget.Toast;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.BuildConfig;
import com.keyman.engine.KMManager;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.util.DependencyUtil.LibraryType;

import java.util.Map;

import io.sentry.Breadcrumb;
import io.sentry.Sentry;
import io.sentry.SentryLevel;

public final class KMLog {
  private static final String TAG = "KMLog";

  private static final String KEYBOARD_TAG = "keyman.keyboardId";
  private static final String KEYBOARD_COUNT_TAG = "keyman.installedKeyboardCount";
  private static final String MODEL_TAG = "keyman.modelId";
  private static final String LANGCODE_TAG = "keyman.languageCode";
  private static final String DEBUG_LOGGING_ERROR_TAG = "keyman.debugLoggingError";

  private static void tagDebugInfo() {
    String kbdId = "";
    String lngCode = "";
    String modelId = "";
    int kbdCount = 0;
    // Do not risk raising a new error while tagging info for another error.
    try {
      // Both take a context parameter... but don't actually need or use it!
      Keyboard kbd = KMManager.getCurrentKeyboardInfo(null);
      kbdCount = KMManager.getKeyboardsList(null).size();
      if (kbd != null) {
        kbdId = kbd.getKeyboardID();
        lngCode = kbd.getLanguageCode();
        Map<String, String> modelMap = KMManager.getAssociatedLexicalModel(kbd.getLanguageID());
        if (modelMap != null) {
          modelId = modelMap.get(KMKey_LexicalModelID);
          if (modelId == null) {
            modelId = "";
          }
        }
      }
    } catch (Exception ex) {
      Sentry.setTag(DEBUG_LOGGING_ERROR_TAG,  ex.getMessage() == null ? "" : ex.getMessage());
    }
    Sentry.setTag(KEYBOARD_TAG, kbdId);
    Sentry.setTag(KEYBOARD_COUNT_TAG, "" + kbdCount);
    Sentry.setTag(LANGCODE_TAG, lngCode);
    Sentry.setTag(MODEL_TAG, modelId);
  }

  private static boolean canLogToSentry() {
    return DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled();
  }

  /**
   * Utility to log info and send to Sentry
   * @param tag String of the caller
   * @param msg String of the info message
   */
  public static void LogInfo(String tag, String msg) {
    if (msg == null || msg.isEmpty()) {
      return;
    }

    Log.i(tag, msg);

    if (!canLogToSentry()) {
      return;
    }

    Sentry.withScope(scope -> {
      tagDebugInfo();
      Sentry.captureMessage(msg, SentryLevel.INFO);
    });
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

    if (!canLogToSentry()) {
      return;
    }

    Breadcrumb crumb = new Breadcrumb();
    crumb.setMessage(msg);
    crumb.setLevel(SentryLevel.INFO);

    try {
      if(addStackTrace) {
        StackTraceElement[] rawTrace = Thread.currentThread().getStackTrace();

        // The call that gets us the stack-trace above... shows up in the
        // stack trace, so we'll skip the first few (redundant) entries.
        int skipCount = 3;

        // Sentry does limit the size of messages... so let's just
        // keep 10 entries and call it a day.
        if(rawTrace.length > skipCount) {
          int limit = Math.min(rawTrace.length, 10 + skipCount);
          String[] trace = new String[limit - skipCount];
          for (int i = skipCount; i < limit; i++) {
            trace[i-skipCount] = rawTrace[i].toString();
          }
          crumb.setData("stacktrace", trace);
        }
      }
    } catch (Exception e) {
      Sentry.captureException(e);
    }

    Sentry.addBreadcrumb(crumb);
  }

  /**
   * Utility to log error and send to Sentry
   * @param tag String of the caller
   * @param msg String of the error message
   */
  public static void LogError(String tag, String msg) {
    if (msg == null || msg.isEmpty()) {
      return;
    }

    Log.e(tag, msg);

    try {
      // On alpha and beta tiers, we pop an error toast so testers can be aware
      // of the error
      if (KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME) != KMManager.Tier.STABLE) {
        BaseActivity.makeToast(null, msg, Toast.LENGTH_LONG);
      }
    } catch(Exception e) {
      if(canLogToSentry()) {
        Sentry.captureException(e);
      }
    }

    if (!canLogToSentry()) {
      return;
    }

    Sentry.withScope(scope -> {
      tagDebugInfo();
      Sentry.captureMessage(msg, SentryLevel.ERROR);
    });
  }

  /**
   * Utility to log exceptions and send to Sentry
   * @param tag String of the caller
   * @param msg String of the exception message (maybe localized)
   * @param e Throwable exception
   */
  public static void LogException(String tag, String msg, Throwable e) {
    KMLog.LogExceptionWithData(tag, msg, null, null, e);
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
    String errorMsg = "";
    try {
      if (msg != null && !msg.isEmpty()) {
        errorMsg = msg + "\n" + e;
      } else if (e != null) {
        errorMsg = e.toString();
      }

      // On alpha and beta tiers, we pop an error toast so testers can be aware
      // of the exception
      if (KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME) != KMManager.Tier.STABLE) {
        BaseActivity.makeToast(null, errorMsg, Toast.LENGTH_LONG);
      }
    } catch (Exception innerE) {
      if (canLogToSentry()) {
        Sentry.captureException(innerE);
      }
    }

    Log.e(tag, errorMsg);

    if (!canLogToSentry()) {
      return;
    }

    Sentry.addBreadcrumb(errorMsg);
    Sentry.withScope(scope -> {
      tagDebugInfo();
      try {
        if(obj != null && objName != null) {
          Sentry.setExtra(objName, obj.toString());
        }
      } catch(Exception innerE) {
        Sentry.captureException(innerE);
      }
      Sentry.captureException(e);

      if(obj != null && objName != null) {
        // And remove the exception-specific tagged data, lest it also be
        // tracked on subsequent errors not associated with the current call.
        Sentry.removeExtra(objName);
      }
    });
  }
}
