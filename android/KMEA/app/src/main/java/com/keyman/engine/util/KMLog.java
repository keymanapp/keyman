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

  private static final String KEYBOARD_TAG = "keyboardId";
  private static final String KEYBOARD_COUNT_TAG = "installedKeyboardCount";
  private static final String MODEL_TAG = "modelId";
  private static final String LANGCODE_TAG = "languageCode";

  private static boolean isLogging = false;

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
    } catch (Exception ignored) {
    }
    Sentry.setExtra(KEYBOARD_TAG, kbdId);
    Sentry.setExtra(KEYBOARD_COUNT_TAG, "" + kbdCount);
    Sentry.setExtra(LANGCODE_TAG, lngCode);
    Sentry.setExtra(MODEL_TAG, modelId);
  }

  /**
   * Utility to log info and send to Sentry
   * @param tag String of the caller
   * @param msg String of the info message
   */
  public static void LogInfo(String tag, String msg) {
    if(isLogging) {
      return;
    }
    isLogging = true;
    if (msg != null && !msg.isEmpty()) {
      Log.i(tag, msg);

      if (DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled()) {
        tagDebugInfo();
        Sentry.captureMessage(msg, SentryLevel.INFO);
      }
    }
    isLogging = false;
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

    if(isLogging) {
      return;
    }
    isLogging = true;

    Log.i(tag, msg);

    if (!DependencyUtil.libraryExists(LibraryType.SENTRY) || !Sentry.isEnabled()) {
      isLogging = false;
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
    tagDebugInfo();
    Sentry.addBreadcrumb(crumb);
    isLogging = false;
  }

  /**
   * Utility to log error and send to Sentry
   * @param tag String of the caller
   * @param msg String of the error message
   */
  public static void LogError(String tag, String msg) {
    if(isLogging) {
      return;
    }
    isLogging = true;
    if (msg != null && !msg.isEmpty()) {
      Log.e(tag, msg);

      if (KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME) != KMManager.Tier.STABLE) {
        BaseActivity.makeToast(null, msg, Toast.LENGTH_LONG);
      }

      if (DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled()) {
        tagDebugInfo();
        Sentry.captureMessage(msg, SentryLevel.ERROR);
      }
    }
    isLogging = false;
  }

  /**
   * Utility to log exceptions and send to Sentry
   * @param tag String of the caller
   * @param msg String of the exception message (maybe localized)
   * @param e Throwable exception
   */
  public static void LogException(String tag, String msg, Throwable e) {
    if(isLogging) {
      return;
    }
    isLogging = true;
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
      tagDebugInfo();
      Sentry.addBreadcrumb(errorMsg);
      Sentry.captureException(e);
    }
    isLogging = false;
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
    if(isLogging) {
      return;
    }
    isLogging = true;
    if (obj != null && DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled()) {
      tagDebugInfo();
      String objStr = null;
      try {
        objStr = obj.toString();
        Sentry.setExtra(objName, objStr);
      } catch (Exception innerE) {
        LogException(TAG, "Sentry.setExtra failed for " + objName, innerE);
      }
      // Report the original exception
      LogException(tag, msg, e);
      Sentry.removeExtra(objName);
    }
    isLogging = false;
  }
}
