/**
 * Keyman is copyright (C) SIL International. MIT License.
 */
package com.tavultesoft.kmea.util;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.view.ViewGroup;
import android.webkit.WebView;
import com.tavultesoft.kmea.util.KMLog;

public final class WebViewUtils {
  public static final String TAG = "WebViewUtils";

  // Keyman Engine functionality based on Chrome version
  public enum EngineModeType {
    ENGINE_MODE_TYPE_DISABLED, // WebView doesn't support touch keyboard features
    ENGINE_MODE_TYPE_DEGRADED, // WebView supports touch keyboards but not LDML keyboards
    ENGINE_MODE_TYPE_FULL;     // WebView supports touch keyboards and LDML keyboards
  }

  /**
   * Get the Keyman Engine mode based on the Chrome version.
   * @param context       - The context
   * @param chromeVersion - String of the device's Chrome version.
   *                      If not provided, the device's Chrome version is queried
   * @return EngineModeType
   */
  public static EngineModeType getEngineModeType(Context context, String chromeVersion) {
    if (context == null) {
      return EngineModeType.ENGINE_MODE_TYPE_DISABLED;
    }
    if (chromeVersion == null || chromeVersion.isEmpty()) {
      chromeVersion = getChromeVersion(context);
    }

    if (FileUtils.compareVersions(chromeVersion, "57.0") == FileUtils.VERSION_GREATER) {
      return EngineModeType.ENGINE_MODE_TYPE_FULL;
    } else if (FileUtils.compareVersions(chromeVersion, "37.0") == FileUtils.VERSION_GREATER) {
      return EngineModeType.ENGINE_MODE_TYPE_DEGRADED;
    }

    return EngineModeType.ENGINE_MODE_TYPE_DISABLED;
  }

  // Inject a meta viewport tag into the head of the file if it doesn't exist
  public static void injectViewport(WebView webView) {
    if (webView != null) {
      webView.loadUrl(
        "javascript:(function() {" +
          "if(document.head && !document.querySelectorAll('meta[name=viewport]').length) {"+
          "let meta=document.createElement('meta');"+
          "meta.name='viewport';"+
          "meta.content='width=device-width, initial-scale=1';"+
          "document.head.appendChild(meta);"+
          "}"+
          "})()"
      );
    }
  }

  // Blank the webView and destroy completely
  // Reference: https://stackoverflow.com/questions/17418503/destroy-webview-in-android/17458577#17458577
  public static void cleanup(WebView webView) {
    if (webView != null) {
      webView.loadUrl("about:blank");
      ViewGroup viewGroup = (ViewGroup) webView.getParent();
      if (viewGroup != null) {
        viewGroup.removeView(webView);
      }
      webView.removeAllViews();
      webView.destroy();
      webView = null;
    }
  }

  /**
   * Get the Chrome version. Returns empty string if Chrome not installed.
   * @param context - The context
   * @return String
   */
  private static String getChromeVersion(Context context) {
    if (context == null) {
      return "";
    }
    try {
      PackageInfo pInfo;
      pInfo = context.getPackageManager().getPackageInfo("com.android.chrome", PackageManager.GET_ACTIVITIES);
      if (pInfo != null) {
        return pInfo.versionName;
      }
    } catch (PackageManager.NameNotFoundException e) {
      KMLog.LogInfo(TAG, "Chrome not installed");
    }
    return "";
  }
}
