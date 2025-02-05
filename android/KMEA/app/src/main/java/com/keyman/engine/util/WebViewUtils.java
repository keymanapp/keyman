/**
 * Keyman is copyright (C) SIL International. MIT License.
 */
package com.keyman.engine.util;

import android.content.Context;
import android.content.pm.PackageManager;
import android.util.AndroidRuntimeException;
import android.view.ViewGroup;
import android.webkit.CookieManager;
import android.webkit.WebView;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class WebViewUtils {
  public static final String TAG = "WebViewUtils";

  // Keyman Engine functionality based on Chrome version
  public enum EngineWebViewVersionStatus {
    UNDETERMINED, // Functionality not determined yet
    DISABLED,     // WebView doesn't support touch keyboard features
    DEGRADED,     // WebView supports touch keyboards but not LDML keyboards
    FULL;         // WebView supports touch keyboards and LDML keyboards
  }

  // System WebView status
  public enum SystemWebViewStatus {
    UNDETERMINED,  // Functionality not determined yet
    NOT_INSTALLED, // WebView not installed
    DISABLED,      // WebView installed, but disabled
    FULL;          // WebView installed and enabled
  }

  private static final String CHROME_INSTALL_PATTERN_FORMATSTR = "^.*Chrome/([\\d.]+).*$";
  private static final Pattern installPattern = Pattern.compile(CHROME_INSTALL_PATTERN_FORMATSTR);

  /**
   * Get the Keyman Engine mode based on the Chrome version.
   * @param context       - The context
   * @param webView       - If provided, the Chrome version of webView is used
   * @param chromeVersion - String of the device's Chrome version.
   *                      If not provided, the device's Chrome version is queried
   * @return EngineWebViewVersionStatus
   */
  public static EngineWebViewVersionStatus getEngineWebViewVersionStatus(
      Context context, WebView webView, String chromeVersion) {
    if (context == null) {
      return EngineWebViewVersionStatus.DISABLED;
    }
    if (chromeVersion == null || chromeVersion.isEmpty()) {
      chromeVersion = getChromeVersion(context, webView);
    }

    if (FileUtils.compareVersions("37.0", chromeVersion) == FileUtils.VERSION_GREATER) {
      return EngineWebViewVersionStatus.DISABLED;
    } else if (FileUtils.compareVersions("57.0", chromeVersion) == FileUtils.VERSION_GREATER) {
      return EngineWebViewVersionStatus.DEGRADED;
    }

    return EngineWebViewVersionStatus.FULL;
  }

  /**
   * Determine the state of the system WebView (required for Keyman)
   * @param context
   * @return {SystemWebViewStatus}
   */
  public static SystemWebViewStatus getSystemWebViewStatus(
      Context context) {
    // Check whether WebView is installed - this also returns true when installed but disabled
    PackageManager packageManager = context.getPackageManager();
    if (!packageManager.hasSystemFeature("android.software.webview")) {
      return SystemWebViewStatus.NOT_INSTALLED;
    }

    // Use CookieManager to check if WebView is really enabled
    // https://stackoverflow.com/a/70354583
    try {
      CookieManager.getInstance();
    } catch (AndroidRuntimeException e) {
      // WebView installed but not enabled
      return SystemWebViewStatus.DISABLED;
    } catch (Exception e) {
      return SystemWebViewStatus.DISABLED;
    }

    return SystemWebViewStatus.FULL;
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
   * Get the Chrome version. Returns "0.0" if context is null or Chrome not installed.
   * @param context - The context
   * @param webView - If provided, the user agent string for the webview is parsed
   * @return String - Version string of Chrome
   */
  private static String getChromeVersion(Context context, WebView webView) {
    if (context == null) {
      KMLog.LogInfo(TAG, "Chrome not installed");
      return "0.0";
    }

    WebView mWebView = (webView != null) ? webView : new WebView(context);
    String userAgentString = mWebView.getSettings().getUserAgentString();

    Matcher matcher = installPattern.matcher(userAgentString);
    if (matcher.matches() && matcher.groupCount() >= 1 && matcher.group(1) != null) {
      return matcher.group(1);
    }

    KMLog.LogInfo(TAG, "Chrome not installed");
    return "0.0";
  }
}
