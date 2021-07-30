/**
 * Keyman is copyright (C) SIL International. MIT License.
 */
package com.tavultesoft.kmea.util;

import android.content.Context;
import android.view.ViewGroup;
import android.webkit.WebView;
import com.tavultesoft.kmea.util.KMLog;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class WebViewUtils {
  public static final String TAG = "WebViewUtils";

  // Keyman Engine functionality based on Chrome version
  public enum EngineWebViewVersionStatus {
    DISABLED, // WebView doesn't support touch keyboard features
    DEGRADED, // WebView supports touch keyboards but not LDML keyboards
    FULL;     // WebView supports touch keyboards and LDML keyboards
  }

  private static final String CHROME_INSTALL_PATTERN_FORMATSTR = "^.*Chrome/([\\d\\.]+)\\s.*$";
  private static final Pattern installPattern = Pattern.compile(CHROME_INSTALL_PATTERN_FORMATSTR);

  /**
   * Get the Keyman Engine mode based on the Chrome version.
   * @param context       - The context
   * @param chromeVersion - String of the device's Chrome version.
   *                      If not provided, the device's Chrome version is queried
   * @return EngineWebViewVersionStatus
   */
  public static EngineWebViewVersionStatus getEngineWebViewVersionStatus(Context context, String chromeVersion) {
    if (context == null) {
      return EngineWebViewVersionStatus.DISABLED;
    }
    if (chromeVersion == null || chromeVersion.isEmpty()) {
      chromeVersion = getChromeVersion(context);
    }

    if (FileUtils.compareVersions(chromeVersion, "57.0") == FileUtils.VERSION_GREATER) {
      return EngineWebViewVersionStatus.FULL;
    } else if (FileUtils.compareVersions(chromeVersion, "37.0") == FileUtils.VERSION_GREATER) {
      return EngineWebViewVersionStatus.DEGRADED;
    }

    return EngineWebViewVersionStatus.DISABLED;
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
      KMLog.LogInfo(TAG, "Chrome not installed");
      return "";
    }

    WebView mWebView = new WebView(context);
    String userAgentString = mWebView.getSettings().getUserAgentString();

    Matcher matcher = installPattern.matcher(userAgentString);
    if (matcher.matches() && matcher.groupCount() >= 1 && matcher.group(1) != null) {
      return matcher.group(1);
    }

    KMLog.LogInfo(TAG, "Chrome not installed");
    return "";
  }
}
