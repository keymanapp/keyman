/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */
package com.tavultesoft.kmea.util;

import android.view.ViewGroup;
import android.webkit.WebView;

public final class WebViewUtil {
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
}
