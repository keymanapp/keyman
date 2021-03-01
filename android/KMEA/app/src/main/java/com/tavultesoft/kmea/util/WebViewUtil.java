/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */
package com.tavultesoft.kmea.util;

import android.webkit.WebView;

public final class WebViewUtil {
  // Inject a meta viewport tag into the head of the file if it doesn't exist
  public static void injectViewport(WebView w) {
    if (w != null) {
      w.loadUrl(
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
}
