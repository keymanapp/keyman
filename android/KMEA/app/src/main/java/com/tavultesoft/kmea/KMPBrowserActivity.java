/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.PorterDuff.Mode;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.widget.ImageButton;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import androidx.appcompat.app.AppCompatActivity;

import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KMManager.Tier;
import com.tavultesoft.kmea.util.KMPLink;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.tavultesoft.kmea.util.KMPLink.KMP_PRODUCTION_HOST;
import static com.tavultesoft.kmea.util.KMPLink.KMP_STAGING_HOST;

public class KMPBrowserActivity extends AppCompatActivity {
  private static final String TAG = "KMPBrowserActivity";

  // URL for keyboard search web page presented to user when they add a keyboard in the app.
  private static final String KMP_SEARCH_KEYBOARDS_FORMATSTR = "%s/go/android/%s/download-keyboards%s";
  private static final String KMP_SEARCH_KEYBOARDS_LANGUAGES = "/languages/%s";
  private WebView webView;
  private boolean isLoading = false;
  private boolean didFinishLoading = false;

  @SuppressLint({"SetJavaScriptEnabled", "InflateParams"})
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;

    setContentView(R.layout.activity_kmp_browser);

    webView = (WebView) findViewById(R.id.kmpBrowserWebView);
    webView.getSettings().setLayoutAlgorithm(WebSettings.LayoutAlgorithm.NORMAL);
    webView.getSettings().setJavaScriptEnabled(true);
    webView.getSettings().setUseWideViewPort(true);
    webView.getSettings().setLoadWithOverviewMode(true);
    webView.getSettings().setBuiltInZoomControls(true);
    webView.getSettings().setSupportZoom(true);
    webView.setLayerType(View.LAYER_TYPE_SOFTWARE, null);

    webView.setWebChromeClient(new WebChromeClient() {
      public void onProgressChanged(WebView view, int progress) {
      }
    });
    webView.setWebViewClient(new WebViewClient() {
      @Override
      public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
        didFinishLoading = true;
        isLoading = false;
      }

      @Override
      public boolean shouldOverrideUrlLoading(WebView view, String url) {
        String lowerURL = url.toLowerCase();
        if (lowerURL.equals("about:blank")) {
          return true; // never load a blank page, e.g. when the component initializes
        }

        if (KMPLink.isKeymanInstallLink(url)) {
          String downloadURL = KMPLink.getKeyboardDownloadLink(url);

          // Create intent with keyboard download link for KMAPro main activity to handle
          Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(downloadURL));
          startActivityForResult(intent, 1);

          // Finish activity
          finish();
        }
        if (lowerURL.startsWith("keyman:")) {
          // Warn for unsupported keyman schemes
          Log.d(TAG, "Scheme for " + url + " not handled");
          return true;
        }

        // Display URL
        return false;
      }

      @Override
      public void onPageStarted(WebView view, String url, Bitmap favicon) {
        isLoading = true;
        didFinishLoading = false;
      }

      @Override
      public void onPageFinished(WebView view, String url) {
        didFinishLoading = true;
        isLoading = false;
      }
    });

    // Tier determines the keyboard search host
    String host = (KMManager.getTier(BuildConfig.VERSION_NAME) == Tier.STABLE) ?
      KMP_PRODUCTION_HOST : KMP_STAGING_HOST;
    // If language ID is provided, include it in the keyboard search
    String languageID = getIntent().getStringExtra("languageCode");
    String languageStr = (languageID != null) ? String.format(KMP_SEARCH_KEYBOARDS_LANGUAGES, languageID) : "";
    String appMajorVersion = KMManager.getMajorVersion();
    String kmpSearchUrl = String.format(KMP_SEARCH_KEYBOARDS_FORMATSTR, host, appMajorVersion, languageStr);
    webView.loadUrl(kmpSearchUrl);
  }

  @Override
  protected void onResume() {
    super.onResume();
    if (webView != null) {
      webView.reload();
    }
  }

  @Override
  protected void onPause() {
    super.onPause();
  }

  @Override
  protected void onDestroy() {
    super.onDestroy();
  }

  @Override
  protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    super.onActivityResult(requestCode, resultCode, data);

    if (webView != null) {
      if (resultCode == RESULT_OK && data != null) {
        String url = data.getStringExtra("url");
        if (url != null)
          webView.loadUrl(url);
      }
    }
  }

  @Override
  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
  }

  @Override
  public void onBackPressed() {
    if (webView != null && webView.canGoBack()) {
      webView.goBack();
    } else {
      super.onBackPressed();
      finish();
    }
  }

}
