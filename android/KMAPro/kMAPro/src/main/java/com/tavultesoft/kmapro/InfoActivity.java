/**
 * Copyright (C) 2017-2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import com.keyman.engine.BaseActivity;

import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.ImageButton;
import android.widget.TextView;
import android.annotation.SuppressLint;

public class InfoActivity extends BaseActivity {
  private final static String TAG = "InfoActivity";
  private WebView webView;
  private final String htmlPath = "file:///android_asset/info";
  private final String htmlPage = "index.html";
  private String kmOfflineUrl = "";

  @SuppressLint("SetJavaScriptEnabled")
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;

    setContentView(R.layout.activity_info);

    // Navigation buttons
    final ImageButton backButton = (ImageButton) findViewById(R.id.back_button);
    backButton.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        onBackPressed();
      }
    });

    final ImageButton forwardButton = (ImageButton) findViewById(R.id.forward_button);
    forwardButton.setEnabled(false);
    forwardButton.setOnClickListener(new View.OnClickListener() {
      @Override
      public void onClick(View v) {
        if (webView != null && webView.canGoForward()) {
          webView.goForward();
        }
      }
    });

    final ImageButton closeButton = (ImageButton) findViewById(R.id.close_button);
    closeButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        finish();
      }
    });

    TextView version = findViewById(R.id.infoVersion);

    // Parse app version string to create a version title (Not using KMManager.getVersion() because that's for KMEA)
    String versionStr = BuildConfig.VERSION_NAME;

    String versionTitle = String.format(getString(R.string.title_version), versionStr);
    version.setText(versionTitle);

    // The offline mirroring process (currently) adds .html to the end of the whole string.
    kmOfflineUrl = String.format("%s/%s", htmlPath, htmlPage);
    webView = (WebView) findViewById(R.id.infoWebView);
    webView.getSettings().setLayoutAlgorithm(WebSettings.LayoutAlgorithm.SINGLE_COLUMN);
    webView.getSettings().setJavaScriptEnabled(true);
    webView.getSettings().setAllowFileAccess(true);
    webView.getSettings().setUseWideViewPort(true);
    webView.getSettings().setLoadWithOverviewMode(true);
    webView.setLayerType(View.LAYER_TYPE_SOFTWARE, null);

    webView.setWebChromeClient(new WebChromeClient() {
    });
    webView.setWebViewClient(new WebViewClient() {
      @Override
      public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
      }

      @Override
      public boolean shouldOverrideUrlLoading(WebView view, String url) {
        String lowerURL = url.toLowerCase();
        if (lowerURL.equals("about:blank")) {
          return true; // never load a blank page, e.g. when the component initializes
        }

        Uri uri = Uri.parse(url);

        // All links that aren't internal Keyman help links open in user's browser
        if (!url.contains(htmlPath)) {
          Intent intent = new Intent(Intent.ACTION_VIEW, uri);
          startActivity(intent);
        } else {
          view.loadUrl(url);
        }

        return true;
      }

      @Override
      public void onPageStarted(WebView view, String url, Bitmap favicon) {
      }

      @Override
      public void onPageFinished(WebView view, String url) {
        if (webView != null) {
          forwardButton.setEnabled(webView.canGoForward());
        }
      }
    });

    // Always load offline info page from assets
    webView.loadUrl(kmOfflineUrl);
  }

  @Override
  public void onBackPressed() {
    if (webView != null && webView.canGoBack()) {
      webView.goBack();
    } else {
      super.onBackPressed();
      finish();
      overridePendingTransition(0, android.R.anim.fade_out);
    }
  }
}