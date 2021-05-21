/**
 * Copyright (C) 2017-2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import com.tavultesoft.kmea.BaseActivity;

import android.content.Context;
import android.graphics.Bitmap;
import android.os.Bundle;
import android.view.View;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
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
        if (url != null && !url.toLowerCase().equals("about:blank")) {
          view.loadUrl(url);
         }

        return true;
      }

      @Override
      public void onPageStarted(WebView view, String url, Bitmap favicon) {
      }

      @Override
      public void onPageFinished(WebView view, String url) {
      }
    });

    // Always load offline info page from assets
    webView.loadUrl(kmOfflineUrl);
  }

  @Override
  public void onBackPressed() {
    finish();
    overridePendingTransition(0, android.R.anim.fade_out);
  }
}