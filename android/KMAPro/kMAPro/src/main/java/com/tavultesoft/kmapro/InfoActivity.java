/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.util.ArrayList;
import java.util.HashMap;

import com.tavultesoft.kmea.KMManager;

import android.content.Context;
import android.graphics.Bitmap;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.TextView;
import android.annotation.SuppressLint;
import androidx.appcompat.widget.Toolbar;
import androidx.appcompat.app.AppCompatActivity;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;

public class InfoActivity extends AppCompatActivity {

  private WebView webView;
  private final String kmBaseUrl = "https://keyman.com/android/app/";
  private String kmUrl = "";
  private final String htmlPath = "file:///android_asset/info/info.html";

  @SuppressLint("SetJavaScriptEnabled")
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;

    setContentView(R.layout.activity_info);

    Toolbar toolbar = findViewById(R.id.info_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setTitle(null);
    getSupportActionBar().setDisplayUseLogoEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setLogo(R.drawable.keyman_logo);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    getSupportActionBar().setDisplayShowCustomEnabled(true);

    TextView version = new TextView(this);
    version.setWidth((int) getResources().getDimension(R.dimen.label_width));
    version.setTextSize(getResources().getDimension(R.dimen.titlebar_label_textsize));
    version.setGravity(Gravity.CENTER);

    String ver = "";
    PackageInfo pInfo;
    try {
      pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
      ver = String.format("%s: %s", getString(R.string.title_version), pInfo.versionName);
    } catch (NameNotFoundException e) {
      // Could not get version number
    }
    version.setText(ver);
    getSupportActionBar().setCustomView(version);


    String currentKbID = KMManager.KMDefault_KeyboardID;
    HashMap<String, String> curKbInfo = KMManager.getCurrentKeyboardInfo(this);
    if (curKbInfo != null)
      currentKbID = KMManager.getCurrentKeyboardInfo(this).get(KMManager.KMKey_KeyboardID);

    String installedKbs = "";
    ArrayList<HashMap<String, String>> kbList = KMManager.getKeyboardsList(this);
    if (kbList != null) {
      for (HashMap<String, String> kbInfo : kbList) {
        String kbID = kbInfo.get(KMManager.KMKey_KeyboardID);
        if (!installedKbs.contains(kbID))
          installedKbs += kbID + ",";
      }
    }

    int lastIndex = installedKbs.length() - 1;
    if (lastIndex > 0)
      installedKbs = installedKbs.substring(0, lastIndex);

    if (installedKbs.isEmpty())
      installedKbs = currentKbID;

    kmUrl = String.format("%s?active=%s&installed=%s", kmBaseUrl, currentKbID, installedKbs);
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

    if (KMManager.hasConnection(context)) {
      // Load app info page from server
      webView.loadUrl(kmUrl);
    } else {
      // Load app info page from assets
      webView.loadUrl(htmlPath);
    }
  }

  @Override
  public void onBackPressed() {
    finish();
    overridePendingTransition(0, android.R.anim.fade_out);
  }
}