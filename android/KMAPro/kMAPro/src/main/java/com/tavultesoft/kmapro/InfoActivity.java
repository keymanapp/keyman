/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.util.ArrayList;
import java.util.HashMap;

import com.tavultesoft.kmea.KMManager;

import android.net.Uri;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.webkit.WebSettings.LayoutAlgorithm;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.TextView;
import android.annotation.SuppressLint;
import android.app.ActionBar;
import android.app.Activity;
import android.content.Intent;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;

public class InfoActivity extends Activity {

  private final String kmBaseUrl = "http://keyman.com/android/app/";
  private String kmUrl = "";
  private final String htmlPath = "file:///android_asset/info/info.html";

  @SuppressLint("SetJavaScriptEnabled")
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    //getWindow().requestFeature(Window.FEATURE_PROGRESS);

    final ActionBar actionBar = getActionBar();
    actionBar.setLogo(R.drawable.keyman_logo);
    actionBar.setDisplayShowTitleEnabled(false);
    actionBar.setDisplayShowCustomEnabled(true);
    actionBar.setBackgroundDrawable(MainActivity.getActionBarDrawable(this));
    TextView version = new TextView(this);
    version.setWidth((int) getResources().getDimension(R.dimen.version_label_width));
    version.setTextSize(getResources().getDimension(R.dimen.version_label_textsize));
    version.setGravity(Gravity.CENTER);

    String ver = "";
    PackageInfo pInfo;
    try {
      pInfo = getPackageManager().getPackageInfo(getPackageName(), 0);
      ver = String.format("Version: %s", pInfo.versionName);
    } catch (NameNotFoundException e) {
      // Could not get version number
    }

    version.setText(ver);
    actionBar.setCustomView(version);

    setContentView(R.layout.activity_info);

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
    WebView webView = (WebView) findViewById(R.id.webView);
    webView.getSettings().setLayoutAlgorithm(LayoutAlgorithm.SINGLE_COLUMN);
    webView.getSettings().setJavaScriptEnabled(true);
    webView.getSettings().setUseWideViewPort(true);
    webView.getSettings().setLoadWithOverviewMode(true);
    webView.setLayerType(View.LAYER_TYPE_SOFTWARE, null);
    /*
    final Activity activity = this;
    webView.setWebChromeClient(new WebChromeClient() {
      public void onProgressChanged(WebView view, int progress) {
        activity.setProgress(progress * 1000);
      }
    });
    */

    webView.setWebViewClient(new WebViewClient() {
      @Override
      public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
        // Handle the error
      }

      @Override
      public boolean shouldOverrideUrlLoading(WebView view, String url) {
        if (url.equals(kmUrl) || url.equals(htmlPath)) {
          view.loadUrl(url);
        } else {
          Intent i = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
          startActivity(i);
        }

        return true;
      }
    });

    if (KMManager.hasConnection(this)) {
      // Load app info page from server
      webView.loadUrl(kmUrl);
    } else {
      webView.loadUrl(htmlPath);
      // Load app info page from assets
    }
  }

  @Override
  public void onBackPressed() {
    finish();
    overridePendingTransition(0, android.R.anim.fade_out);
  }
}