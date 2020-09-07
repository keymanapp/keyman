/**
 * Copyright (C) 2017-2019 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import com.tavultesoft.kmea.BuildConfig;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KMManager.FormFactor;
import com.tavultesoft.kmea.util.KMLog;

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
  private final static String TAG = "InfoActivity";
  private WebView webView;
  private final String HELP_PRODUCTION_HOST = "help.keyman.com";
  private final String HELP_STAGING_HOST = "help.keyman-staging.com";
  private final String HELP_BASE_FORMAT_STR = "https://%s/products/android/%s/%s?embed=android&formfactor=%s";
  private String kmUrl = "";
  private final String htmlPath = "file:///android_asset/info/products/android";
  private final String htmlPage = "index.php";
  private String kmOfflineUrl = "";

  @SuppressLint("SetJavaScriptEnabled")
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;

    setContentView(R.layout.activity_info);

    Toolbar toolbar = findViewById(R.id.titlebar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setTitle(null);
    getSupportActionBar().setDisplayUseLogoEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setLogo(R.drawable.keyman_logo);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    getSupportActionBar().setDisplayShowCustomEnabled(true);

    TextView version = findViewById(R.id.bar_title);
    version.setWidth((int) getResources().getDimension(R.dimen.label_width));
    version.setGravity(Gravity.CENTER);

    // Parse app version string to create a version title (Not using KMManager.getVersion() because that's for KMEA)
    String versionStr = BuildConfig.VERSION_NAME;

    String versionTitle = String.format(getString(R.string.title_version), versionStr);
    version.setText(versionTitle);

    // Extract the the major minor version from the full version string
    String[] versionArray = versionStr.split("\\.", 3);
    String majorMinorVersion = String.format("%s.%s", versionArray[0], versionArray[1]);

    // Determine the appropriate form factor
    FormFactor ff = KMManager.getFormFactor();
    String formFactor;

    if(ff == FormFactor.PHONE) {
      formFactor = "phone";
    } else {
      formFactor = "tablet";
    }

    String helpHost = "";
    switch (KMManager.getTier(BuildConfig.VERSION_NAME)) {
      case ALPHA:
      case BETA:
        helpHost = HELP_STAGING_HOST;
        break;
      default:
        helpHost = HELP_PRODUCTION_HOST;
    }

    kmUrl = String.format(HELP_BASE_FORMAT_STR, helpHost, majorMinorVersion, htmlPage, formFactor);
    // The offline mirroring process (currently) adds .html to the end of the whole string.
    kmOfflineUrl = String.format("%s/%s/%s.html", htmlPath, formFactor, htmlPage);
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
      webView.loadUrl(kmOfflineUrl);
    }
  }

  @Override
  public void onBackPressed() {
    finish();
    overridePendingTransition(0, android.R.anim.fade_out);
  }
}