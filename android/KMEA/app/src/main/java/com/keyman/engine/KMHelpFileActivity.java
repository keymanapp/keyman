/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.keyman.engine;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.TextView;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;

import androidx.appcompat.app.ActionBar;
import androidx.appcompat.widget.Toolbar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.content.FileProvider;

import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.FileProviderUtils;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.HelpFile;
import com.keyman.engine.util.WebViewUtils;

import java.io.File;

import static android.app.Application.getProcessName;

/**
 * Display Keyboard help file
 */
public class KMHelpFileActivity extends BaseActivity {
  private static final String TAG = "KMHelpFileActivity";
  private WebView webView;
  private Button finishButton;
  private String pkgID;
  private static boolean didSetDataDirectorySuffix = false;

  @SuppressLint({"SetJavaScriptEnabled", "InflateParams"})
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;

    // Different processes in the same application cannot directly share WebView-related data
    // https://developer.android.com/reference/android/webkit/WebView.html#setDataDirectorySuffix(java.lang.String)
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      if (!didSetDataDirectorySuffix) {
        String processName = getProcessName();
        WebView.setDataDirectorySuffix(processName);
        didSetDataDirectorySuffix = true;
      }
    }

    setContentView(R.layout.activity_help_file_layout);

    Bundle bundle = getIntent().getExtras();
    pkgID = "";
    String url = "";
    if (bundle != null) {
      pkgID = bundle.getString(KMManager.KMKey_PackageID);
      url = bundle.getString(KMManager.KMKey_CustomHelpLink);
    }

    finishButton = (Button) findViewById(R.id.finishButton);
    finishButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        finishAfterTransition();
        overridePendingTransition(0, android.R.anim.fade_out);
      }
    });

    Toolbar toolbar = (Toolbar) findViewById(R.id.titlebar);
    ActionBar actionBar = getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(null);
      actionBar.setDisplayUseLogoEnabled(false);
      actionBar.setDisplayShowHomeEnabled(false);
      actionBar.setDisplayShowTitleEnabled(false);
      actionBar.setDisplayShowCustomEnabled(true);
    }

    TextView packageActivityTitle = (TextView) findViewById(R.id.bar_title);
    String titleStr = String.format(getString(R.string.welcome_package), pkgID);
    packageActivityTitle.setText(titleStr);
    packageActivityTitle.setTextColor(getResources().getColor(android.R.color.black));

    webView = (WebView) findViewById(R.id.kmHelpFileWebView);
    webView.getSettings().setLayoutAlgorithm(WebSettings.LayoutAlgorithm.NORMAL);
    webView.getSettings().setJavaScriptEnabled(true);
    webView.getSettings().setAllowFileAccess(true);
    webView.getSettings().setAllowFileAccessFromFileURLs(true);
    webView.getSettings().setAllowUniversalAccessFromFileURLs(true);
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
      }

      @Override
      public boolean shouldOverrideUrlLoading(WebView view, String url) {
        String lowerURL = url.toLowerCase();
        if (lowerURL.equals("about:blank")) {
          return true; // never load a blank page, e.g. when the component initializes
        } else if (FileUtils.hasPDFExtension(url)) {
          // Special case to view pdf files
          url = url.replace("file://", "");
          Intent i = HelpFile.toActionView(context, url, pkgID);
          if (FileProviderUtils.exists(context) || KMManager.isTestMode()) {
            startActivity(i);
            return true;
          }
        }

        // Display URL
        return false;
      }

      @Override
      public void onPageStarted(WebView view, String url, Bitmap favicon) {
      }

      @Override
      public void onPageFinished(WebView view, String url) {
        // Inject a meta viewport tag into the head of the file if it doesn't exist
        WebViewUtils.injectViewport(view);
      }
    });

    webView.loadUrl("file:///" + url);
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
    WebViewUtils.cleanup(webView);
  }

  @Override
  public boolean onKeyDown(int keyCode, KeyEvent event) {
    if (event.getAction() == KeyEvent.ACTION_DOWN) {
      switch (keyCode) {
        case KeyEvent.KEYCODE_BACK:
          // Dismiss the help file
          super.onBackPressed();
          finishAndRemoveTask();
        break;
      }
    }

    return true;
  }

}
