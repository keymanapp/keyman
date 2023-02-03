/**
 * Copyright (C) 2020-2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Toast;
import androidx.appcompat.app.AppCompatActivity;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.util.KMPLink;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.WebViewUtils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static android.app.Application.getProcessName;

public class KMPBrowserActivity extends BaseActivity {
  private static final String TAG = "KMPBrowserActivity";

  // URL for keyboard search web page presented to user when they add a keyboard in the app.
  private static final String KMP_SEARCH_KEYBOARDS_FORMATSTR = "https://%s/go/android/%s/download-keyboards%s";
  private static final String KMP_SEARCH_KEYBOARDS_LANGUAGES = "/languages/%s";

  // Patterns for determining if a link should be opened in external browser
  // 1. Host isn't keyman.com (production/staging)
  // 2. Host is keyman.com but not /keyboards/
  private static final String INTERNAL_KEYBOARDS_LINK_FORMATSTR = "^http(s)?://(%s|%s)/keyboards([/?].*)?$";
  private static final String keyboardPatternFormatStr = KMString.format(INTERNAL_KEYBOARDS_LINK_FORMATSTR,
    KMPLink.KMP_PRODUCTION_HOST,
    KMPLink.KMP_STAGING_HOST);
  private static final Pattern keyboardPattern = Pattern.compile(keyboardPatternFormatStr);

  private WebView webView;
  private boolean isLoading = false;
  private boolean didFinishLoading = false;
  private static boolean didSetDataDirectorySuffix = false;

  @SuppressLint({"SetJavaScriptEnabled", "InflateParams"})
  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;

    // Difference processes in the same application cannot directly share WebView-related data
    // https://developer.android.com/reference/android/webkit/WebView.html#setDataDirectorySuffix(java.lang.String)
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      if (!didSetDataDirectorySuffix) {
        String processName = getProcessName();
        WebView.setDataDirectorySuffix(processName);
        didSetDataDirectorySuffix = true;
      }
    }

    setContentView(R.layout.activity_kmp_browser);

    webView = (WebView) findViewById(R.id.kmpBrowserWebView);
    webView.getSettings().setLayoutAlgorithm(WebSettings.LayoutAlgorithm.NORMAL);
    webView.getSettings().setJavaScriptEnabled(true);
    webView.getSettings().setAllowFileAccess(true);
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
          Uri downloadURI = KMPLink.getKeyboardDownloadLink(url);

          // Create intent with keyboard download link for KMAPro main activity to handle
          Intent intent = new Intent(context, MainActivity.class);
          intent.setData(downloadURI);
          startActivity(intent);

          // Finish activity
          finish();
        } else if (!isKeymanKeyboardsLink(url)) {
          Uri uri = Uri.parse(url);

          // All links that aren't internal Keyman keyboard links open in user's browser
          Intent intent = new Intent(Intent.ACTION_VIEW, uri);
          if (intent.resolveActivity(getPackageManager()) != null) {
            startActivity(intent);
          } else {
            Toast.makeText(context, getString(R.string.unable_to_open_browser), Toast.LENGTH_SHORT).show();
          }
          return true;
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
    String host = KMPLink.getHost();
    // If language ID is provided, include it in the keyboard search
    String languageID = getIntent().getStringExtra("languageCode");
    String languageStr = (languageID != null) ? KMString.format(KMP_SEARCH_KEYBOARDS_LANGUAGES, languageID) : "";
    String appMajorVersion = KMManager.getMajorVersion();
    String kmpSearchUrl = KMString.format(KMP_SEARCH_KEYBOARDS_FORMATSTR, host, appMajorVersion, languageStr);
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
    WebViewUtils.cleanup(webView);
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

  /**
   * Check if a URL is a valid internal Keyman keyboard link
   * @param url String of the URL to parse
   * @return boolean
   */
  public boolean isKeymanKeyboardsLink(String url) {
    boolean status = false;
    if (url == null || url.isEmpty()) {
      return status;
    }
    Matcher matcher = keyboardPattern.matcher(url);
    if (matcher.matches()) {
      status = true;
    }

    return status;
  }
}
