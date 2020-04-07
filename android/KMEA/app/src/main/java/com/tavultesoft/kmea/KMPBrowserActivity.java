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
import android.view.View;
import android.widget.ImageButton;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import androidx.appcompat.app.AppCompatActivity;

public class KMPBrowserActivity extends AppCompatActivity {

  private WebView webView;
  private static final String fontBaseUri = "https://s.keyman.com/font/deploy/";
  private String loadedFont;
  private static final String KMP_SEARCH_BASE_URL = "https://keyman.com/keyboards?embed=linux&version="; // TODO: Update to Android
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
        //progressBar.setProgress(100 - progress);
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
        if (!url.toLowerCase().equals("about:blank")) {
          if (url.startsWith("keyman:download")) {
            // KMAPro main activity will handle this intent
            Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
            startActivityForResult(intent, 1);
          } else {
            view.loadUrl(url);
          }
        }
        return true;
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

        loadFont();
      }
    });

    String appVersion = KMManager.getVersion();
    String kmpSearchUrl = String.format("%s%s", KMP_SEARCH_BASE_URL, appVersion);
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

  private void loadFont() {
    String font = KMManager.getKeyboardTextFontFilename();
    if (!font.isEmpty()) {
      loadedFont = font;
      String fontUrl = String.format("%s%s", fontBaseUri, font);
      String jsStr = String.format(
        "var style = document.createElement('style');" +
          "style.type = 'text/css';" +
          "style.innerHTML = '@font-face{font-family:\"KMCustomFont\";src:url(\"%s\");} " +
          "*{font-family:\"KMCustomFont\" !important;}';" +
          "document.getElementsByTagName('head')[0].appendChild(style);", fontUrl);
      webView.loadUrl(String.format("javascript:%s", jsStr));
    } else {
      loadedFont = "sans-serif";
      String jsStr = "var style = document.createElement('style');" +
        "style.type = 'text/css';" +
        "style.innerHTML = '*{font-family:\"sans-serif\" !important;}';" +
        "document.getElementsByTagName('head')[0].appendChild(style);";
      webView.loadUrl(String.format("javascript:%s", jsStr));
    }
  }

  private void setImageButtonEnabled(ImageButton imgButton, int resId, boolean enabled) {
    imgButton.setEnabled(enabled);
    Drawable originalIcon;
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
      originalIcon = getResources().getDrawable(resId, getTheme());
    } else {
      originalIcon = getResources().getDrawable(resId);
    }
    Drawable icon = enabled ? originalIcon : convertDrawableToGrayScale(originalIcon);
    imgButton.setImageDrawable(icon);
  }

  private static Drawable convertDrawableToGrayScale(Drawable drawable) {
    if (drawable == null)
      return null;

    Drawable drw = drawable.mutate();
    drw.setColorFilter(Color.LTGRAY, Mode.SRC_IN);
    return drw;
  }
}