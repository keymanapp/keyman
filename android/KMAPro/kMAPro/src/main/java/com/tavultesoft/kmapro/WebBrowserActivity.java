/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.util.KMPLink;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Bitmap;
import android.graphics.Color;
import android.graphics.PorterDuff.Mode;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.util.Patterns;
import android.view.KeyEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnFocusChangeListener;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.ImageButton;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.TextView.OnEditorActionListener;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import androidx.appcompat.widget.Toolbar;
import androidx.appcompat.app.AppCompatActivity;

import static android.app.Application.getProcessName;

public class WebBrowserActivity extends BaseActivity {
  private static final String TAG = "WebBrowserActivity";
  private WebView webView;
  private EditText addressField;
  private ImageButton clearButton;
  private ImageButton stopButton;
  private ImageButton reloadButton;
  private ProgressBar progressBar;
  private static final String fontBaseUri = "https://s.keyman.com/font/deploy/";
  private String loadedFont;
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

    setContentView(R.layout.activity_web_browser);


    final Toolbar toolbar = findViewById(R.id.web_browser_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setTitle(null);
    getSupportActionBar().setDisplayUseLogoEnabled(false);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    getSupportActionBar().setDisplayShowCustomEnabled(true);
    getSupportActionBar().setBackgroundDrawable(MainActivity.getActionBarDrawable(this));

    webView = (WebView) findViewById(R.id.browserWebView);
    addressField = (EditText) findViewById(R.id.address_field);
    clearButton = (ImageButton) findViewById(R.id.clear_button);
    stopButton = (ImageButton) findViewById(R.id.stop_button);
    reloadButton = (ImageButton) findViewById(R.id.reload_button);
    final ImageButton backButton = (ImageButton) findViewById(R.id.backButton);
    final ImageButton forwardButton = (ImageButton) findViewById(R.id.forwardButton);
    final ImageButton bookmarksButton = (ImageButton) findViewById(R.id.bookmarksButton);
    final ImageButton globeButton = (ImageButton) findViewById(R.id.globeButton);
    final ImageButton closeButton = (ImageButton) findViewById(R.id.closeButton);
    progressBar = (ProgressBar) findViewById(R.id.progressBar);
    progressBar.setRotation(180);

    addressField.setOnFocusChangeListener(new OnFocusChangeListener() {
      @Override
      public void onFocusChange(View v, boolean hasFocus) {
        if (hasFocus) {
          if (addressField.length() > 0) {
            reloadButton.setVisibility(View.GONE);
            stopButton.setVisibility(View.GONE);
            clearButton.setVisibility(View.VISIBLE);
          } else {
            reloadButton.setVisibility(View.GONE);
            stopButton.setVisibility(View.GONE);
            clearButton.setVisibility(View.GONE);
          }
        } else {
          if (isLoading) {
            clearButton.setVisibility(View.GONE);
            reloadButton.setVisibility(View.GONE);
            stopButton.setVisibility(View.VISIBLE);
            addressField.setText(webView.getUrl());
          } else if (didFinishLoading) {
            clearButton.setVisibility(View.GONE);
            stopButton.setVisibility(View.GONE);
            reloadButton.setVisibility(View.VISIBLE);
            addressField.setText(webView.getUrl());
          }
        }
      }
    });

    addressField.addTextChangedListener(new TextWatcher() {
      @Override
      public void onTextChanged(CharSequence s, int start, int before, int count) {
        // Do nothing
      }

      @Override
      public void beforeTextChanged(CharSequence s, int start, int count, int after) {
        // Do nothing
      }

      @Override
      public void afterTextChanged(Editable s) {
        if (s.length() >= 1) {
          if (addressField.hasFocus()) {
            reloadButton.setVisibility(View.GONE);
            stopButton.setVisibility(View.GONE);
            clearButton.setVisibility(View.VISIBLE);
          }
        } else {
          if (addressField.hasFocus()) {
            reloadButton.setVisibility(View.GONE);
            stopButton.setVisibility(View.GONE);
            clearButton.setVisibility(View.GONE);
          } else {
            if (isLoading) {
              clearButton.setVisibility(View.GONE);
              reloadButton.setVisibility(View.GONE);
              stopButton.setVisibility(View.VISIBLE);
            } else if (didFinishLoading) {
              clearButton.setVisibility(View.GONE);
              stopButton.setVisibility(View.GONE);
              reloadButton.setVisibility(View.VISIBLE);
            }
          }
        }
      }
    });

    clearButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        addressField.setText("");
      }
    });

    stopButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        addressField.clearFocus();
        webView.stopLoading();
        updateButtons();
        didFinishLoading = true;
        isLoading = false;
        if (!addressField.hasFocus()) {
          clearButton.setVisibility(View.GONE);
          stopButton.setVisibility(View.GONE);
          reloadButton.setVisibility(View.VISIBLE);
        }

        loadFont();
      }
    });

    reloadButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        addressField.clearFocus();
        webView.reload();
      }
    });

    backButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        addressField.clearFocus();
        webView.goBack();
      }
    });

    forwardButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        addressField.clearFocus();
        webView.goForward();
      }
    });

    bookmarksButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        addressField.clearFocus();
        Intent i = new Intent(context, BookmarksActivity.class);
        i.putExtra("title", webView.getTitle());
        i.putExtra("url", webView.getUrl());
        startActivityForResult(i, 1);
      }
    });

    globeButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        addressField.clearFocus();
        KMManager.showKeyboardPicker(context, KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);
      }
    });

    closeButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        finish();
        overridePendingTransition(0, android.R.anim.fade_out);
      }
    });

    webView.getSettings().setLayoutAlgorithm(WebSettings.LayoutAlgorithm.NORMAL);
    webView.getSettings().setJavaScriptEnabled(true);
    // Intentionally disallow file:// access
    webView.getSettings().setAllowFileAccess(false);
    webView.getSettings().setUseWideViewPort(true);
    webView.getSettings().setLoadWithOverviewMode(true);
    webView.getSettings().setBuiltInZoomControls(true);
    webView.getSettings().setSupportZoom(true);
    webView.setLayerType(View.LAYER_TYPE_SOFTWARE, null);

    webView.setWebChromeClient(new WebChromeClient() {
      public void onProgressChanged(WebView view, int progress) {
        progressBar.setProgress(100 - progress);
      }
    });
    webView.setWebViewClient(new WebViewClient() {
      @Override
      public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
        updateButtons();
        didFinishLoading = true;
        isLoading = false;
        if (!addressField.hasFocus()) {
          clearButton.setVisibility(View.GONE);
          stopButton.setVisibility(View.GONE);
          reloadButton.setVisibility(View.VISIBLE);
        }
      }

      @Override
      public boolean shouldOverrideUrlLoading(WebView view, String url) {
        String lowerURL = url.toLowerCase();
        if (lowerURL.equals("about:blank")) {
          return true; // never load a blank page, e.g. when the component initializes
        }
        if (KMPLink.isKeymanDownloadLink(lowerURL)) {
          // KMAPro main activity will handle this intent
          // Pass original url because path and query are case-sensitive
          Intent intent = new Intent(Intent.ACTION_VIEW, Uri.parse(url));
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
        updateButtons();
        isLoading = true;
        didFinishLoading = false;
        addressField.setText(url);
        if (!addressField.hasFocus()) {
          clearButton.setVisibility(View.GONE);
          reloadButton.setVisibility(View.GONE);
          stopButton.setVisibility(View.VISIBLE);
        }
      }

      @Override
      public void onPageFinished(WebView view, String url) {
        updateButtons();
        didFinishLoading = true;
        isLoading = false;
        if (!addressField.hasFocus()) {
          clearButton.setVisibility(View.GONE);
          stopButton.setVisibility(View.GONE);
          reloadButton.setVisibility(View.VISIBLE);
        }

        loadFont();
      }
    });

    addressField.setOnEditorActionListener(new OnEditorActionListener() {
      @Override
      public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
        boolean handled = false;
        if (actionId == EditorInfo.IME_ACTION_GO || event.getKeyCode() == KeyEvent.KEYCODE_ENTER) {
          InputMethodManager imm = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
          imm.hideSoftInputFromWindow(addressField.getWindowToken(), 0);
          String urlStr = v.getText().toString();

          boolean valid = false;
          String originalUrl = urlStr;
          while(!valid) {
            try {
              new URL(urlStr);
              valid = true;
            } catch (MalformedURLException e) {
              // Intentionally not logging exception because it may be a query

              if (Patterns.WEB_URL.matcher("http://" + urlStr).matches()) {
                urlStr = "http://" + urlStr;
                valid = true;
              } else {
                try {
                  urlStr = String.format("https://www.google.com/search?q=%s",
                    URLEncoder.encode(originalUrl, StandardCharsets.UTF_8.displayName()));
                  valid = true; // no need to test again; we'll assume it's valid this time
                } catch (UnsupportedEncodingException ue) {
                  urlStr = String.format("https://www.google.com/search?q=%s", originalUrl);
                  break;
                }
              }
            }
          }
          webView.loadUrl(urlStr);
          addressField.clearFocus();
          handled = true;
        }

        return handled;
      }
    });

    updateButtons();
    addressField.clearFocus();

    // Load last visited Url
    SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = prefs.edit();
    String url = prefs.getString("lastVisitedUrl", "https://www.google.com/");
    webView.loadUrl(url);
  }

  @Override
  protected void onResume() {
    super.onResume();
    if (webView != null) {
      if (didFinishLoading) {
        String fontFilename = KMManager.getKeyboardTextFontFilename();
        if (!loadedFont.equals(fontFilename)) {
          webView.reload();
        }
      }
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

  private void updateButtons() {
    ImageButton backButton = (ImageButton) findViewById(R.id.backButton);
    ImageButton forwardButton = (ImageButton) findViewById(R.id.forwardButton);

    setImageButtonEnabled(backButton, R.drawable.ic_navigation_back, webView.canGoBack());
    setImageButtonEnabled(forwardButton, R.drawable.ic_navigation_forward, webView.canGoForward());
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
