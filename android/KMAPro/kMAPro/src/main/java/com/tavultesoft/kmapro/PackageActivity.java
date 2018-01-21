package com.tavultesoft.kmapro;

import android.annotation.SuppressLint;
import android.app.ActionBar;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Bundle;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnClickListener;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.Button;
import android.widget.TextView;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;

import java.io.File;

public class PackageActivity extends Activity{

  private WebView webView;
  private static final String fontBaseUri = "https://s.keyman.com/font/deploy/";
  private String loadedFont;
  private boolean isLoading = false;
  private boolean didFinishLoading = false;
  private String kmpPath;
  private String packageID;
  private String keyboardRoot;

  @SuppressLint({"SetJavaScriptEnabled", "InflateParams"})
  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;

    Bundle bundle = getIntent().getExtras();
    if (bundle != null) {
      kmpPath = bundle.getString("filePath");
      packageID = bundle.getString(KMKeyboardDownloaderActivity.ARG_PKG_ID);
    }
    if (packageID == KMManager.KMDefault_UndefinedPackageID) {
      keyboardRoot = context.getDir("data", Context.MODE_PRIVATE).toString() +
        File.separator + KMManager.KMDefault_UndefinedPackageID + File.separator;
    } else {
      keyboardRoot = context.getDir("data", Context.MODE_PRIVATE).toString() +
        File.separator + KMManager.KMDefault_AssetPackages + File.separator + packageID + File.separator;
    }

    final ActionBar actionBar = getActionBar();
    actionBar.setLogo(R.drawable.keyman_logo);
    actionBar.setDisplayShowHomeEnabled(false);
    actionBar.setDisplayShowTitleEnabled(false);
    actionBar.setDisplayShowCustomEnabled(true);
    actionBar.setBackgroundDrawable(MainActivity.getActionBarDrawable(this));
    TextView packageName = new TextView(this);
    packageName.setWidth((int) getResources().getDimension(R.dimen.package_label_width));
    packageName.setTextSize(getResources().getDimension(R.dimen.package_label_textsize));
    packageName.setGravity(Gravity.CENTER);

    String packageStr = "Install Keyboard Package";
    packageName.setText(packageStr);
    actionBar.setCustomView(packageName);

    setContentView(R.layout.activity_package_installer);

    final Button installButton = (Button) findViewById(R.id.installButton);
    final Button cancelButton = (Button) findViewById(R.id.cancelButton);

    webView = (WebView) findViewById(R.id.webView);
    webView.getSettings().setLayoutAlgorithm(WebSettings.LayoutAlgorithm.NORMAL);
    webView.getSettings().setJavaScriptEnabled(true);
    webView.getSettings().setUseWideViewPort(true);
    webView.getSettings().setLoadWithOverviewMode(true);
    webView.getSettings().setBuiltInZoomControls(true);
    webView.getSettings().setSupportZoom(true);
    webView.getSettings().setTextZoom(2);
    webView.setVerticalScrollBarEnabled(true);
    webView.setHorizontalScrollBarEnabled(true);
    webView.setLayerType(View.LAYER_TYPE_SOFTWARE, null);

    webView.setWebChromeClient(new WebChromeClient() {
    });
    webView.setWebViewClient(new WebViewClient() {
      @Override
      public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
        didFinishLoading = true;
        isLoading = false;
      }

      @Override
      public boolean shouldOverrideUrlLoading(WebView view, String url) {
        if (!url.toLowerCase().equals("about:blank"))
          view.loadUrl(url);

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
      }
    });

    File welcome = new File(keyboardRoot, "Welcome.htm");
    if (welcome.exists()) {
      webView.loadUrl("file:///" + welcome.getAbsolutePath());
    } else {
      // TODO: loadURL(welcome.htm) after KMP is processed
      String url = "https://www.google.com/";
      webView.loadUrl(url);
    }

    installButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        // Process KMP package
      }
    });

    cancelButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        finish();
      }
    });
  }

  @Override
  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
  }

  @Override
  public void onBackPressed() {
    finish();
    overridePendingTransition(0, android.R.anim.fade_out);
  }

}
