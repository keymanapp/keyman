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

import android.util.Log;
import android.widget.Toast;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.FileUtils;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class PackageActivity extends Activity{

  private WebView webView;
  private File kmpFile;
  private File tempPackagePath;
  private List<Map<String, String>> installedPackageKeyboards;

  private static ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> kbDownloadEventListeners = null;

  @SuppressLint({"SetJavaScriptEnabled", "InflateParams"})
  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final Context context = this;
    Bundle bundle = getIntent().getExtras();
    if (bundle != null) {
      kmpFile = new File(bundle.getString("kmpFile"));
    }

    final String pkgId = PackageProcessor.getPackageName(kmpFile);
    String version = PackageProcessor.getPackageVersion(kmpFile, false);

    try {
      tempPackagePath = PackageProcessor.unzipKMP(kmpFile);
    } catch (Exception e) {
      Log.e("PackageActivity", "Failed to extract " + kmpFile.getAbsolutePath());
      finish();
    }

    final ActionBar actionBar = getActionBar();
    actionBar.setLogo(R.drawable.keyman_logo);
    actionBar.setDisplayShowHomeEnabled(false);
    actionBar.setDisplayShowTitleEnabled(false);
    actionBar.setDisplayShowCustomEnabled(true);
    actionBar.setBackgroundDrawable(MainActivity.getActionBarDrawable(this));
    TextView packgeActivityTitle = new TextView(this);
    packgeActivityTitle.setWidth((int) getResources().getDimension(R.dimen.package_label_width));
    packgeActivityTitle.setTextSize(getResources().getDimension(R.dimen.package_label_textsize));
    packgeActivityTitle.setGravity(Gravity.CENTER);

    String titleStr = "Install Keyboard Package";
    if (version != null) {
      titleStr += " v." + version;
    }
    packgeActivityTitle.setText(titleStr);
    actionBar.setCustomView(packgeActivityTitle);

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
    webView.getSettings().setTextZoom(200);
    webView.setVerticalScrollBarEnabled(true);
    webView.setHorizontalScrollBarEnabled(true);
    webView.setLayerType(View.LAYER_TYPE_SOFTWARE, null);

    webView.setWebChromeClient(new WebChromeClient() {
    });
    webView.setWebViewClient(new WebViewClient() {
      @Override
      public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
      }

      @Override
      public boolean shouldOverrideUrlLoading(WebView view, String url) {
        if (!url.toLowerCase().equals("about:blank"))
          view.loadUrl(url);

        return true;
      }

      @Override
      public void onPageStarted(WebView view, String url, Bitmap favicon) {
      }

      @Override
      public void onPageFinished(WebView view, String url) {
      }
    });

    // Determine if KMP contains welcome.htm (case-insensitive) to display
    FileFilter welcomeFilter = new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        if (pathname.isFile() && pathname.getName().equalsIgnoreCase("welcome.htm")) {
          return true;
        }
        return false;
      }
    };

    File[] files = tempPackagePath.listFiles(welcomeFilter);
    if (files.length > 0 && files[0].exists() && files[0].length() > 0) {
      webView.loadUrl("file:///" + files[0].getAbsolutePath());
    } else {
      String htmlString = "<div id='welcome'><H1>Package: " + pkgId + "</H1></div>";
      webView.loadData(htmlString, "text/html; charset=utf-8", "UTF-8");
    }

    installButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        try {
          // Remove currently installed package and then install
          File currentPackage = new File(context.getDir("data", MODE_PRIVATE).toString() +
            File.separator + KMManager.KMDefault_AssetPackages + File.separator + pkgId);
          if (currentPackage.exists()) {
            FileUtils.deleteDirectory(currentPackage);
          }
          installedPackageKeyboards = PackageProcessor.processKMP(kmpFile);
          // Do the notifications!
          boolean success = installedPackageKeyboards.size() != 0;
          if(success) {
            notifyPackageInstallListeners(KeyboardEventHandler.EventType.PACKAGE_INSTALLED, installedPackageKeyboards, 1);
            if(installedPackageKeyboards != null) {
              notifyPackageInstallListeners(KeyboardEventHandler.EventType.PACKAGE_INSTALLED, installedPackageKeyboards, 1);
            }
          } else {
            Toast.makeText(context, "No new keyboards installed", Toast.LENGTH_SHORT).show();
            Log.d("PackageActivity", "Package install returned no updated keyboards!");
          }

        } catch (Exception e) {
          Log.e("PackageActivity", "Error " + e);
        } finally {
          cleanup();
        }
      }
    });

    cancelButton.setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        cleanup();
      }
    });


  }

  private void cleanup() {
    try {
      if (kmpFile.exists()) {
        kmpFile.delete();
      }
      if (tempPackagePath.exists()) {
        FileUtils.deleteDirectory(tempPackagePath);
      }
    } catch (Exception e) {

    } finally {
      finish();
    }
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

  void notifyPackageInstallListeners(KeyboardEventHandler.EventType eventType, List<Map<String, String>> keyboards, int result) {
    if (kbDownloadEventListeners != null) {
      KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, eventType, keyboards, result);
    }
  }

  public static void addKeyboardDownloadEventListener(KeyboardEventHandler.OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners == null) {
      kbDownloadEventListeners = new ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener>();
    }

    if (listener != null && !kbDownloadEventListeners.contains(listener)) {
      kbDownloadEventListeners.add(listener);
    }
  }

  public static void removeKeyboardDownloadEventListener(KeyboardEventHandler.OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners != null) {
      kbDownloadEventListeners.remove(listener);
    }
  }

}
