/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmapro;

import androidx.annotation.NonNull;
import androidx.appcompat.app.ActionBar;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.Fragment;

import com.stepstone.stepper.BlockingStep;
import com.stepstone.stepper.StepperLayout;
import com.stepstone.stepper.VerificationError;
import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.WebViewUtils;

import android.graphics.Bitmap;
import android.os.Bundle;
import android.os.Handler;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.TextView;

import java.io.File;
import java.io.FileFilter;

/**
 * Fragment to display either readme.htm or welcome.htm in a WebView
 * If button is "INSTALL", callback to PackageActivity will install the package
 */
public class WebViewFragment extends Fragment implements BlockingStep {
  private File tempPackagePath;
  private String packageID;
  private String pkgTarget;
  private String pkgName;
  private String fileName;
  private boolean isInstallButton = false;
  private OnInstallClickedListener callback;

  public void setOnInstallClickedListener(OnInstallClickedListener callback) {
    this.callback = callback;
  }

  // This interface to be implemented by calling Activity
  public interface OnInstallClickedListener {
    public void onInstallClicked(String pkgTarget, String packageID);
  }

  @Override
  public View onCreateView(LayoutInflater inflater, ViewGroup container,
                           Bundle savedInstanceState) {
    View v = inflater.inflate(R.layout.fragment_webview_layout, container, false);
    Bundle bundle = getArguments();

    if (bundle != null) {
      if (bundle.containsKey("tempPackagePath")) {
        tempPackagePath = (File)bundle.getSerializable("tempPackagePath");
      }
      if (bundle.containsKey("packageID")) {
        packageID = bundle.getString("packageID");
      }
      if (bundle.containsKey("pkgTarget")) {
        pkgTarget = bundle.getString("pkgTarget");
      }
      if (bundle.containsKey("pkgName")) {
        pkgName = bundle.getString("pkgName");
      }
      if (bundle.containsKey("fileName")) {
        fileName = bundle.getString("fileName");
      }
      if (bundle.containsKey("isInstallButton")) {
        isInstallButton = bundle.getBoolean("isInstallButton");
      }
    }

    Toolbar toolbar = (Toolbar) v.findViewById(R.id.titlebar);
    ((AppCompatActivity)getActivity()).setSupportActionBar(toolbar);
    ActionBar actionBar = ((AppCompatActivity)getActivity()).getSupportActionBar();
    if (actionBar != null) {
      actionBar.setTitle(null);
      actionBar.setDisplayUseLogoEnabled(false);
      actionBar.setDisplayShowHomeEnabled(false);
      actionBar.setDisplayShowTitleEnabled(false);
      actionBar.setDisplayShowCustomEnabled(true);
      actionBar.setBackgroundDrawable(MainActivity.getActionBarDrawable(getContext()));
    }

    TextView packageActivityTitle = (TextView) v.findViewById(R.id.bar_title);
    packageActivityTitle.setWidth((int) getResources().getDimension(R.dimen.package_label_width));

    String titleStr = pkgTarget.equals(PackageProcessor.PP_TARGET_KEYBOARDS) ?
      getString(R.string.install_keyboard_package) :
      getString(R.string.install_predictive_text_package);
    if (FileUtils.isWelcomeFile(fileName)) {
      titleStr = String.format(getString(R.string.welcome_package), pkgName);
    }
    packageActivityTitle.setText(titleStr);

    WebView webView = (WebView) v.findViewById(R.id.packageWebView);
    webView.getSettings().setLayoutAlgorithm(WebSettings.LayoutAlgorithm.NORMAL);
    webView.getSettings().setJavaScriptEnabled(true);
    webView.getSettings().setAllowFileAccess(true);
    webView.getSettings().setUseWideViewPort(true);
    webView.getSettings().setLoadWithOverviewMode(true);
    webView.getSettings().setBuiltInZoomControls(true);
    webView.getSettings().setSupportZoom(true);
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
        if (url != null && !url.toLowerCase().equals("about:blank"))
          view.loadUrl(url);

        return true;
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

    // Determine if ad-hoc distributed KMP contains readme.htm/welcome.htm (case-insensitive) to display
    FileFilter _readmeFilter = new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        if (pathname.isFile() && FileUtils.isReadmeFile(pathname.getName())) {
          return true;
        }
        return false;
      }
    };
    FileFilter _welcomeFilter = new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        if (pathname.isFile() && FileUtils.isWelcomeFile(pathname.getName())) {
          return true;
        }
        return false;
      }
    };

    File[] files = (FileUtils.isReadmeFile(fileName)) ? tempPackagePath.listFiles(_readmeFilter) :
      tempPackagePath.listFiles(_welcomeFilter);
    if (files != null && files.length > 0 && files[0].exists() && files[0].length() > 0) {
      webView.loadUrl("file:///" + files[0].getAbsolutePath());
    } else {
      // No readme.htm so display minimal package information
      String targetString = "";
      if (pkgTarget.equals(PackageProcessor.PP_TARGET_KEYBOARDS)) {
        targetString = pkgName != null && pkgName.toLowerCase().endsWith("keyboard")
          ? "" : String.format(" %s", getResources().getQuantityString(R.plurals.title_keyboards, 1));
      } else if (pkgTarget.equals(PackageProcessor.PP_TARGET_LEXICAL_MODELS)) {
        targetString = pkgName != null && pkgName.toLowerCase().endsWith("model")
          ? "" :String.format(" %s", "model");
      }
      String htmlString = String.format(
        "<body style=\"max-width:600px;\"><H1>The %s%s Package</H1></body>",
        pkgName, targetString);
      // If welcome missing, don't load anything
      if (FileUtils.isReadmeFile(fileName)) {
        webView.loadData(htmlString, "text/html; charset=utf-8", "UTF-8");
      }
    }

    return v;

  }
  @Override
  public void onNextClicked(final StepperLayout.OnNextClickedCallback callback) {
    // Send data to calling Activity
    if (isInstallButton) {
      this.callback.onInstallClicked(pkgTarget, packageID);
    }

    new Handler().postDelayed(new Runnable() {
      @Override
      public void run() {
        //you can do anythings you want
        callback.goToNextStep();
      }
    }, 1000L);// delay open another fragment,
  }

  @Override
  public void onCompleteClicked(StepperLayout.OnCompleteClickedCallback callback) {
    // Send data to calling Activity (1-step stepper)
    if (isInstallButton) {
     this.callback.onInstallClicked(pkgTarget, packageID);
    }

    // Cleanup
    getActivity().finish();
  }

  @Override
  public void onBackClicked(StepperLayout.OnBackClickedCallback callback) {
    if (callback.getStepperLayout().getCurrentStepPosition() == 0) {
      // Cleanup after cancelling package installation
      MainActivity.cleanupPackageInstall();
    }
    callback.goToPrevStep();
  }
  @Override
  public VerificationError verifyStep() {
    return null;
  }
  @Override
  public void onSelected() {
  }
  @Override
  public void onError(@NonNull VerificationError error) {
  }
}