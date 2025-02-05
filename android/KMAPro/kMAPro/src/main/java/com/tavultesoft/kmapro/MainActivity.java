/**
 * Copyright (C) 2018-2021 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import static android.content.pm.PackageManager.PERMISSION_GRANTED;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.keyman.android.CheckInstallReferrer;
import com.keyman.android.BannerController;
import com.keyman.engine.BaseActivity;
import com.keyman.engine.KMHelpFileActivity;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KMManager.KeyboardType;
import com.keyman.engine.KmpInstallMode;
import com.keyman.engine.KMTextView;
import com.keyman.engine.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.keyman.engine.KeyboardEventHandler.OnKeyboardEventListener;
import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.CloudDownloadMgr;
import com.keyman.engine.cloud.impl.CloudLexicalModelMetaDataDownloadCallback;
import com.keyman.engine.data.CloudRepository;
import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.LexicalModel;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.DownloadFileUtils;
import com.keyman.android.DownloadIntentService;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMPLink;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.WebViewUtils;
import com.keyman.engine.util.WebViewUtils.EngineWebViewVersionStatus;
import com.keyman.engine.util.WebViewUtils.SystemWebViewStatus;

import android.Manifest;
import android.app.ProgressDialog;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.net.Uri.Builder;
import android.os.Build;
import android.os.Bundle;
import android.os.Environment;
import android.os.Handler;
import android.os.ParcelFileDescriptor;
import android.os.Parcelable;
import android.annotation.SuppressLint;
import android.annotation.TargetApi;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.widget.Toolbar;
import androidx.appcompat.app.AlertDialog;
import android.content.ClipData;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.ResolveInfo;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.Rect;
import android.graphics.RectF;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;

import android.provider.Settings;
import android.text.Html;
import android.util.Log;
import android.util.TypedValue;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.webkit.WebView;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.SeekBar;
import android.widget.TextView;
import android.widget.Toast;

import io.sentry.android.core.SentryAndroid;

public class MainActivity extends BaseActivity implements OnKeyboardEventListener, OnKeyboardDownloadEventListener,
    ActivityCompat.OnRequestPermissionsResultCallback {
  public static Context context;

  // Fields used for installing kmp packages
  public static final int PERMISSION_REQUEST_STORAGE = 0;
  public static final int READ_REQUEST_CODE = 42;

  private static final String TAG = "MainActivity";

  private KMTextView textView;
  private final int minTextSize = 16;
  private final int maxTextSize = 72;
  private int textSize = minTextSize;
  private int lastOrientation = Configuration.ORIENTATION_UNDEFINED;
  private static final String userTextKey = "UserText";
  private static final String userTextSizeKey = "UserTextSize";
  private Toolbar toolbar;
  private Menu menu;

  private static Dataset repo;
  private boolean didExecuteParser = false;

  DownloadResultReceiver resultReceiver;
  private static ProgressDialog progressDialog;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    setTheme(R.style.AppTheme);
    super.onCreate(savedInstanceState);
    context = this;

    checkSendCrashReport();
    if (KMManager.getMaySendCrashReport()) {
      SentryAndroid.init(context, options -> {
        options.setEnableAutoSessionTracking(false);
        options.setRelease(com.tavultesoft.kmapro.BuildConfig.VERSION_GIT_TAG);
        options.setEnvironment(com.tavultesoft.kmapro.BuildConfig.VERSION_ENVIRONMENT);
      });
    }

    resultReceiver = new DownloadResultReceiver(new Handler(), context);

    if (BuildConfig.DEBUG) {
      KMManager.setDebugMode(true);
    }

    // Verify WebView installed and enabled before attempting to initialize KMManager
    KMManager.initialize(getApplicationContext(), KeyboardType.KEYBOARD_TYPE_INAPP);
    KMManager.executeResourceUpdate(this);

    DefaultLanguageResource.install(context);

    SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    KMManager.SpacebarText spacebarText = KMManager.SpacebarText.fromString(prefs.getString(KeymanSettingsActivity.spacebarTextKey, KMManager.SpacebarText.LANGUAGE_KEYBOARD.toString()));
    KMManager.setSpacebarText(spacebarText);

    checkHapticFeedback();

    setContentView(R.layout.activity_main);

    toolbar = (Toolbar) findViewById(R.id.titlebar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setTitle(null);
    getSupportActionBar().setDisplayUseLogoEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setLogo(R.drawable.keyman_logo);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    getSupportActionBar().setBackgroundDrawable(getActionBarDrawable(this));

    textView = (KMTextView) findViewById(R.id.kmTextView);
    textView.setText(prefs.getString(userTextKey, ""));
    textSize = prefs.getInt(userTextSizeKey, minTextSize);
    textView.setTextSize((float) textSize);
    textView.setSelection(textView.getText().length());

    // Check WebView enabled and Chrome version
    checkWebViewAndChromeVersion(context);
    CheckInstallReferrer.checkGooglePlayInstallReferrer(this, context);
    checkGetStarted();
  }

  @TargetApi(Build.VERSION_CODES.JELLY_BEAN)
  @Override
  public void onActivityResult(int requestCode, int resultCode, Intent returnIntent) {
    super.onActivityResult(requestCode, resultCode, returnIntent);

    if (resultCode != RESULT_OK) {
      checkGetStarted();
      return;
    } else {
      ClipData userdata = returnIntent.getClipData();
      int len = userdata.getItemCount();
      for (int i = 0; i < len; i++) {
        Uri fileUri = userdata.getItemAt(i).getUri();
        try {
          String filename = fileUri.getLastPathSegment();
          ParcelFileDescriptor pfd = getContentResolver().openFileDescriptor(fileUri, "r");
          FileInputStream inputStream = new FileInputStream(pfd.getFileDescriptor());

          if (filename.endsWith(".dat")) {
            File newFile = new File(getDir("userdata", Context.MODE_PRIVATE), filename);
            copyFile(inputStream, newFile);
            inputStream.close();
          } else if (FileUtils.hasJavaScriptExtension(filename) || FileUtils.hasFontExtension(filename)) {
            File packagesDir = new File(getDir("data", Context.MODE_PRIVATE) + File.separator +
              KMManager.KMDefault_AssetPackages);
            if (!packagesDir.exists()) {
              packagesDir.mkdir();
            }

            File undefinedPackageDir = new File(getDir("data", Context.MODE_PRIVATE) +
              File.separator + KMManager.KMDefault_UndefinedPackageID);
            if (!undefinedPackageDir.exists()) {
              undefinedPackageDir.mkdir();
            }
            File newFile = new File(undefinedPackageDir, filename);
            copyFile(inputStream, newFile);
            inputStream.close();
          }
        } catch (Exception e) {
          KMLog.LogException(TAG, "", e);
        }
      }

      checkGetStarted();
    }
  }

  @Override
  protected void onResume() {
    super.onResume();
    KMManager.onResume();
    KMManager.hideSystemKeyboard();

    // onConfigurationChanged() only triggers when device is rotated while app is in foreground
    // This handles when device is rotated while app is in background
    // using KMManager.getOrientation() since getConfiguration().orientation is unreliable #10241
    int newOrientation = KMManager.getOrientation(context);
    if (newOrientation != lastOrientation) {
      lastOrientation = newOrientation;
      Configuration newConfig = this.getResources().getConfiguration();
      KMManager.onConfigurationChanged(newConfig);
    }
    resizeTextView(textView.isKeyboardVisible());

    KMManager.addKeyboardEventListener(this);
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);
    PackageActivity.addKeyboardDownloadEventListener(this);

    CheckInstallReferrer.checkGooglePlayInstallReferrer(this, context);

    Intent intent = getIntent();
    Uri loadingIntentUri = intent.getData();

    if (loadingIntentUri != null) {
      String scheme = loadingIntentUri.getScheme().toLowerCase();
      final String link = loadingIntentUri.toString();
      switch (scheme) {
        // content:// Android DownloadManager
        // file:// Chrome downloads and Filebrowsers
        case "content":
        case "file":
          requestPermissionIntentUri = loadingIntentUri;
          if (CheckPermissions.isPermissionOK(this)) {
            KMLog.LogBreadcrumb(TAG, "Installing local KMP: " + link, true);
            useLocalKMP(context, loadingIntentUri);
          } else {
            CheckPermissions.requestPermission(this, context);
          }
          break;
        case "http" :
        case "https" :
          // Might need to modify link like KMPBrowserActivity
          if (KMPLink.isKeymanInstallLink(link)) {
            loadingIntentUri = KMPLink.getKeyboardDownloadLink(link);
          }
          // Is usually triggered by KMPBrowserActivity
          KMLog.LogBreadcrumb(TAG, "Installing via HTTPS intent: " + link, true);
          downloadKMP(loadingIntentUri, KmpInstallMode.Full);
          break;
        case "keyman" :
          // TODO: Only accept download links from Keyman browser activities when universal links work
          KMLog.LogBreadcrumb(TAG, "Installing via keyman-link intent: " + link, true);
          if (KMPLink.isKeymanDownloadLink(loadingIntentUri.toString())) {

            // Convert opaque URI to hierarchical URI so the query parameters can be parsed
            Builder builder = new Uri.Builder();
            builder.scheme("https")
              .authority("keyman.com")
              .appendPath("keyboards")
              .encodedQuery(loadingIntentUri.getEncodedQuery());
            loadingIntentUri = Uri.parse(builder.build().toString());
            downloadKMP(loadingIntentUri, KmpInstallMode.Full);
          } else if (KMPLink.isLegacyKeymanDownloadLink(loadingIntentUri.toString())) {
            loadingIntentUri = KMPLink.getLegacyKeyboardDownloadLink(link);
            downloadKMP(loadingIntentUri, KmpInstallMode.Full);
          } else {
            String msg = "Unrecognized scheme: " + scheme;
            KMLog.LogError(TAG, msg);
          }
          break;
        default :
          String msg = "Unrecognized scheme: " + scheme;
          KMLog.LogError(TAG, msg);
      }
    }
    intent.setData(null);
  }

  @Override
  protected void onPause() {
    super.onPause();
    KMManager.onPause();
    KMManager.removeKeyboardEventListener(this);

    // Intentionally not removing KeyboardDownloadEventListener to
    // ensure onKeyboardDownloadFinished() gets called

    SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = prefs.edit();
    editor.putString(userTextKey, textView.getText().toString());
    editor.putInt(userTextSizeKey, textSize);
    editor.commit();
  }

  @Override
  protected void onNewIntent(Intent intent) {
    super.onNewIntent(intent);
    setIntent(intent);
  }

  @Override
  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);
    KMManager.onConfigurationChanged(newConfig);
    getSupportActionBar().setBackgroundDrawable(getActionBarDrawable(this));
    resizeTextView(textView.isKeyboardVisible());
    invalidateOptionsMenu();
    lastOrientation = newConfig.orientation;
  }

  @SuppressLint("RestrictedApi")
  @Override
  public boolean onPrepareOptionsMenu(final Menu menu) {
    this.menu = menu;
    final MenuItem _overflowMenuItem = menu.findItem(R.id.action_overflow);
    if (_overflowMenuItem != null) {
      MenuItem updateKeyboards = this.menu.findItem(R.id.action_update_keyboards);
      updateUpdateCountIndicator(updateKeyboards,
        KMManager.getUpdateTool().getOpenUpdateCount(), true);
    }
    return super.onPrepareOptionsMenu(menu);
  }

  private void updateUpdateCountIndicator(int anUpdateCount) {
    if (menu == null) {
      return;
    }
    final MenuItem _overflowMenuItem = menu.findItem(R.id.action_overflow);
    if (_overflowMenuItem != null) {
      updateUpdateCountIndicator(_overflowMenuItem, anUpdateCount, false);
    }

    final MenuItem _keyboardupdate = menu.findItem(R.id.action_update_keyboards);
    if (_keyboardupdate != null && anUpdateCount > 0) {
      _keyboardupdate.setVisible(true);
    }
  }

  private void updateUpdateCountIndicator(MenuItem theItem, int anUpdateCount, boolean aHideMenuitem)
  {
    final ViewGroup _rootView = (ViewGroup) theItem.getActionView();

    if(anUpdateCount==0) {
      if (aHideMenuitem) {
        theItem.setVisible(false);
      } else if (_rootView != null) {
        _rootView.findViewById(R.id.update_count_indicator).setVisibility(View.GONE);
      }
    } else {
      if(aHideMenuitem) {
        theItem.setVisible(true);
      } else if(_rootView!=null) {
        _rootView.findViewById(R.id.update_count_indicator).setVisibility(View.VISIBLE);
      }
    }

    if(_rootView==null) {
      return;
    }

    TextView _t = _rootView.findViewById(R.id.update_count_indicator);
    _t.setText(String.valueOf(anUpdateCount));
  }

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    // Inflate the menu; this adds items to the action bar if it is present.
    getMenuInflater().inflate(R.menu.main, menu);
    this.menu = menu;

    KMManager.getUpdateTool().addPropertyChangeListener(new PropertyChangeListener()
      {
        @Override
        public void propertyChange(PropertyChangeEvent evt) {
          if(!evt.getPropertyName().equals("updateCount"))
            return;
          updateUpdateCountIndicator(
            evt.getNewValue()==null?0:(Integer) evt.getNewValue());
        }
      }
    );
    updateUpdateCountIndicator(KMManager.getUpdateTool().getOpenUpdateCount());
    return true;
  }

  @Override
  public boolean onOptionsItemSelected(MenuItem item) {
    switch (item.getItemId()) {
      case R.id.action_info:
        showInfo();
        return true;
      case R.id.action_share:
        showShareDialog();
        return true;
      /* Disable Web Browser to investigate Google sign-in
      case R.id.action_web:
        showWebBrowser();
        return true;*/
      case R.id.action_text_size:
        showTextSizeDialog();
        return true;
      case R.id.action_clear_text:
        showClearTextDialog();
        return true;
      case R.id.action_get_started:
        showGetStarted();
        return true;
      case R.id.action_settings:
        showSettings();
        return true;
      case R.id.action_update_keyboards:
        KMManager.getUpdateTool().executeOpenUpdates();
        // Dismiss icon
        updateUpdateCountIndicator(0);
        final MenuItem _keyboardupdate = menu.findItem(R.id.action_update_keyboards);
        if (_keyboardupdate != null && _keyboardupdate.isVisible()) {
          _keyboardupdate.setVisible(false);
        }

        return true;
      default:
        return super.onOptionsItemSelected(item);
    }
  }

  @Override
  public boolean onKeyUp(int keycode, KeyEvent e) {
    switch (keycode) {
      case KeyEvent.KEYCODE_MENU:
        menu.performIdentifierAction(R.id.action_overflow, Menu.FLAG_PERFORM_NO_CLOSE);
        return true;
    }

    return super.onKeyUp(keycode, e);
  }

  @Override
  public void onKeyboardLoaded(KeyboardType keyboardType) {
    // Initialize keyboard options
    KMManager.sendOptionsToKeyboard();
  }

  @Override
  public void onKeyboardChanged(String newKeyboard) {
    textView.setTypeface(KMManager.getKeyboardTextFontTypeface(this));
  }

  @Override
  public void onKeyboardShown() {
    // Refresh banner theme
    BannerController.setHTMLBanner(this, KeyboardType.KEYBOARD_TYPE_INAPP);
    resizeTextView(true);
  }

  @Override
  public void onKeyboardDismissed() {
    resizeTextView(false);
  }

  public void downloadKMP(String packageId, String bcp47, KmpInstallMode installMode) {
    Uri downloadUri = bcp47 == null ?
      Uri.parse(KMString.format("https://keyman.com/go/package/download/%s", new Object[]{packageId})) :
      Uri.parse(KMString.format("https://keyman.com/go/package/download/%s?bcp47=%s", new Object[]{packageId, bcp47}));
    downloadKMP(downloadUri, installMode);
  }

  /**
   * Parse the URI data to determine the filename and URL for the .kmp keyboard package.
   * If URL is valid, download the kmp.
   * @param packageUri URI to download the package.
   * @param installMode KMP installation mode (silent, welcome only, or full)
   * TODO: only ever pass packageId and bcp47 from callers, as KMPLink should be responsible for
   *       URL parsing, not this function.
   */
  public void downloadKMP(Uri packageUri, KmpInstallMode installMode) {
    if (packageUri == null) {
      KMLog.LogError(TAG,"null uri passed to downloadKmp");
      String message = "Download failed. Invalid URL.";
      Toast.makeText(getApplicationContext(), message,
        Toast.LENGTH_SHORT).show();
      return;
    }
    try {
      // Initial try with Keyman 13.0 download link, format keyman://localhost/install?url=...
      String url = packageUri.getQueryParameter(KMKeyboardDownloaderActivity.KMKey_URL);
      if (url == null) {
        // Otherwise, Keyman 14+ download link is format https://keyman.com/go/package/download/<package>[?bcp47=<code>][&...]
        url = packageUri.toString();
      }
      if (url != null) {
        // Create filename by extracting packageID from query or urlNoQuery
        String filename = packageUri.getQueryParameter("id");
        if (filename == null) {
          String urlNoQuery = packageUri.getPath();
          filename = FileUtils.getFilename(urlNoQuery);
        }
        if (!filename.endsWith(FileUtils.KEYMANPACKAGE)) {
          filename += FileUtils.KEYMANPACKAGE;
        }

        // Parse query for the BCP 47 language ID
        String languageID = packageUri.getQueryParameter(KMKeyboardDownloaderActivity.KMKey_BCP47);
        // TODO: Using "tag" for now, but production will be KMKeyboardDownloaderActivity.KMKey_BCP47
        if (languageID == null) {
          languageID = packageUri.getQueryParameter("tag");
        }

        url = url.toLowerCase();
        try {
          if (progressDialog == null) {
            Intent downloadIntent = new Intent(MainActivity.this, DownloadIntentService.class);
            downloadIntent.putExtra("url", url);
            downloadIntent.putExtra("filename", filename);
            downloadIntent.putExtra("language", languageID);
            downloadIntent.putExtra("destination", MainActivity.this.getCacheDir().toString());
            downloadIntent.putExtra("receiver", resultReceiver);
            downloadIntent.putExtra("installMode", installMode.toString());

            progressDialog = new ProgressDialog(MainActivity.this);
            progressDialog.setMessage(String.format(getString(R.string.downloading_keyboard_package), filename));
            progressDialog.setCancelable(true); // Cancelable in case there's exceptions
            progressDialog.show();
            progressDialog.setOnCancelListener(new DialogInterface.OnCancelListener() {
              @Override
              public void onCancel(DialogInterface dialog) {
                stopService(downloadIntent);
                cleanupPackageInstall();
              }
            });

            // Download the KMP to app cache
            startService(downloadIntent);
          }
        } catch (Exception e) {
          KMLog.LogException(TAG, "", e);
          cleanupPackageInstall();
          return;//break;
        }
      } else {
        String message = "Download failed. Not a .kmp keyboard package.";
        Toast.makeText(getApplicationContext(), message,
          Toast.LENGTH_SHORT).show();
      }
    } catch (UnsupportedOperationException e) {
      String message = "Download failed. Invalid URL.";
      KMLog.LogException(TAG,  message, e);
      Toast.makeText(getApplicationContext(), message,
        Toast.LENGTH_SHORT).show();
      cleanupPackageInstall();
    }
  }

  public void resizeTextView(boolean isKeyboardVisible) {
    int bannerHeight = 0;
    int keyboardHeight = 0;
    if (isKeyboardVisible) {
      bannerHeight = KMManager.getBannerHeight(this);
      keyboardHeight = KMManager.getKeyboardHeight(this);
    }

    TypedValue outValue = new TypedValue();
    getTheme().resolveAttribute(android.R.attr.actionBarSize, outValue, true);
    int actionBarHeight = getResources().getDimensionPixelSize(outValue.resourceId);

    // *** TO DO: Try to check if status bar is visible, set statusBarHeight to 0 if it is not visible ***
    int statusBarHeight = 0;
    int resourceId = getResources().getIdentifier("status_bar_height", "dimen", "android");
    if (resourceId > 0)
      statusBarHeight = getResources().getDimensionPixelSize(resourceId);

    Point size = KMManager.getWindowSize(context);
    int screenHeight = size.y;
    Log.d(TAG, "Main resizeTextView bannerHeight: " + bannerHeight);
    textView.setHeight(screenHeight - statusBarHeight - actionBarHeight - bannerHeight - keyboardHeight);
  }

  private void showInfo() {
    Intent i = new Intent(this, InfoActivity.class);
    i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
    startActivity(i);
    overridePendingTransition(android.R.anim.fade_in, R.anim.hold);
  }

  private void showWebBrowser() {
    // Telemetry for in-app browser usage.
    // Logging here because WebBrowserActivity is launched in a separate process.
    KMLog.LogInfo(TAG, "WebBrowserActivity launched");

    Intent i = new Intent(this, WebBrowserActivity.class);
    i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
    startActivity(i);
    overridePendingTransition(android.R.anim.fade_in, R.anim.hold);
  }

  private void showShareDialog() {
    // Sharing text content via send intent
    // Reference: https://developer.android.com/training/sharing/send#send-text-content
    // Note: Sharing to Facebook removed in https://github.com/keymanapp/keyman/issues/156
    // Users can copy text or use Keyman as system keyboard for typing into Facebook
    Intent sendIntent = new Intent(Intent.ACTION_SEND);
    sendIntent.setType("text/plain");
    sendIntent.putExtra(Intent.EXTRA_TEXT, textView.getText().toString());

    Intent shareIntent = Intent.createChooser(sendIntent, null);
    startActivity(shareIntent);
  }

  @SuppressLint("InflateParams")
  private void showTextSizeDialog() {
    LayoutInflater inflater = LayoutInflater.from(MainActivity.this);
    final View textSizeController = inflater.inflate(R.layout.text_size_controller, null);
    final AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(MainActivity.this);
    dialogBuilder.setIcon(R.drawable.ic_light_action_textsize);
    dialogBuilder.setTitle(getTextSizeString());
    dialogBuilder.setView(textSizeController);
    dialogBuilder.setPositiveButton(getString(R.string.label_ok), new DialogInterface.OnClickListener() {
      @Override
      public void onClick(DialogInterface dialog, int which) {
        // Done
      }
    });

    final AlertDialog dialog = dialogBuilder.create();
    dialog.show();

    final SeekBar seekBar = (SeekBar) dialog.findViewById(R.id.seekBar);
    seekBar.setProgress(textSize - minTextSize);
    seekBar.setMax(maxTextSize - minTextSize);
    seekBar.setOnSeekBarChangeListener(new SeekBar.OnSeekBarChangeListener() {

      @Override
      public void onStopTrackingTouch(SeekBar seekBar) {
        // Do nothing
      }

      @Override
      public void onStartTrackingTouch(SeekBar seekBar) {
        // Do nothing
      }

      @Override
      public void onProgressChanged(SeekBar seekBar, int progress, boolean fromUser) {
        textSize = progress + minTextSize;
        textView.setTextSize((float) textSize);
        dialog.setTitle(getTextSizeString());
      }
    });

    dialog.findViewById(R.id.textSizeDownButton).setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        if (textSize > minTextSize) {
          textSize--;
          seekBar.setProgress(textSize - minTextSize);
        }
      }
    });

    dialog.findViewById(R.id.textSizeUpButton).setOnClickListener(new OnClickListener() {
      @Override
      public void onClick(View v) {
        if (textSize < maxTextSize) {
          textSize++;
          seekBar.setProgress(textSize - minTextSize);
        }
      }
    });
  }

  /**
   * Combine a localized string for "Text Size" plus Arabic numerals
   * @return String
   */
  private String getTextSizeString() {
    // Instead of formatting the number, will truncate formatting and concat the actual textSize
    String label = getString(R.string.text_size).replace("%1$d", "");
    return label + KMString.format(" %d", textSize);
  }

  private void showClearTextDialog() {
    AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(MainActivity.this);
    dialogBuilder.setIcon(R.drawable.ic_light_action_trash);
    dialogBuilder.setTitle(getString(R.string.action_clear_text));
    dialogBuilder.setMessage(getString(R.string.all_text_will_be_cleared));
    dialogBuilder.setPositiveButton(getString(R.string.label_ok), new DialogInterface.OnClickListener() {
      @Override
      public void onClick(DialogInterface dialog, int which) {
        textView.setText("");
      }
    });

    dialogBuilder.setNegativeButton(getString(R.string.label_cancel), new DialogInterface.OnClickListener() {
      @Override
      public void onClick(DialogInterface dialog, int which) {
        // Cancel
      }
    });

    dialogBuilder.show();
  }

  private void checkGetStarted() {
    SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean showGetStarted = prefs.getBoolean(GetStartedActivity.showGetStartedKey, true);
    if (showGetStarted) {
      boolean shouldShowGetStarted = false;
      List<Keyboard> kbList = KMManager.getKeyboardsList(this);
      if (kbList != null && kbList.size() < 2)
        shouldShowGetStarted = true;

      if (!SystemIMESettings.isEnabledAsSystemKB(this))
        shouldShowGetStarted = true;

      if (!SystemIMESettings.isDefaultKB(this))
        shouldShowGetStarted = true;

      if (shouldShowGetStarted)
        showGetStarted();
    }
  }

  private void checkSendCrashReport() {
    SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean maySendCrashReport = prefs.getBoolean(KeymanSettingsActivity.sendCrashReport, true);
    KMManager.setMaySendCrashReport(maySendCrashReport);
  }

  private void checkHapticFeedback() {
    SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean mayHaveHapticFeedback = prefs.getBoolean(KeymanSettingsActivity.hapticFeedbackKey, false);
    KMManager.setHapticFeedback(mayHaveHapticFeedback);
  }

  /**
   * Check that WebView is enabled and Chrome version high enough for Keyman.
   * If they're not, update the view and button to install/enable the missing app.
   * @param context
   */
  private void checkWebViewAndChromeVersion(Context context) {
    final SystemWebViewStatus webViewStatus = WebViewUtils.getSystemWebViewStatus(context);
    final boolean updateChrome = KMManager.getEngineWebViewVersionStatus() != EngineWebViewVersionStatus.FULL;
    if (webViewStatus == SystemWebViewStatus.FULL && !updateChrome) {
      return;
    }

    final TextView textView = (TextView)findViewById((R.id.kmWebViewChromeTextView));
    String buttonLabel = getString(R.string.button_update_chrome);
    if (webViewStatus == SystemWebViewStatus.NOT_INSTALLED) {
      textView.setText(getString(R.string.body_install_webview));
      buttonLabel = getString(R.string.label_install_webview);
    } else if (webViewStatus == SystemWebViewStatus.DISABLED) {
      textView.setText(R.string.body_enable_webview);
      buttonLabel = getString(R.string.label_enable_webview);
    }

    LinearLayout webViewChromeLayout = (LinearLayout) findViewById(R.id.checkWebViewChromeLayout);
    webViewChromeLayout.setVisibility(View.VISIBLE);

    Button button = (Button)findViewById(R.id.webViewChromeButton);
    button.setText(buttonLabel);
    button.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        // Initialize the intent to something (alternate Play Store link to install/update Chrome)
        Intent intent = new Intent(Intent.ACTION_VIEW,
          Uri.parse("https://play.google.com/store/apps/details?id=com.android.chrome"));
        if (webViewStatus == SystemWebViewStatus.NOT_INSTALLED) {
          // Play Store link to install WebView
          intent = new Intent(Intent.ACTION_VIEW,
            Uri.parse("https://play.google.com/store/apps/details?id=com.google.android.webview"));
        } else if (webViewStatus == SystemWebViewStatus.DISABLED) {
          // Application settings to enable WebView
          intent = new Intent("android.settings.APPLICATION_DETAILS_SETTINGS",
            Uri.parse("package:com.google.android.webview"));
        } else if (updateChrome) {
          // Primary Play Store link to install/update Chrome
          intent = new Intent(Intent.ACTION_VIEW,
            Uri.parse("market://details?id=com.android.chrome"));
        }

        // Launch intent
        try {
          PackageManager packageManager = context.getPackageManager();
          if (intent.resolveActivity(packageManager) != null) {
            startActivity(intent);
          } else {
            // (alternate Play Store link to install/update Chrome)
            intent = new Intent(Intent.ACTION_VIEW,
              Uri.parse("https://play.google.com/store/apps/details?id=com.android.chrome"));

            intent.resolveActivity(packageManager);
            startActivity(intent);
          }
        } catch (android.content.ActivityNotFoundException e) {
          Toast.makeText(getApplicationContext(), getString(R.string.unable_to_open_browser), Toast.LENGTH_SHORT).show();
        }
      }
    });
  }

  private Uri requestPermissionIntentUri;

  @Override
  public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
    super.onRequestPermissionsResult(requestCode, permissions, grantResults);
    if (requestCode == PERMISSION_REQUEST_STORAGE) {
      // Request for storage permission
      if (grantResults.length == 1 &&
          grantResults[0] == PERMISSION_GRANTED) {
        // Permission has been granted. Resume task needing this permission
        useLocalKMP(context, requestPermissionIntentUri);
      } else {
        // Permission request denied
        CheckPermissions.showPermissionDenied(context);
      }
    }
  }

  // TODO: Move this to KMEA during Keyman 13.0 refactoring
  public static void useLocalKMP(Context context, Uri data) {
    if (data != null) {
      useLocalKMP(context, data, false);
    }
  }

  public static void useLocalKMP(Context context, Uri data, boolean silentInstall) {
    DownloadFileUtils.Info info = DownloadFileUtils.cacheDownloadFile(context, data);
    boolean isKMP = info.isKMP();
    String filename = info.getFilename();
    File cacheKMPFile = info.getFile();

    if (filename == null || filename.isEmpty() || cacheKMPFile == null || !cacheKMPFile.exists()) {
      // failed to retrieve downloaded file
      String message = context.getString(R.string.failed_to_retrieve_file);
      Toast.makeText(context, message, Toast.LENGTH_LONG).show();
      return;
    } else if (!isKMP) {
      String noKeyboardsInstalledMessage = String.format(
        context.getString(R.string.not_valid_package_file), filename, context.getString(R.string.no_targets_to_install));
      Toast.makeText(context, noKeyboardsInstalledMessage, Toast.LENGTH_LONG).show();
      return;
    }

    if (cacheKMPFile != null) {
      Bundle bundle = new Bundle();
      bundle.putString("kmpFile", cacheKMPFile.getAbsolutePath());
      bundle.putBoolean("silentInstall", silentInstall);

      Intent packageIntent = new Intent(context, PackageActivity.class);
      packageIntent.putExtras(bundle);
      context.startActivity(packageIntent);
    }
  }

  private void showGetStarted() {
    Intent getStartedIntent = new Intent(this, GetStartedActivity.class);
    startActivity(getStartedIntent);
  }

  private void showSettings() {
    Intent settingsIntent = new Intent(this, KeymanSettingsActivity.class);
    startActivity(settingsIntent);
  }

  /**
   * Dismiss the download progress dialog
   */
  public static void cleanupPackageInstall() {
    if (progressDialog != null && progressDialog.isShowing()) {
      progressDialog.dismiss();
    };
    progressDialog = null;
  }

  public static Drawable getActionBarDrawable(Context context) {
    Point size = KMManager.getWindowSize(context);
    int width = size.x;

    TypedValue outValue = new TypedValue();
    context.getTheme().resolveAttribute(android.R.attr.actionBarSize, outValue, true);
    int actionBarHeight = context.getResources().getDimensionPixelSize(outValue.resourceId);
    int bh = context.getResources().getDimensionPixelSize(R.dimen.keyman_bar_height);
    int th = actionBarHeight - bh;

    float w1 = (width * 56 / 100f);
    float w2 = (width * 23 / 100f);

    Bitmap.Config conf = Bitmap.Config.ARGB_8888;
    Bitmap bitmap = Bitmap.createBitmap(width, actionBarHeight, conf);
    Canvas canvas = new Canvas(bitmap);

    Paint p = new Paint();
    p.setStyle(Paint.Style.FILL);
    p.setColor(Color.WHITE);
    canvas.drawRect(new Rect(0, 0, width, actionBarHeight), p);

    p.setColor(context.getResources().getColor(R.color.keyman_orange));
    canvas.drawRect(new RectF(0, th, w1, actionBarHeight), p);

    p.setColor(context.getResources().getColor(R.color.keyman_red));
    canvas.drawRect(new RectF(w1, th, w1 + w2, actionBarHeight), p);

    p.setColor(context.getResources().getColor(R.color.keyman_blue));
    canvas.drawRect(new RectF(w1 + w2, th, width, actionBarHeight), p);
    return new BitmapDrawable(context.getResources(), bitmap);
  }

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Do nothing
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    // Do nothing
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    cleanupPackageInstall();

    String packageID = null;
    String customHelpLink = null;
    KmpInstallMode installMode = KmpInstallMode.Full;
    for (int i = 0; i < keyboardsInstalled.size(); i++) {
      HashMap<String, String> hashMap = new HashMap<>(keyboardsInstalled.get(i));
      String languageID = hashMap.get(KMManager.KMKey_LanguageID);
      Keyboard keyboardInfo = new Keyboard(
        hashMap.get(KMManager.KMKey_PackageID),
        hashMap.get(KMManager.KMKey_KeyboardID),
        hashMap.get(KMManager.KMKey_KeyboardName),
        languageID,
        hashMap.get(KMManager.KMKey_LanguageName),
        hashMap.get(KMManager.KMKey_Version),
        hashMap.get(KMManager.KMKey_CustomHelpLink),
        hashMap.get(KMManager.KMKey_KMPLink),
        true,
        hashMap.get(KMManager.KMKey_Font),
        hashMap.get(KMManager.KMKey_OskFont));

        if (hashMap.containsKey(KMManager.KMKey_KMPInstall_Mode)) {
          installMode = KmpInstallMode.fromString(hashMap.get(KMManager.KMKey_KMPInstall_Mode));
        }
      if (i == 0) {
        packageID = keyboardInfo.getPackageID();
        customHelpLink = keyboardInfo.getHelpLink();
        if (KMManager.addKeyboard(this, keyboardInfo)) {
          KMManager.setKeyboard(keyboardInfo);
        }
      } else {
        KMManager.addKeyboard(this, keyboardInfo);
      }

      // Determine if associated lexical model for languageID should be downloaded
      if ((languageID != null) && !languageID.isEmpty()) {

        // Force the cloud catalog to update
        if (!didExecuteParser) {
          didExecuteParser = true;
          repo = CloudRepository.shared.fetchDataset(context);
        }

        // Check if associated model is not already installed
        if ((KMManager.getAssociatedLexicalModel(languageID) == null) && KMManager.hasConnection(context)) {
          String _downloadid = CloudLexicalModelMetaDataDownloadCallback.createDownloadId(languageID);
          CloudLexicalModelMetaDataDownloadCallback _callback = new CloudLexicalModelMetaDataDownloadCallback();

          ArrayList<CloudApiTypes.CloudApiParam> aPreparedCloudApiParams = new ArrayList<>();
          String url = CloudRepository.prepareLexicalModelQuery(languageID);
          aPreparedCloudApiParams.add(new CloudApiTypes.CloudApiParam(
            CloudApiTypes.ApiTarget.KeyboardLexicalModels, url).setType(CloudApiTypes.JSONType.Array));

          CloudDownloadMgr.getInstance().executeAsDownload(
            context, _downloadid, null, _callback,
            aPreparedCloudApiParams.toArray(new CloudApiTypes.CloudApiParam[0]));
        }
      }
    }

    // Display welcome.htm help file -- if we are not doing a silent install
    if (installMode != KmpInstallMode.Silent &&
        customHelpLink != null && !customHelpLink.isEmpty() &&
        packageID != null && !packageID.isEmpty() && FileUtils.isWelcomeFile(customHelpLink)) {
      // Display local welcome.htm help file, including associated assets
      Intent i = new Intent(this, KMHelpFileActivity.class);
      // Have to use packageID since we don't store the package name
      i.putExtra(KMManager.KMKey_PackageID, packageID);
      i.putExtra(KMManager.KMKey_CustomHelpLink, customHelpLink);
      startActivity(i);
    }
  }

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
    cleanupPackageInstall();

    String langId = (KMManager.getCurrentKeyboardInfo(this) != null) ?
      KMManager.getCurrentKeyboardInfo(this).getLanguageID() :
      KMManager.KMDefault_LanguageID;
    boolean matchingModel = false;

    for(int i=0; i<lexicalModelsInstalled.size(); i++) {
      HashMap<String, String>lexicalModelInfo = new HashMap<>(lexicalModelsInstalled.get(i));
      if(lexicalModelInfo.get(KMManager.KMKey_LanguageID).equals(langId)) {
        matchingModel = true;
      }
      KMManager.addLexicalModel(this, lexicalModelInfo);
    }

    // We're on the main thread, so if the active keyboard's language code matches,
    // let's register the associated lexical model.
    if(matchingModel) {
      KMManager.registerAssociatedLexicalModel(langId);
    }
  }

  private void copyFile(FileInputStream inStream, File dstFile) throws IOException {
    OutputStream outStream = new FileOutputStream(dstFile);

    byte[] buffer = new byte[1024];
    int len;
    while ((len = inStream.read(buffer)) > 0) {
      outStream.write(buffer, 0, len);
    }
    outStream.flush();
    outStream.close();
  }
}
