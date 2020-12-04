/**
 * Copyright (C) 2018-2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KMManager.KeyboardType;
import com.tavultesoft.kmea.KMTextView;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;
import com.tavultesoft.kmea.cloud.CloudApiTypes;
import com.tavultesoft.kmea.cloud.CloudDownloadMgr;
import com.tavultesoft.kmea.cloud.impl.CloudLexicalModelMetaDataDownloadCallback;
import com.tavultesoft.kmea.data.CloudRepository;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.LexicalModel;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.DownloadIntentService;
import com.tavultesoft.kmea.util.KMLog;
import com.tavultesoft.kmea.util.KMPLink;

import android.Manifest;
import android.app.ProgressDialog;
import android.content.ComponentName;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.net.Uri;
import android.net.Uri.Builder;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.ParcelFileDescriptor;
import android.os.Parcelable;
import android.annotation.SuppressLint;
import android.annotation.TargetApi;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.view.menu.MenuBuilder;
import androidx.appcompat.view.menu.MenuPopupHelper;
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
import android.provider.OpenableColumns;
import androidx.annotation.NonNull;
import androidx.core.app.ActivityCompat;

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
import android.widget.SeekBar;
import android.widget.TextView;
import android.widget.Toast;

import io.sentry.android.core.SentryAndroid;

public class MainActivity extends AppCompatActivity implements OnKeyboardEventListener, OnKeyboardDownloadEventListener,
    ActivityCompat.OnRequestPermissionsResultCallback {
  public static Context context;

  // Fields used for installing kmp packages
  private static final int PERMISSION_REQUEST_STORAGE = 0;
  public static final int READ_REQUEST_CODE = 42;

  private static final String TAG = "MainActivity";

  private KMTextView textView;
  private final int minTextSize = 16;
  private final int maxTextSize = 72;
  private int textSize = minTextSize;
  private static final String defaultKeyboardInstalled = "DefaultKeyboardInstalled";
  private static final String defaultDictionaryInstalled = "DefaultDictionaryInstalled";
  private static final String userTextKey = "UserText";
  private static final String userTextSizeKey = "UserTextSize";
  private Toolbar toolbar;
  private Menu menu;
  private Uri data;

  private static Dataset repo;
  private boolean didExecuteParser = false;

  DownloadResultReceiver resultReceiver;
  private ProgressDialog progressDialog;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    setTheme(R.style.AppTheme);
    super.onCreate(savedInstanceState);
    context = this;

    checkSendCrashReport();
    if (KMManager.getMaySendCrashReport()) {
      SentryAndroid.init(context, options -> {
        options.setRelease("release-" + com.tavultesoft.kmapro.BuildConfig.VERSION_NAME);
        options.setEnvironment(com.tavultesoft.kmapro.BuildConfig.VERSION_ENVIRONMENT);
      });
    }

    checkStoragePermission(null);
    resultReceiver = new DownloadResultReceiver(new Handler(), context);

    if (BuildConfig.DEBUG) {
      KMManager.setDebugMode(true);
    }

    KMManager.initialize(getApplicationContext(), KeyboardType.KEYBOARD_TYPE_INAPP);
    KMManager.executeResourceUpdate(this);

    SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    // Add default keyboard
    boolean installDefaultKeyboard = prefs.getBoolean(defaultKeyboardInstalled, false);
    if (!installDefaultKeyboard) {
      if (!KMManager.keyboardExists(context, KMManager.KMDefault_PackageID, KMManager.KMDefault_KeyboardID,
          KMManager.KMDefault_LanguageID)) {
        KMManager.addKeyboard(this, KMManager.getDefaultKeyboard(getApplicationContext()));
      }
      SharedPreferences.Editor editor = prefs.edit();
      editor.putBoolean(defaultKeyboardInstalled, true);
      editor.commit();
    }

    // Add default dictionary
    boolean installDefaultDictionary = prefs.getBoolean(defaultDictionaryInstalled, false);
    if (!installDefaultDictionary) {
      LexicalModel defaultLexicalModel = LexicalModel.getDefaultLexicalModel(context);
      HashMap<String, String> lexicalModelInfo = new HashMap<String, String>();
      lexicalModelInfo.put(KMManager.KMKey_PackageID, defaultLexicalModel.getPackageID());
      lexicalModelInfo.put(KMManager.KMKey_LanguageID, defaultLexicalModel.getLanguageID());
      lexicalModelInfo.put(KMManager.KMKey_LexicalModelID, defaultLexicalModel.getLexicalModelID());
      lexicalModelInfo.put(KMManager.KMKey_LexicalModelName, defaultLexicalModel.getLexicalModelName());
      lexicalModelInfo.put(KMManager.KMKey_LexicalModelVersion, defaultLexicalModel.getVersion());
      /*
      // If welcome.htm exists, add custom help link
      welcomeFile = new File(KMManager.getLexicalModelsDir(), KMManager.KMDefault_DictionaryPackageID + File.separator + FileUtils.WELCOME_HTM);
      lexicalModelInfo.put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
       */
      KMManager.addLexicalModel(context, lexicalModelInfo);
      KMManager.registerAssociatedLexicalModel(KMManager.KMDefault_LanguageID);

      SharedPreferences.Editor editor = prefs.edit();
      editor.putBoolean(defaultDictionaryInstalled, true);
      editor.commit();
    }

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

    KMManager.addKeyboardEventListener(this);
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);
    PackageActivity.addKeyboardDownloadEventListener(this);

    // Get calling activity
    ComponentName component = this.getCallingActivity();
    String caller = null;
    if (component != null) {
      caller = component.getClassName();
    }

    Intent intent = getIntent();
    data = intent.getData();

    if (data != null) {
      String scheme = data.getScheme().toLowerCase();
      switch (scheme) {
        // content:// Android DownloadManager
        // file:// Chrome downloads and Filebrowsers
        case "content":
        case "file":
          checkStoragePermission(data);
          break;
        case "http" :
        case "https" :
          // Might need to modify link like KMPBrowserActivity
          String link = data.toString();
          if (KMPLink.isKeymanInstallLink(link)) {
            data = KMPLink.getKeyboardDownloadLink(link);
          }
          downloadKMP(scheme);
          break;
        case "keyman" :
          // Only accept download links from Keyman browser activities
          if (KMPLink.isKeymanDownloadLink(data.toString()) && caller != null &&
            (caller.equalsIgnoreCase("com.tavultesoft.kmea.KMPBrowserActivity") ||
             caller.equalsIgnoreCase("com.tavultesoft.kmapro.WebBrowserActivity"))) {

            // Convert opaque URI to hierarchical URI so the query parameters can be parsed
            Builder builder = new Uri.Builder();
            builder.scheme("https")
              .authority("keyman.com")
              .appendPath("keyboards")
              .encodedQuery(data.getEncodedQuery());
            data = Uri.parse(builder.build().toString());
            downloadKMP(scheme);
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
    getSupportActionBar().setBackgroundDrawable(getActionBarDrawable(this));
    resizeTextView(textView.isKeyboardVisible());
    invalidateOptionsMenu();
  }

  @SuppressLint("RestrictedApi")
  @Override
  public boolean onPrepareOptionsMenu(final Menu menu) {
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
    if (_keyboardupdate != null) {
      updateUpdateCountIndicator(_keyboardupdate, anUpdateCount, true);
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
      case R.id.action_web:
        showWebBrowser();
        return true;
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
    // Do nothing
  }

  @Override
  public void onKeyboardChanged(String newKeyboard) {
    textView.setTypeface(KMManager.getKeyboardTextFontTypeface(this));
  }

  @Override
  public void onKeyboardShown() {
    resizeTextView(true);
  }

  @Override
  public void onKeyboardDismissed() {
    resizeTextView(false);
  }

  /**
   * Parse the URI data to determine the filename and URL for the .kmp keyboard package.
   * If URL is valid, download the kmp.
   * @param scheme String of the URI's scheme.
   *
   */
  private void downloadKMP(String scheme) {
    if (data == null) {
      String message = "Download failed. Invalid URL.";
      Toast.makeText(getApplicationContext(), message,
        Toast.LENGTH_SHORT).show();
      return;
    }
    try {
      // Initial try with Keyman 13.0 download link
      String url = data.getQueryParameter(KMKeyboardDownloaderActivity.KMKey_URL);
      if (url == null) {
        url = data.toString();
      }
      if (url != null) {
        // Create filename by extracting packageID from query or urlNoQuery
        String filename = data.getQueryParameter("id");
        if (filename == null) {
          String urlNoQuery = data.getPath();
          filename = FileUtils.getFilename(urlNoQuery);
        }
        if (!filename.endsWith(FileUtils.KEYMANPACKAGE)) {
          filename += FileUtils.KEYMANPACKAGE;
        }

        // Parse query for the BCP 47 language ID
        String languageID = data.getQueryParameter(KMKeyboardDownloaderActivity.KMKey_BCP47);
        // TODO: Using "tag" for now, but production will be KMKeyboardDownloaderActivity.KMKey_BCP47
        if (languageID == null) {
          languageID = data.getQueryParameter("tag");
        }

        url = url.toLowerCase();
        try {
          progressDialog = new ProgressDialog(MainActivity.this);
          progressDialog.setMessage(String.format(getString(R.string.downloading_keyboard_package), filename));
          progressDialog.setCancelable(false);
          progressDialog.show();
          resultReceiver.setProgressDialog(progressDialog);

          // Download the KMP to app cache
          Intent downloadIntent = new Intent(MainActivity.this, DownloadIntentService.class);
          downloadIntent.putExtra("url", url);
          downloadIntent.putExtra("filename", filename);
          downloadIntent.putExtra("language", languageID);
          downloadIntent.putExtra("destination", MainActivity.this.getCacheDir().toString());
          downloadIntent.putExtra("receiver", resultReceiver);

          startService(downloadIntent);
        } catch (Exception e) {
          KMLog.LogException(TAG, "", e);
          if (progressDialog != null && progressDialog.isShowing()) {
            progressDialog.dismiss();
          }
          progressDialog = null;
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
    }
  }

  private void resizeTextView(boolean isKeyboardVisible) {
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

    Point size = new Point(0, 0);
    getWindowManager().getDefaultDisplay().getSize(size);
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
    Intent i = new Intent(this, WebBrowserActivity.class);
    i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
    startActivity(i);
    overridePendingTransition(android.R.anim.fade_in, R.anim.hold);
  }

  private void showShareDialog() {
    List<Intent> shareIntents = new ArrayList<Intent>();
    Intent intent = new Intent(Intent.ACTION_SEND);
    intent.setType("text/plain");
    intent.putExtra(Intent.EXTRA_TEXT, textView.getText().toString());

    List<ResolveInfo> resInfo = getPackageManager().queryIntentActivities(intent, 0);
    final String[] IGNORED_PACKAGE_NAMES = {
      "com.facebook.katana",
      "com.facebook.lite"
    };

    for (ResolveInfo resolveInfo : resInfo) {
      String packageName = resolveInfo.activityInfo.packageName;

      Intent shareIntent = new Intent(android.content.Intent.ACTION_SEND);
      shareIntent.setType("text/plain");
      shareIntent.setPackage(packageName);

      String text = textView.getText().toString();
      String htmlMailFormat = "<html><head></head><body>%s%s</body></html>";
      String extraMailText = "<br><br>Sent from&nbsp<a href=\"http://keyman.com/android\">Keyman for Android</a>";

      // Sharing to Facebook removed in https://github.com/keymanapp/keyman/issues/156
      // Users can copy text or use Keyman as system keyboard for typing into Facebook
      if (!Arrays.asList(IGNORED_PACKAGE_NAMES).contains(packageName)) {
        if (packageName.equals("com.google.android.gm")) {
          // Html string for Gmail
          shareIntent.setType("message/rfc822");
          text = text.replace("<", "&lt;");
          text = text.replace(">", "&gt;");
          text = text.replace(" ", "&nbsp;");
          text = text.replace('\n', ' ');
          text = text.replace(" ", "<br>");
          shareIntent.putExtra(Intent.EXTRA_TEXT, Html.fromHtml(String.format(htmlMailFormat, text, extraMailText)));
        } else {
          // Text for all others
          shareIntent.putExtra(Intent.EXTRA_TEXT, text);
        }

        shareIntents.add(shareIntent);
      }
    }

    Intent chooserIntent = Intent.createChooser(shareIntents.remove(0), "Share via");
    chooserIntent.putExtra(Intent.EXTRA_INITIAL_INTENTS, shareIntents.toArray(new Parcelable[]{}));
    startActivity(chooserIntent);
  }

  @SuppressLint("InflateParams")
  private void showTextSizeDialog() {
    LayoutInflater inflater = LayoutInflater.from(MainActivity.this);
    final View textSizeController = inflater.inflate(R.layout.text_size_controller, null);
    final AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(MainActivity.this);
    dialogBuilder.setIcon(R.drawable.ic_light_action_textsize);
    dialogBuilder.setTitle(String.format(getString(R.string.text_size), textSize));
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
        dialog.setTitle(String.format(getString(R.string.text_size), textSize));
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

  @Override
  public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
    super.onRequestPermissionsResult(requestCode, permissions, grantResults);
    if (requestCode == PERMISSION_REQUEST_STORAGE) {
      // Request for storage permission
      if (grantResults.length ==2 &&
          grantResults[0] == PackageManager.PERMISSION_GRANTED &&
          grantResults[1] == PackageManager.PERMISSION_GRANTED) {
        // Permission has been granted. Resume task needing this permission
        useLocalKMP(context, data);
      } else {
        // Permission request denied
        String message = "Storage permission request was denied. Unable to install keyboard package";
        Toast.makeText(getApplicationContext(), message,
          Toast.LENGTH_SHORT).show();
      }
    }
  }

  private void checkStoragePermission(Uri data) {
    // Check if the Storage permission has been granted
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
      if ((checkSelfPermission(Manifest.permission.WRITE_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED) &&
          (checkSelfPermission(Manifest.permission.READ_EXTERNAL_STORAGE) == PackageManager.PERMISSION_GRANTED)) {
        useLocalKMP(context, data);
      } else {
        // Permission is missing and must be requested
        requestStoragePermission();
      }
    } else {
      // Permission automatically granted on older Android versions
      useLocalKMP(context, data);
    }
  }

  /**
   * Requests the {@link android.Manifest.permission#READ_EXTERNAL_STORAGE} and
   *              {@link android.Manifest.permission#WRITE_EXTERNAL_STORAGE} permissions
   */
  private void requestStoragePermission() {
    if (ActivityCompat.shouldShowRequestPermissionRationale(this, Manifest.permission.READ_EXTERNAL_STORAGE) &&
        ActivityCompat.shouldShowRequestPermissionRationale(this, Manifest.permission.WRITE_EXTERNAL_STORAGE)) {
      // Provide additional rationale to the user if the permission was not granted
      String message = "To install keyboard packages, allow Keyman permission to read/write storage.";
      Toast.makeText(getApplicationContext(), message ,
        Toast.LENGTH_LONG).show();
      ActivityCompat.requestPermissions(this,
        new String[]{
          Manifest.permission.READ_EXTERNAL_STORAGE,
          Manifest.permission.WRITE_EXTERNAL_STORAGE},
        PERMISSION_REQUEST_STORAGE);
    } else {
      // Request the permission. The result will be received in onRequestPermissionsResult().
      ActivityCompat.requestPermissions(this,
        new String[]{
          Manifest.permission.READ_EXTERNAL_STORAGE,
          Manifest.permission.WRITE_EXTERNAL_STORAGE},
        PERMISSION_REQUEST_STORAGE);
    }
  }

  // TODO: Move this to KMEA during Keyman 13.0 refactoring
  public static void useLocalKMP(Context context, Uri data) {
    if (data != null) {
      useLocalKMP(context, data, false);
    }
  }

  public static void useLocalKMP(Context context, Uri data, boolean silentInstall) {
    String filename = "";
    String cacheKMPFilename = "";
    File cacheKMPFile = null;
    InputStream inputFile = null;
    Bundle bundle = new Bundle();
    try {
      boolean isKMP = false;
      switch (data.getScheme().toLowerCase()) {
        case "content":
          // DownloadManager passes a path "/document/number" so we need to extract the .kmp filename
          Cursor cursor = context.getContentResolver().query(data, null, null, null, null);
          cursor.moveToFirst();
          int nameIndex = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME);
          filename = cursor.getString(nameIndex);
          isKMP = FileUtils.hasKeymanPackageExtension(filename);
          cacheKMPFilename = filename;
          inputFile = context.getContentResolver().openInputStream(data);
          break;

        case "file":
          File kmpFile = new File(data.getPath());
          filename = kmpFile.getName();
          isKMP = FileUtils.hasKeymanPackageExtension(data.toString());
          cacheKMPFilename = kmpFile.getName();
          inputFile = new FileInputStream(kmpFile);
          break;
      }

      if (isKMP) {
        // Copy KMP to app cache
        cacheKMPFile = new File(context.getCacheDir().toString(), cacheKMPFilename);
        if (cacheKMPFile.exists()) {
          cacheKMPFile.delete();
        }

        FileUtils.copy(inputFile, new FileOutputStream(cacheKMPFile));
      } else {
        String noKeyboardsInstalledMessage = " is not a valid Keyman package file.\n" +
          "No keyboards/dictionaries were installed.";
        Toast.makeText(context,
          filename + noKeyboardsInstalledMessage, Toast.LENGTH_LONG).show();
      }
    } catch (Exception e) {
      String message = "Access denied to " + filename +
        ".\nCheck Android Settings --> Apps --> Keyman to grant storage permissions";
      KMLog.LogException(TAG, "Unable to copy " + filename + " to app cache ", e);
      Toast.makeText(context, message, Toast.LENGTH_LONG).show();
      return;
    }

    if (cacheKMPFile != null) {
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

  public static Drawable getActionBarDrawable(Context context) {
    Point size = new Point();
    WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
    wm.getDefaultDisplay().getSize(size);
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
    for(int i=0; i < keyboardsInstalled.size(); i++) {
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

      if (i == 0) {
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

          Toast.makeText(context,
            context.getString(R.string.query_associated_model),
            Toast.LENGTH_SHORT).show();

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
  }

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
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
