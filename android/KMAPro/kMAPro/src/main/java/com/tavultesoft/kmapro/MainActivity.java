/**
 * Copyright (C) 2018 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

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

import com.crashlytics.android.Crashlytics;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KMManager.KeyboardType;
import com.tavultesoft.kmea.KMTextView;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.DownloadIntentService;

import android.Manifest;
import android.app.ProgressDialog;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.ParcelFileDescriptor;
import android.os.Parcelable;
import android.annotation.SuppressLint;
import android.annotation.TargetApi;
import android.support.v7.app.AppCompatActivity;
import android.support.v7.widget.Toolbar;
import android.support.v7.app.AlertDialog;
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
import android.os.ResultReceiver;
import android.provider.OpenableColumns;
import android.support.annotation.NonNull;
import android.support.v4.app.ActivityCompat;
import android.text.Html;
import android.util.Log;
import android.util.TypedValue;
import android.view.ContextThemeWrapper;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.WindowManager;
import android.widget.SeekBar;
import android.widget.Toast;

public class MainActivity extends AppCompatActivity implements OnKeyboardEventListener, OnKeyboardDownloadEventListener,
  ActivityCompat.OnRequestPermissionsResultCallback {

  // Fields used for installing kmp packages
  private static final int PERMISSION_REQUEST_STORAGE = 0;
  Uri data;

  private static final String TAG = "MainActivity";
  private FirebaseAnalytics mFirebaseAnalytics;

  private KMTextView textView;
  private final int minTextSize = 16;
  private final int maxTextSize = 72;
  private int textSize = minTextSize;
  private static final String userTextKey = "UserText";
  private static final String userTextSizeKey = "UserTextSize";
  protected static final String dontShowGetStartedKey = "DontShowGetStarted";
  protected static final String didCheckUserDataKey = "DidCheckUserData";
  private Toolbar toolbar;
  private Menu menu;

  DownloadResultReceiver resultReceiver;
  private ProgressDialog progressDialog;

  private class DownloadResultReceiver extends ResultReceiver {
    public DownloadResultReceiver(Handler handler) {
      super(handler);
    }

    @Override
    protected void onReceiveResult(int resultCode, Bundle resultData) {
      if (progressDialog != null && progressDialog.isShowing()) {
        progressDialog.dismiss();
      };
      progressDialog = null;
      switch(resultCode) {
        case FileUtils.DOWNLOAD_ERROR :
          Toast.makeText(getApplicationContext(), "Download failed",
            Toast.LENGTH_SHORT).show();
          break;
        case FileUtils.DOWNLOAD_SUCCESS :
          String downloadedFilename = resultData.getString("filename");
          String kmpFilename = resultData.getString("destination") + File.separator + downloadedFilename;

          Bundle bundle = new Bundle();
          bundle.putString("kmpFile", kmpFilename);
          Intent packageIntent = new Intent(getApplicationContext(), PackageActivity.class);
          packageIntent.putExtras(bundle);
          startActivity(packageIntent);
          break;
      }
      super.onReceiveResult(resultCode, resultData);
    }
  }

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    setTheme(R.style.AppTheme);
    super.onCreate(savedInstanceState);
    resultReceiver = new DownloadResultReceiver(new Handler());

    mFirebaseAnalytics = FirebaseAnalytics.getInstance(this);

    if (BuildConfig.DEBUG) {
      KMManager.setDebugMode(true);
    }

    KMManager.initialize(getApplicationContext(), KeyboardType.KEYBOARD_TYPE_INAPP);
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
    SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    textView.setText(prefs.getString(userTextKey, ""));
    textSize = prefs.getInt(userTextSizeKey, minTextSize);
    textView.setTextSize((float) textSize);
    textView.setSelection(textView.getText().length());

    boolean didCheckUserData = prefs.getBoolean(MainActivity.didCheckUserDataKey, false);
    if (!didCheckUserData && (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN)) {
      try {
        Intent getUserdataIntent = new Intent("keyman.ACTION_GET_USERDATA");
        startActivityForResult(getUserdataIntent, 0);
      } catch (Exception e) {
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(MainActivity.didCheckUserDataKey, true);
        editor.commit();
        checkGetStarted();
      }
    } else {
      checkGetStarted();
    }
  }

  @TargetApi(Build.VERSION_CODES.JELLY_BEAN)
  @Override
  public void onActivityResult(int requestCode, int resultCode, Intent returnIntent) {
    if (resultCode != RESULT_OK) {
      checkGetStarted();
      return;
    } else {
      boolean didFail = false;
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
          didFail = true;
          Log.e(TAG, e.getMessage());
        }
      }

      if (!didFail) {
        KMManager.updateOldKeyboardsList(this);
        SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(MainActivity.didCheckUserDataKey, true);
        editor.commit();
      }

      checkGetStarted();
    }
  }

  @Override
  protected void onResume() {
    super.onResume();
    KMManager.onResume();
    KMManager.hideSystemKeyboard();

    if (!KMManager.keyboardExists(this, KMManager.KMDefault_UndefinedPackageID,
      KMManager.KMDefault_KeyboardID, KMManager.KMDefault_LanguageID)) {
      HashMap<String, String> kbInfo = new HashMap<String, String>();
      kbInfo.put(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
      kbInfo.put(KMManager.KMKey_KeyboardID, KMManager.KMDefault_KeyboardID);
      kbInfo.put(KMManager.KMKey_LanguageID, KMManager.KMDefault_LanguageID);
      kbInfo.put(KMManager.KMKey_KeyboardName, KMManager.KMDefault_KeyboardName);
      kbInfo.put(KMManager.KMKey_LanguageName, KMManager.KMDefault_LanguageName);
      kbInfo.put(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(
        this, KMManager.KMDefault_UndefinedPackageID, KMManager.KMDefault_KeyboardID));
      kbInfo.put(KMManager.KMKey_Font, KMManager.KMDefault_KeyboardFont);
      KMManager.addKeyboard(this, kbInfo);
    }

    KMManager.addKeyboardEventListener(this);
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);
    PackageActivity.addKeyboardDownloadEventListener(this);

    Intent intent = getIntent();
    data = intent.getData();

    if (data != null) {

      switch (data.getScheme().toLowerCase()) {
        // Android DownloadManager
        case "content":
          checkStoragePermission(data);
          break;
        // Chrome downloads and Filebrowsers
        case "file":
          checkStoragePermission(data);
          break;
        case "http" :
        case "https" :
          Intent downloadIntent;
          String url = data.getQueryParameter(KMKeyboardDownloaderActivity.KMKey_URL);
          if (url == null) {
            url = data.toString();
          }
          if (url != null) {
            // URL contains KMP to download in background.
            boolean isCustom = KMKeyboardDownloaderActivity.isCustom(url);

            int index = url.lastIndexOf("/") + 1;
            String filename = "unknown:";
            if (index >= 0 && index <= url.length()) {
              filename = url.substring(index);
            }

            // Only handle ad-hoc kmp packages
            if (FileUtils.hasKeyboardPackageExtension(url)) {
              try {
                // Download the KMP to app cache
                downloadIntent = new Intent(MainActivity.this, DownloadIntentService.class);
                downloadIntent.putExtra("url", url);
                downloadIntent.putExtra("destination", MainActivity.this.getCacheDir().toString());
                downloadIntent.putExtra("receiver", resultReceiver);

                progressDialog = new ProgressDialog(MainActivity.this);
                String ellipsisStr = "\u2026";
                progressDialog.setMessage(String.format("%s\n%s%s",
                  getString(R.string.downloading_keyboard_package), filename, ellipsisStr));
                progressDialog.setCancelable(false);
                progressDialog.show();

                startService(downloadIntent);
              } catch (Exception e) {
                if (progressDialog != null && progressDialog.isShowing()) {
                  progressDialog.dismiss();
                }
                progressDialog = null;
                break;
              }
            } else {
              String message = "Download failed. Not a .kmp keyboard package.";
              Toast.makeText(getApplicationContext(), message,
                Toast.LENGTH_SHORT).show();
            }
          }
          break;
        default :
          Log.e(TAG, "Unrecognized protocol " + data.getScheme());
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

  @Override
  public boolean onCreateOptionsMenu(Menu menu) {
    // Inflate the menu; this adds items to the action bar if it is present.
    getMenuInflater().inflate(R.menu.main, menu);
    this.menu = menu;
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

  private void resizeTextView(boolean isKeyboardVisible) {
    int keyboardHeight = 0;
    if (isKeyboardVisible)
      keyboardHeight = KMManager.getKeyboardHeight(this);

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
    textView.setHeight(screenHeight - statusBarHeight - actionBarHeight - keyboardHeight);
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
    dialogBuilder.setTitle(String.format("%s: %d", getString(R.string.action_text_size), textSize));
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
        dialog.setTitle(String.format("%s: %d", getString(R.string.action_text_size), textSize));
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
    dialogBuilder.setMessage(String.format("\n%s\n", getString(R.string.all_text_will_be_cleared)));
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
    boolean dontShowGetStarted = prefs.getBoolean(MainActivity.dontShowGetStartedKey, false);
    if (!dontShowGetStarted) {
      boolean shouldShowGetStarted = false;
      ArrayList<HashMap<String, String>> kbList = KMManager.getKeyboardsList(this);
      if (kbList != null && kbList.size() < 2)
        shouldShowGetStarted = true;

      if (!GetStartedActivity.isEnabledAsSystemKB(this))
        shouldShowGetStarted = true;

      if (!GetStartedActivity.isDefaultKB(this))
        shouldShowGetStarted = true;

      if (shouldShowGetStarted)
        showGetStarted();
    }
  }

  @Override
  public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
    if (requestCode == PERMISSION_REQUEST_STORAGE) {
      // Request for storage permission
      if (grantResults.length == 1 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
        // Permission has been granted. Resume task needing this permission
        useLocalKMP(data);
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
      if (checkSelfPermission(Manifest.permission.READ_EXTERNAL_STORAGE) ==
        PackageManager.PERMISSION_GRANTED) {
        useLocalKMP(data);
      } else {
        // Permission is missing and must be requested
        requestStoragePermission();
      }
    } else {
      // Permission automatically granted on older Android versions
      useLocalKMP(data);
    }
  }

  /**
   * Requests the {@link android.Manifest.permission#READ_EXTERNAL_STORAGE} permission
   */
  private void requestStoragePermission() {
    if (ActivityCompat.shouldShowRequestPermissionRationale(this,
        Manifest.permission.READ_EXTERNAL_STORAGE)) {
      // Provide additional rationale to the user if the permission was not granted
      String message = "To install keyboard package, allow Keyman permission to read storage.";
      Toast.makeText(getApplicationContext(), message ,
        Toast.LENGTH_LONG).show();
      ActivityCompat.requestPermissions(MainActivity.this, new String[]{Manifest.permission.READ_EXTERNAL_STORAGE},
        PERMISSION_REQUEST_STORAGE);
    } else {
      // Request the permission. The result will be received in onRequestPermissionResult().
      ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.READ_EXTERNAL_STORAGE},
        PERMISSION_REQUEST_STORAGE);
    }
  }

  private void useLocalKMP(Uri data) {
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
          Cursor cursor = getContentResolver().query(data, null, null, null, null);
          cursor.moveToFirst();
          int nameIndex = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME);
          filename = cursor.getString(nameIndex);
          isKMP = FileUtils.hasKeyboardPackageExtension(filename);
          cacheKMPFilename = filename;
          inputFile = getContentResolver().openInputStream(data);
          break;

        case "file":
          File kmpFile = new File(data.getPath());
          filename = kmpFile.getName();
          isKMP = FileUtils.hasKeyboardPackageExtension(data.toString());
          cacheKMPFilename = kmpFile.getName();
          inputFile = new FileInputStream(kmpFile);
          break;
      }

      if (isKMP) {
        // Copy KMP to app cache
        cacheKMPFile = new File(MainActivity.this.getCacheDir().toString(), cacheKMPFilename);
        if (cacheKMPFile.exists()) {
          cacheKMPFile.delete();
        }

        FileUtils.copy(inputFile, new FileOutputStream(cacheKMPFile));
      } else {
        String noKeyboardsInstalledMessage = " is not a valid keyboard package file.\nNo keyboards were installed.";
        Toast.makeText(getApplicationContext(),
          filename + noKeyboardsInstalledMessage, Toast.LENGTH_LONG).show();
      }
    } catch (Exception e) {
      String message = "Access denied to " + filename +
        ".\nCheck Android Settings --> Apps --> Keyman to grant storage permissions";
      Toast.makeText(getApplicationContext(), message, Toast.LENGTH_LONG).show();
      Log.e(TAG, "Unable to copy " + filename + " to app cache");
      return;
    }

    if (cacheKMPFile != null) {
      bundle.putString("kmpFile", cacheKMPFile.getAbsolutePath());

      Intent packageIntent = new Intent(getApplicationContext(), PackageActivity.class);
      packageIntent.putExtras(bundle);
      startActivity(packageIntent);
    }
  }

  private void showGetStarted() {
    Intent getStartedIntent = new Intent(this, GetStartedActivity.class);
    startActivity(getStartedIntent);
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
    String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
    if (result > 0) {
      String packageID = keyboardInfo.get(KMManager.KMKey_PackageID);
      String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String keyboardName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
      String languageName = keyboardInfo.get(KMManager.KMKey_LanguageName);
      String kbVersion = keyboardInfo.get(KMManager.KMKey_KeyboardVersion);
      String kFont = keyboardInfo.get(KMManager.KMKey_Font);
      String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
      if (languageID.contains(";")) {
        String[] ids = languageID.split("\\;");
        String[] names = languageName.split("\\;");
        int len = ids.length;
        for (int i = 0; i < len; i++) {
          String langId = ids[i];
          String langName = "Unknown";
          if (i < names.length)
            langName = names[i];
          HashMap<String, String> kbInfo = new HashMap<String, String>();
          kbInfo.put(KMManager.KMKey_PackageID, packageID);
          kbInfo.put(KMManager.KMKey_KeyboardID, keyboardID);
          kbInfo.put(KMManager.KMKey_LanguageID, langId);
          kbInfo.put(KMManager.KMKey_KeyboardName, keyboardName);
          kbInfo.put(KMManager.KMKey_LanguageName, langName);
          kbInfo.put(KMManager.KMKey_KeyboardVersion, kbVersion);
          kbInfo.put(KMManager.KMKey_Font, kFont);
          kbInfo.put(KMManager.KMKey_OskFont, kOskFont);
          if (i == 0) {
            if (KMManager.addKeyboard(this, kbInfo)) {
              KMManager.setKeyboard(packageID, keyboardID, langId, keyboardName, langName, kFont, kOskFont);
            }
          } else {
            KMManager.addKeyboard(this, kbInfo);
          }
        }
      } else {
        if (KMManager.addKeyboard(this, keyboardInfo)) {
          KMManager.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);
        }
      }
    } else {
      Toast.makeText(this, "Keyboard " + keyboardID + " download failed", Toast.LENGTH_SHORT).show();
    }
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    for(int i=0; i < keyboardsInstalled.size(); i++) {
      HashMap<String, String> keyboardInfo = new HashMap<>(keyboardsInstalled.get(i));
      if (i == 0) {
        if (KMManager.addKeyboard(this, keyboardInfo)) {
          String packageID = keyboardInfo.get(KMManager.KMKey_PackageID);
          String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
          String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
          String keyboardName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
          String languageName = keyboardInfo.get(KMManager.KMKey_LanguageName);
          String kFont = keyboardInfo.get(KMManager.KMKey_Font);
          String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
          KMManager.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);
        }
      } else {
        KMManager.addKeyboard(this, keyboardInfo);
      }
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