/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmapro;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.tavultesoft.kmea.KMManager;

import com.tavultesoft.kmea.KMManager.KeyboardType;
import com.tavultesoft.kmea.KMTextView;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;

import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.ParcelFileDescriptor;
import android.os.Parcelable;
import android.annotation.SuppressLint;
import android.annotation.TargetApi;
import android.app.Activity;
import android.app.ActionBar;
import android.app.AlertDialog;
import android.content.ClipData;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.LabeledIntent;
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
import android.text.Html;
import android.util.Log;
import android.util.TypedValue;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.WindowManager;
import android.widget.SeekBar;
import android.widget.Toast;

public class MainActivity extends Activity implements OnKeyboardEventListener, OnKeyboardDownloadEventListener {

  private KMTextView textView;
  private final int minTextSize = 16;
  private final int maxTextSize = 72;
  private int textSize = minTextSize;
  private static final String userTextKey = "UserText";
  private static final String userTextSizeKey = "UserTextSize";
  protected static final String dontShowGetStartedKey = "DontShowGetStarted";
  protected static final String didCheckUserDataKey = "DidCheckUserData";
  private Menu menu;

  @Override
  protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    final ActionBar actionBar = getActionBar();
    actionBar.setLogo(R.drawable.keyman_logo);
    actionBar.setDisplayShowTitleEnabled(false);
    actionBar.setBackgroundDrawable(getActionBarDrawable(this));

    if (BuildConfig.DEBUG) {
      KMManager.setDebugMode(true);
    }
    KMManager.initialize(getApplicationContext(), KeyboardType.KEYBOARD_TYPE_INAPP);
    setContentView(R.layout.activity_main);
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
          } else if (filename.endsWith(".js")) {
            File langDir = new File(getDir("data", Context.MODE_PRIVATE) + "/languages");
            if (langDir.exists())
              langDir.mkdir();
            File newFile = new File(langDir, filename);
            copyFile(inputStream, newFile);
            inputStream.close();
          } else if (filename.endsWith(".ttf") || filename.endsWith(".otf")) {
            File fontDir = new File(getDir("data", Context.MODE_PRIVATE) + "/fonts");
            if (fontDir.exists())
              fontDir.mkdir();
            File newFile = new File(fontDir, filename);
            copyFile(inputStream, newFile);
            inputStream.close();
          }
        } catch (Exception e) {
          didFail = true;
          Log.e("onActivityResult", e.getMessage());
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
    if (!KMManager.keyboardExists(this, KMManager.KMDefault_KeyboardID, KMManager.KMDefault_LanguageID)) {
      HashMap<String, String> kbInfo = new HashMap<String, String>();
      kbInfo.put(KMManager.KMKey_KeyboardID, KMManager.KMDefault_KeyboardID);
      kbInfo.put(KMManager.KMKey_LanguageID, KMManager.KMDefault_LanguageID);
      kbInfo.put(KMManager.KMKey_KeyboardName, KMManager.KMDefault_KeyboardName);
      kbInfo.put(KMManager.KMKey_LanguageName, KMManager.KMDefault_LanguageName);
      kbInfo.put(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(this, KMManager.KMDefault_KeyboardID));
      kbInfo.put(KMManager.KMKey_Font, KMManager.KMDefault_KeyboardFont);
      KMManager.addKeyboard(this, kbInfo);
    }

    KMManager.addKeyboardEventListener(this);
    KMManager.addKeyboardDownloadEventListener(this);

    checkUrl();
    getIntent().setData(null);
  }

  @Override
  protected void onPause() {
    super.onPause();
    KMManager.onPause();
    KMManager.removeKeyboardEventListener(this);
    KMManager.removeKeyboardDownloadEventListener(this);
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
    getActionBar().setBackgroundDrawable(getActionBarDrawable(this));
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
    String fbPackageName = "com.facebook.katana";

    for (ResolveInfo resolveInfo : resInfo) {
      String packageName = resolveInfo.activityInfo.packageName;

      Intent shareIntent = new Intent(android.content.Intent.ACTION_SEND);
      shareIntent.setType("text/plain");
      shareIntent.setPackage(packageName);

      String text = textView.getText().toString();
      String htmlMailFormat = "<html><head></head><body>%s%s</body></html>";
      String extraMailText = "<br><br>Sent from&nbsp<a href=\"http://keyman.com/android\">Keyman for Android</a>";

      if (!packageName.equals(fbPackageName)) {
        if (packageName.equals("com.android.email")) {
          // Text for email app, it doesn't currently support HTML
          shareIntent.putExtra(Intent.EXTRA_TEXT, text);
        } else if (packageName.equals("com.google.android.gm")) {
          // Html string for Gmail
          shareIntent.setType("message/rfc822");
          text = text.replace("<", "&lt;");
          text = text.replace(">", "&gt;");
          text = text.replace(" ", "&nbsp;");
          text = text.replace('\n', ' ');
          text = text.replace(" ", "<br>");
          shareIntent.putExtra(Intent.EXTRA_TEXT, Html.fromHtml(String.format(htmlMailFormat, text, extraMailText)));
        } else if (packageName.equals("com.twitter.android")) {
          // Text for Twitter
          shareIntent.putExtra(Intent.EXTRA_TEXT, text);
        } else {
          // Text for all others
          shareIntent.putExtra(Intent.EXTRA_TEXT, text);
        }

        shareIntents.add(shareIntent);
      }
    }

    Intent fbShareIntent = new Intent(this, FBShareActivity.class);
    fbShareIntent.putExtra("messageText", textView.getText().toString());
    fbShareIntent.putExtra("messageTextSize", (float) textSize);
    fbShareIntent.putExtra("messageTextTypeface", KMManager.getKeyboardTextFontFilename());
    shareIntents.add(new LabeledIntent(fbShareIntent, getPackageName(), "Facebook", R.drawable.ic_facebook_logo));

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
    dialogBuilder.setTitle(String.format("Text Size: %d", textSize));
    dialogBuilder.setView(textSizeController);
    dialogBuilder.setPositiveButton("OK", new DialogInterface.OnClickListener() {
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
        dialog.setTitle(String.format("Text Size: %d", textSize));
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
    dialogBuilder.setTitle("Clear Text");
    dialogBuilder.setMessage("\nAll text will be cleared\n");
    dialogBuilder.setPositiveButton("OK", new DialogInterface.OnClickListener() {
      @Override
      public void onClick(DialogInterface dialog, int which) {
        textView.setText("");
      }
    });

    dialogBuilder.setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
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

  private void showGetStarted() {
    Intent getStartedIntent = new Intent(this, GetStartedActivity.class);
    startActivity(getStartedIntent);
  }

  private boolean checkUrl() {
    // return true if requires user action (e.g. Download dialog)
    Intent i = getIntent();
    Uri data = i.getData();
    if (data == null)
      return false;

    final Context context = this;
    final String keyboard = data.getQueryParameter("keyboard");
    final String language = data.getQueryParameter("language");
    final String url = data.getQueryParameter("url");

    if (url != null) {
      /*
      int kbIndex = -1;
      if (keyboard != null && language != null)
        kbIndex = KMManager.getKeyboardIndex(this, keyboard, language);

      if (kbIndex >= 0) {
        KMManager.setKeyboard(context, kbIndex);
        return false;
      }*/

      String direct = data.getQueryParameter("direct");
      final boolean isDirect;
      if (direct != null && direct.equals("true")) {
        isDirect = true;
      } else {
        isDirect = false;
      }

      int index = url.lastIndexOf("/") + 1;
      String jsonFilename = "unknown";
      if (index >= 0 && index <= url.length()) {
        jsonFilename = url.substring(index);
      }
      AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
      dialogBuilder.setTitle("Custom Keyboard: " + jsonFilename);
      dialogBuilder.setMessage("Would you like to download this keyboard?");
      dialogBuilder.setPositiveButton("Download", new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog, int which) {
          // Download custom keyboard
          if (KMManager.hasConnection(context)) {
            KMManager.KMCustomKeyboardDownloader.download(context, url, isDirect, true);
          } else {
            Toast.makeText(context, "No internet connection", Toast.LENGTH_SHORT).show();
          }
        }
      });
      dialogBuilder.setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog, int which) {
          // Cancel
        }
      });

      AlertDialog dialog = dialogBuilder.create();
      dialog.show();

      return true;
    } else if (keyboard != null && language != null && !keyboard.trim().isEmpty() && !language.trim().isEmpty()) {
      int kbIndex = KMManager.getKeyboardIndex(this, keyboard, language);

      if (kbIndex >= 0) {
        KMManager.setKeyboard(context, kbIndex);
        return false;
      }

      String kbKey = String.format("%s_%s", language, keyboard);
      AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
      dialogBuilder.setTitle("Keyboard: " + kbKey);
      dialogBuilder.setMessage("Would you like to download this keyboard?");
      dialogBuilder.setPositiveButton("Download", new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog, int which) {
          // Download keyboard
          if (KMManager.hasConnection(context)) {
            KMManager.KMKeyboardDownloader.download(context, keyboard, language, true);
          } else {
            Toast.makeText(context, "No internet connection", Toast.LENGTH_SHORT).show();
          }
        }
      });
      dialogBuilder.setNegativeButton("Cancel", new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog, int which) {
          // Cancel
        }
      });

      AlertDialog dialog = dialogBuilder.create();
      dialog.show();

      return true;
    }

    return false;
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
    if (result > 0) {
      String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
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
          kbInfo.put(KMManager.KMKey_KeyboardID, keyboardID);
          kbInfo.put(KMManager.KMKey_LanguageID, langId);
          kbInfo.put(KMManager.KMKey_KeyboardName, keyboardName);
          kbInfo.put(KMManager.KMKey_LanguageName, langName);
          kbInfo.put(KMManager.KMKey_KeyboardVersion, kbVersion);
          kbInfo.put(KMManager.KMKey_Font, kFont);
          kbInfo.put(KMManager.KMKey_OskFont, kOskFont);
          if (i == 0) {
            if (KMManager.addKeyboard(this, kbInfo)) {
              KMManager.setKeyboard(keyboardID, langId, keyboardName, langName, kFont, kOskFont);
            }
          } else {
            KMManager.addKeyboard(this, kbInfo);
          }
        }
      } else {
        if (KMManager.addKeyboard(this, keyboardInfo)) {
          KMManager.setKeyboard(keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);
        }
      }
    } else {
      Toast.makeText(this, "Keyboard download failed", Toast.LENGTH_SHORT).show();
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