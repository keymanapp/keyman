/**
 * Copyright (C) 2017-2018 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.crashlytics.android.Crashlytics;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.tavultesoft.kmea.KMManager.KeyboardType;
import com.tavultesoft.kmea.KeyboardEventHandler.EventType;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.res.Configuration;
import android.graphics.Color;
import android.graphics.Rect;
import android.graphics.RectF;
import android.graphics.Typeface;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.GestureDetector;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.webkit.ConsoleMessage;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.widget.Button;
import android.widget.FrameLayout;
import android.widget.GridLayout;
import android.widget.PopupWindow;
import android.widget.PopupWindow.OnDismissListener;
import android.widget.TextView;
import android.widget.Toast;

final class KMKeyboard extends WebView {
  private final Context context;
  private KeyboardType keyboardType = KeyboardType.KEYBOARD_TYPE_UNDEFINED;
  private String packageID;
  private String keyboardID;
  private String keyboardName;
  private String keyboardVersion;

  private static String currentKeyboard = null;
  private static String txtFont = "";
  private static String oskFont = null;
  private static String keyboardRoot = "";
  private final String fontUndefined = "undefined";
  private GestureDetector gestureDetector;
  private static ArrayList<OnKeyboardEventListener> kbEventListeners = null;
  private boolean ShouldShowHelpBubble = false;
  private boolean isChiral = false;
  private static FirebaseAnalytics mFirebaseAnalytics;

  protected boolean keyboardSet = false;
  protected boolean keyboardPickerEnabled = true;
  protected boolean isHelpBubbleEnabled = true;

  public PopupWindow subKeysWindow = null;
  public PopupWindow keyPreviewWindow = null;
  public PopupWindow helpBubbleWindow = null;
  public ArrayList<HashMap<String, String>> subKeysList = null;
  public String[] subKeysWindowPos = {"0", "0"};
  public String specialOskFont = "";

  public KMKeyboard(Context context) {
    super(context);
    this.context = context;
    this.keyboardType = KeyboardType.KEYBOARD_TYPE_INAPP;
    initKMKeyboard(context);
  }

  public KMKeyboard(Context context, KeyboardType keyboardType) {
    super(context);
    this.context = context;
    this.keyboardType = keyboardType;
    initKMKeyboard(context);
  }

  @SuppressWarnings("deprecation")
  @SuppressLint("SetJavaScriptEnabled")
  public void initKMKeyboard(final Context context) {
    mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);

    setFocusable(false);
    clearCache(true);
    getSettings().setJavaScriptEnabled(true);
    getSettings().setBlockNetworkLoads(true);
    getSettings().setCacheMode(WebSettings.LOAD_NO_CACHE);
    getSettings().setSupportZoom(false);
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
      getSettings().setUseWideViewPort(true);
      getSettings().setLoadWithOverviewMode(true);
      setWebContentsDebuggingEnabled(true);
    }

    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP && Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1)
      setLayerType(View.LAYER_TYPE_SOFTWARE, null); // Disable hardware acceleration for API < 17, Keyman keyboard is slower without HWA but it causes some display issues.
    // (Tested on Samsung Galaxy Nexus running Android 4.1.2)

    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR2) {
      // These were deprecated in API 18
      getSettings().setRenderPriority(WebSettings.RenderPriority.HIGH);
      getSettings().setPluginState(WebSettings.PluginState.ON_DEMAND);
    }

    setWebChromeClient(new WebChromeClient() {
      public boolean onConsoleMessage(ConsoleMessage cm) {
        if (KMManager.isDebugMode()) {
          Log.d("KMEA", "Keyman JS Log: Line " + cm.lineNumber() + ", " + cm.sourceId() + ":" + cm.message());
        }

        // Send console errors to Firebase Analytics.
        // (Ignoring spurious message "No keyboard stubs exist = ...")
        // TODO: Analyze if this error warrants reverting to default keyboard
        // TODO: Fix base error rather than trying to ignore it "No keyboard stubs exist"
        if ((cm.messageLevel() == ConsoleMessage.MessageLevel.ERROR) && (!cm.message().startsWith("No keyboard stubs exist"))) {
          sendKMWError(cm.lineNumber(), cm.sourceId(), cm.message());
          Toast.makeText(context, "Fatal Error with " + currentKeyboard +
            ". Loading default keyboard", Toast.LENGTH_LONG).show();

          setKeyboard(KMManager.KMDefault_UndefinedPackageID, KMManager.KMDefault_KeyboardID,
            KMManager.KMDefault_LanguageID, KMManager.KMDefault_KeyboardName,
            KMManager.KMDefault_LanguageName, KMManager.KMDefault_KeyboardFont, null);
        }
        return true;
      }
    });

    gestureDetector = new GestureDetector(context, new GestureDetector.SimpleOnGestureListener() {
      @Override
      public boolean onDown(MotionEvent event) {
        return false;
      }

      @Override
      public void onLongPress(MotionEvent event) {
        showSubKeys(context);
      }

      @Override
      public boolean onSingleTapUp(MotionEvent event) {
        return false;
      }
    });
  }

  public void loadKeyboard() {
    keyboardSet = false;
    String htmlPath = "file://" + getContext().getDir("data", Context.MODE_PRIVATE) + "/" + KMManager.KMFilename_KeyboardHtml;
    loadUrl(htmlPath);
    setBackgroundColor(0);
  }

  public void hideKeyboard() {
    String jsString = "javascript:hideKeyboard()";
    loadUrl(jsString);
  }

  public void executeHardwareKeystroke(int code, int shift, int lstates) {
    String jsFormat = "javascript:executeHardwareKeystroke(%d,%d, %d)";
    String jsString = String.format(jsFormat, code, shift, lstates);
    loadUrl(jsString);
  }

  @SuppressLint("ClickableViewAccessibility")
  @Override
  public boolean onTouchEvent(MotionEvent event) {
    if (subKeysWindow != null) {
      //popupWindow.getContentView().dispatchTouchEvent(event);
      subKeysWindow.getContentView().findViewById(R.id.grid).dispatchTouchEvent(event);
    } else {
      //handleTouchEvent(event);
      gestureDetector.onTouchEvent(event);
      if (event.getAction() == MotionEvent.ACTION_UP)
        subKeysList = null;
    }

    return super.onTouchEvent(event);
  }

  public void onResume() {
    DisplayMetrics dms = context.getResources().getDisplayMetrics();
    int kbWidth = (int) (dms.widthPixels / dms.density);
    // Ensure window is loaded for javascript functions
    loadUrl(String.format(
      "javascript:window.onload = function(){ setOskWidth(\"%d\");"+
      "setOskHeight(\"0\"); };", kbWidth));
    if (ShouldShowHelpBubble) {
      ShouldShowHelpBubble = false;
      Handler handler = new Handler();
      handler.postDelayed(new Runnable() {
        @Override
        public void run() {
          loadUrl("javascript:showHelpBubble()");
        }
      }, 2000);
    }
  }

  public void onPause() {
    dismissKeyPreview(0);
    dismissSubKeysWindow();
    ShouldShowHelpBubble = dismissHelpBubble();
  }

  public void onDestroy() {
    dismissKeyPreview(0);
    dismissSubKeysWindow();
    dismissHelpBubble();
  }

  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);
    dismissKeyPreview(0);
    dismissSubKeysWindow();
    loadUrl(String.format("javascript:setOskWidth(%d)", newConfig.screenWidthDp));
    loadUrl("javascript:setOskHeight(0)");
    if (dismissHelpBubble()) {
      Handler handler = new Handler();
      handler.postDelayed(new Runnable() {
        @Override
        public void run() {
          loadUrl("javascript:showHelpBubble()");
        }
      }, 2000);
    }
  }

  public void dismissSubKeysWindow() {
    try {
      if (subKeysWindow != null && subKeysWindow.isShowing())
        subKeysWindow.dismiss();
    } catch (Exception e) {
    }
  }

  public static String currentKeyboard() {
    return currentKeyboard;
  }

  /**
   * Return the full path to the display text font. Usually used for creating a Typeface font
   * @return String
   */
  public static String textFontFilename() {
    return txtFont.isEmpty() ? "" : keyboardRoot + txtFont;
  }

  /**
   * Return the full path to the OSK font. Usually used for creating a Typeface font
   * @return String
   */
  public static String oskFontFilename() {
    return keyboardRoot + oskFont;
  }

  /**
   * Return the full path to the special OSK font,
   * which is with all the keyboard assets at the root app_data folder
   * @param String filename
   * @return String
   */
  public String specialOSKFontFilename(String filename) {
    return context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator + filename;
  }

  public boolean setKeyboard(String packageID, String keyboardID, String languageID) {
    if (packageID == null || keyboardID == null || languageID == null)
      return false;

    boolean retVal = true;
    String keyboardVersion = KMManager.getLatestKeyboardFileVersion(getContext(), packageID, keyboardID);
    if (!KMManager.shouldAllowSetKeyboard() || keyboardVersion == null) {
      Toast.makeText(context, "Can't set " + packageID + "::" + keyboardID + " for " +
        languageID + " language. Loading default keyboard", Toast.LENGTH_LONG).show();
      keyboardID = KMManager.KMDefault_KeyboardID;
      languageID = KMManager.KMDefault_LanguageID;
      retVal = false;
    }

    String kbKey = String.format("%s_%s", languageID, keyboardID);
    //if (kbKey.equals(currentKeyboard))
    //  return false;

    String keyboardName = "";
    String languageName = "";
    keyboardVersion = KMManager.getLatestKeyboardFileVersion(getContext(), packageID, keyboardID);

    setKeyboardRoot(packageID);
    String keyboardPath = makeKeyboardPath(packageID, keyboardID, keyboardVersion);

    String tFont = "''";
    String oFont = null;
    HashMap<String, HashMap<String, String>> kbsInfo = LanguageListActivity.getKeyboardsInfo(context);
    if (kbsInfo != null) {
      HashMap<String, String> kbInfo = kbsInfo.get(kbKey);
      if (kbInfo != null) {
        keyboardName = kbInfo.get(KMManager.KMKey_KeyboardName).replace("'", "\\'");
        languageName = kbInfo.get(KMManager.KMKey_LanguageName).replace("'", "\\'");
        txtFont = getFontFilename(kbInfo.get(KMManager.KMKey_Font));
        oskFont = getFontFilename(kbInfo.get(KMManager.KMKey_OskFont));

        if (!txtFont.isEmpty()) {
          tFont = makeFontPaths(kbInfo.get(KMManager.KMKey_Font));
        }

        if (!oskFont.isEmpty()) {
          oFont = makeFontPaths(kbInfo.get(KMManager.KMKey_Font));
        } else {
          oskFont = null;
        }

        if (oFont == null) {
          oFont = tFont;
        }
      } else {
        return false;
      }
    } else {
      return false;
    }

    if (tFont.equals("''")) {
      tFont = fontUndefined;
    }
    if (oFont.equals("''")) {
      oFont = fontUndefined;
    }

    // Escape single-quoted names for javascript call
    keyboardName = keyboardName.replaceAll("\'", "\\\\'"); // Double-escaped-backslash b/c regex.
    languageName = languageName.replaceAll("\'", "\\\\'");

    String jsFormat = "javascript:setKeymanLanguage('%s','%s','%s','%s','%s', %s, %s, '%s')";
    String jsString = String.format(jsFormat, keyboardName, keyboardID, languageName, languageID, keyboardPath, tFont, oFont, packageID);
    loadUrl(jsString);

    this.packageID = packageID;
    this.keyboardID = keyboardID;
    this.keyboardName = keyboardName;
    this.keyboardVersion = keyboardVersion;
    currentKeyboard = kbKey;
    keyboardSet = true;
    saveCurrentKeyboardIndex();
    if (dismissHelpBubble()) {
      Handler handler = new Handler();
      handler.postDelayed(new Runnable() {
        @Override
        public void run() {
          loadUrl("javascript:showHelpBubble()");
        }
      }, 2000);
    }

    KeyboardEventHandler.notifyListeners(kbEventListeners, keyboardType, EventType.KEYBOARD_CHANGED, currentKeyboard);
    return retVal;
  }

  public boolean setKeyboard(String packageID, String keyboardID, String languageID, String keyboardName, String languageName, String kFont, String kOskFont) {
    if (packageID == null || keyboardID == null || languageID == null || keyboardName == null || languageName == null) {
      return false;
    }

    boolean retVal = true;
    String keyboardVersion = KMManager.getLatestKeyboardFileVersion(getContext(), packageID, keyboardID);
    if (!KMManager.shouldAllowSetKeyboard() || keyboardVersion == null) {
      Toast.makeText(context, "Can't set " + packageID + "::" + keyboardID + " for " +
        languageID + " language. Loading default keyboard", Toast.LENGTH_LONG).show();
      packageID = KMManager.KMDefault_UndefinedPackageID;
      keyboardID = KMManager.KMDefault_KeyboardID;
      languageID = KMManager.KMDefault_LanguageID;
      keyboardName = KMManager.KMDefault_KeyboardName;
      languageName = KMManager.KMDefault_LanguageName;
      kFont = KMManager.KMDefault_KeyboardFont;
      kOskFont = kFont;
      retVal = false;
    }

    String kbKey = String.format("%s_%s", languageID, keyboardID);
    //if (kbKey.equals(currentKeyboard))
    //  return false;

    keyboardVersion = KMManager.getLatestKeyboardFileVersion(getContext(), packageID, keyboardID);

    setKeyboardRoot(packageID);
    String keyboardPath = makeKeyboardPath(packageID, keyboardID, keyboardVersion);

    String tFont = "''";
    String oFont = null;
    if (kFont == null) {
      txtFont = "";
    } else {
      if (kFont.endsWith(".ttf") || kFont.endsWith(".otf")) {
        txtFont = kFont;
        tFont = String.format("{\"family\":\"font_family_%s\",\"files\":[\"%s%s\"]}", kFont.substring(0, kFont.length() - 4), keyboardRoot, kFont);
      } else {
        txtFont = getFontFilename(kFont);
        if (!txtFont.isEmpty()) {
          tFont = makeFontPaths(kFont);
        }
      }
    }

    if (kOskFont == null || kOskFont.isEmpty()) {
      oskFont = null;
    } else {
      if (kOskFont.endsWith(".ttf") || kOskFont.endsWith(".otf")) {
        oskFont = kOskFont;
        oFont = String.format("{\"family\":\"font_family_%s\",\"files\":[\"%s%s\"]}", kOskFont.substring(0, kOskFont.length() - 4), keyboardRoot, kOskFont);
      } else {
        oskFont = getFontFilename(kOskFont);
        if (!oskFont.isEmpty()) {
          oFont = makeFontPaths(kOskFont);
        } else {
          oskFont = null;
        }
      }
    }

    if (oFont == null) {
      oFont = tFont;
    }

    if (tFont.equals("''")) {
      tFont = fontUndefined;
    }
    if (oFont.equals("''")) {
      oFont = fontUndefined;
    }

    // Escape single-quoted names for javascript call
    keyboardName = keyboardName.replaceAll("\'", "\\\\'"); // Double-escaped-backslash b/c regex.
    languageName = languageName.replaceAll("\'", "\\\\'");

    String jsFormat = "javascript:setKeymanLanguage('%s','%s','%s','%s','%s', %s, %s, '%s')";
    String jsString = String.format(jsFormat, keyboardName, keyboardID, languageName, languageID, keyboardPath, tFont, oFont, packageID);
    loadUrl(jsString);

    this.packageID = packageID;
    this.keyboardID = keyboardID;
    this.keyboardName = keyboardName;
    this.keyboardVersion = keyboardVersion;
    currentKeyboard = kbKey;
    keyboardSet = true;
    saveCurrentKeyboardIndex();
    if (dismissHelpBubble()) {
      Handler handler = new Handler();
      handler.postDelayed(new Runnable() {
        @Override
        public void run() {
          loadUrl("javascript:showHelpBubble()");
        }
      }, 2000);
    }

    KeyboardEventHandler.notifyListeners(kbEventListeners, keyboardType, EventType.KEYBOARD_CHANGED, currentKeyboard);

    return retVal;
  }

  public void setChirality(boolean flag) {
    this.isChiral = flag;
  }

  public boolean getChirality() {

    return this.isChiral;

  }

  // Set the base path of the keyboard depending on the package ID
  private void setKeyboardRoot(String packageID) {
    if (packageID.equals(KMManager.KMDefault_UndefinedPackageID)) {
      this.keyboardRoot = (context.getDir("data", Context.MODE_PRIVATE).toString() +
        File.separator + KMManager.KMDefault_UndefinedPackageID + File.separator);
    } else {
      this.keyboardRoot = (context.getDir("data", Context.MODE_PRIVATE).toString() +
        File.separator + KMManager.KMDefault_AssetPackages + File.separator + packageID + File.separator);
    }
  }

  public String getKeyboardRoot() {
    return this.keyboardRoot;
  }

  private String makeKeyboardPath(String packageID, String keyboardID, String keyboardVersion) {
    String keyboardPath;
    if (packageID.equals(KMManager.KMDefault_UndefinedPackageID)) {
      keyboardPath = getKeyboardRoot() + keyboardID + "-" + keyboardVersion + ".js";
    } else {
      keyboardPath = getKeyboardRoot() + keyboardID + ".js";
    }
    return keyboardPath;
  }

  private void sendKMWError(int lineNumber, String sourceId, String message) {
    Bundle params = new Bundle();
    // Error info
    params.putInt("cm_lineNumber", lineNumber);
    params.putString("cm_sourceID", sourceId);
    params.putString("cm_message", message);

    // Keyboard info
    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      params.putString("keyboardType", "INAPP");
    } else if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      params.putString("keyboardType", "SYSTEM");
    }  else {
      params.putString("keyboardType", "UNDEFINED");
    }
    params.putString("packageID", this.packageID);
    params.putString("keyboardID", this.keyboardID);
    params.putString("keyboardName", this.keyboardName);
    params.putString("keyboardVersion", this.keyboardVersion);

    mFirebaseAnalytics.logEvent("kmw_console_error", params);
  }

  // Extract Unicode numbers (\\uxxxx) from a layer to character string.
  // Ignores empty strings and layer names
  // Returns: String
  protected String convertKeyText(String ktext) {
    String title = "";
    String[] values = ktext.split("\\\\u");
    int length = values.length;
    for (int j = 0; j < length; j++) {
      if (!values[j].isEmpty() && !values[j].contains("-")) {
        int c = Integer.parseInt(values[j], 16);
        // TODO: \\uxxxxxx will need to be handled with title.codePointAt(c)
        title += String.valueOf((char) c);
      }
    }
    return title;
  }

  private void saveCurrentKeyboardIndex() {
    SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = prefs.edit();
    editor.putInt(KMManager.KMKey_UserKeyboardIndex, KMManager.getCurrentKeyboardIndex(context));
    editor.commit();
  }

  /**
   * getFontFilename
   * Parse a Font JSON object and return the font filename (ending in .ttf or .otf)
   * @param jsonString String - Font JSON object as a string
   * @return String - Filename for the font. If font is invalid, return ""
   */
  private String getFontFilename(String jsonString) {
    String font = "";
    try {
      JSONObject fontObj = new JSONObject(jsonString);
      JSONArray sourceArray = fontObj.optJSONArray(KMManager.KMKey_FontSource);
      if (sourceArray != null) {
        String fontFile;
        int length = sourceArray.length();
        for (int i = 0; i < length; i++) {
          fontFile = sourceArray.getString(i);
          if (fontFile.endsWith(".ttf") || fontFile.endsWith(".otf")) {
            font = fontFile;
            break;
          }
        }
      } else {
        String fontFile = fontObj.optString(KMManager.KMKey_FontSource);
        if (fontFile != null) {
          if (fontFile.endsWith(".ttf") || fontFile.endsWith(".otf")) {
            font = fontFile;
          }
        }
      }
    } catch (JSONException e) {
      font = "";
    }

    return font;
  }

  @SuppressLint("InflateParams")
  private void showSubKeys(Context context) {
    if (subKeysList == null || subKeysWindow != null) {
      return;
    }

    WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
    DisplayMetrics metrics = new DisplayMetrics();
    wm.getDefaultDisplay().getMetrics(metrics);
    float density = metrics.density;

    String[] pos = subKeysWindowPos;
    int x = (int) (Float.valueOf(pos[0]) * density);
    int y = (int) (Float.valueOf(pos[1]) * density);

    int kbWidth = getWidth();
    float pvWidth, pvHeight;

    float margin = getResources().getDimension(R.dimen.popup_margin);
    int padding = getResources().getDimensionPixelSize(R.dimen.popup_padding);
    int rows, columns;
    float buttonWidth = getResources().getDimension(R.dimen.key_width);
    float buttonHeight = getResources().getDimension(R.dimen.key_height);
    float arrowWidth = getResources().getDimension(R.dimen.popup_arrow_width);
    float arrowHeight = getResources().getDimension(R.dimen.popup_arrow_height);
    float offset_y = getResources().getDimension(R.dimen.popup_offset_y);

    //int orientation = getResources().getConfiguration().orientation;
    //columns = (orientation == Configuration.ORIENTATION_PORTRAIT)?6:10;
    columns = (int) ((getWidth() - margin) / (buttonWidth + margin));
    int subKeysCount = subKeysList.size();
    if (subKeysCount <= columns) {
      rows = 1;
      pvWidth = (subKeysCount * (buttonWidth + padding)) + 2 * margin + padding;
      pvHeight = (buttonHeight + padding) + 2 * margin + padding + arrowHeight;
    } else {
      rows = (subKeysCount / columns);
      if (subKeysCount % columns > 0) {
        rows++;
      }

      if (subKeysCount % rows == 0) {
        columns = subKeysCount / rows;
      } else {
        int s = (columns * rows - subKeysCount) / 2;
        columns -= s / (rows - 1);
      }

      pvWidth = (columns * (buttonWidth + padding)) + 2 * margin + padding;
      pvHeight = (rows * (buttonHeight + padding)) + 2 * margin + padding + arrowHeight;
    }

    LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    View contentView = inflater.inflate(R.layout.subkeys_popup_layout, null, false);
    KMPopoverView popoverView = (KMPopoverView) contentView.findViewById(R.id.kmPopoverView);
    popoverView.setSize((int) pvWidth, (int) pvHeight);
    popoverView.setArrowSize(arrowWidth, arrowHeight);

    float px = x - pvWidth / 2.0f;
    float py = y + offset_y - pvHeight;
    if (px < 0) {
      px = 0;
    } else if ((px + pvWidth) > kbWidth) {
      px = kbWidth - pvWidth;
    }

    if (px == 0) {
      popoverView.setArrowPosX(x);
    } else if (px == (kbWidth - pvWidth)) {
      popoverView.setArrowPosX(x - px);
    } else {
      popoverView.setArrowPosX(pvWidth / 2.0f);
    }

    popoverView.redraw();

    GridLayout grid = (GridLayout) contentView.findViewById(R.id.grid);
    grid.setColumnCount(columns);

    for (int i = 0; i < subKeysCount; i++) {
      Button button = (Button) inflater.inflate(R.layout.subkey_layout, null);
      button.setId(i + 1);
      button.setLayoutParams(new FrameLayout.LayoutParams((int) buttonWidth, (int) buttonHeight));
      button.setOnClickListener(new OnClickListener() {
        @Override
        public void onClick(View v) {
          int index = v.getId() - 1;
          String keyId = subKeysList.get(index).get("keyId");
          String keyText = getSubkeyText(keyId, subKeysList.get(index).get("keyText"));
          String jsFormat = "javascript:executePopupKey('%s','%s')";
          String jsString = String.format(jsFormat, keyId, keyText);
          loadUrl(jsString);
        }
      });
      button.setClickable(false);

      // Show existing text for subkeys. If subkey text is blank, get from id
      String kId = subKeysList.get(i).get("keyId");
      String kText = getSubkeyText(kId, subKeysList.get(i).get("keyText"));
      String title = convertKeyText(kText);

      // Disable Android's default uppercasing transformation on buttons.
      button.setTransformationMethod(null);
      button.setText(title);

      if (!specialOskFont.isEmpty()) {
        button.setTypeface(KMManager.getFontTypeface(context, specialOSKFontFilename(specialOskFont)));
      } else {
        Typeface font = KMManager.getFontTypeface(context, (oskFont != null) ? oskFontFilename() : textFontFilename());
        if (font != null) {
          button.setTypeface(font);
        } else {
          button.setTypeface(Typeface.SANS_SERIF);
        }
      }

      FrameLayout frame = new FrameLayout(context);
      frame.setPadding(padding, padding, 0, 0);
      frame.addView(button);
      grid.addView(frame);
    }

    grid.setOnTouchListener(new OnTouchListener() {
      @SuppressLint("ClickableViewAccessibility")
      @Override
      public boolean onTouch(View v, MotionEvent event) {
        int action = event.getAction();
        int tx = (int) event.getRawX();
        int ty = (int) event.getRawY();

        if (action == MotionEvent.ACTION_UP) {
          int count = ((ViewGroup) v).getChildCount();
          for (int i = 0; i < count; i++) {
            FrameLayout frame = (FrameLayout) ((ViewGroup) v).getChildAt(i);
            Button button = (Button) frame.getChildAt(0);
            if (button.isPressed()) {
              button.performClick();
              break;
            }
          }
          dismissSubKeysWindow();
        } else if (action == MotionEvent.ACTION_MOVE) {
          int count = ((ViewGroup) v).getChildCount();
          for (int i = 0; i < count; i++) {
            FrameLayout frame = (FrameLayout) ((ViewGroup) v).getChildAt(i);
            Button button = (Button) frame.getChildAt(0);
            int[] pos = new int[2];
            button.getLocationOnScreen(pos);
            Rect rect = new Rect();
            button.getDrawingRect(rect);
            rect.offset(pos[0], pos[1]);
            if (rect.contains(tx, ty)) {
              button.setPressed(true);
            } else {
              button.setPressed(false);
            }
          }
        }
        return false;
      }
    });

    subKeysWindow = new PopupWindow(contentView, (int) pvWidth, (int) pvHeight, false);
    subKeysWindow.setTouchable(true);
    subKeysWindow.setOnDismissListener(new OnDismissListener() {
      @Override
      public void onDismiss() {
        subKeysList = null;
        subKeysWindow = null;
        String jsString = "javascript:popupVisible(0)";
        loadUrl(jsString);
      }
    });

    int posX, posY;
    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      int[] kbPos = new int[2];
      KMKeyboard.this.getLocationOnScreen(kbPos);
      posX = (int) px;
      posY = kbPos[1] + (int) py;
    } else {
      int[] kbPos = new int[2];
      KMKeyboard.this.getLocationInWindow(kbPos);
      posX = (int) px;
      posY = kbPos[1] + (int) py;
    }

    dismissHelpBubble();
    dismissKeyPreview(0);
    //subKeysWindow.setAnimationStyle(R.style.PopupAnim);
    subKeysWindow.showAtLocation(KMKeyboard.this, Gravity.TOP | Gravity.LEFT, posX, posY);
    String jsString = "javascript:popupVisible(1)";
    loadUrl(jsString);
  }

  // Attempt to get the subkey text.
  // If the subkey popup text is empty, parse the ID
  private String getSubkeyText(String keyID, String keyText) {
    String text = keyText;
    if (text.isEmpty()) {
      text = keyID.replaceAll("U_", "\\\\u");
    }
    return text;
  }

  /**
   * Take a font JSON object and adjust to pass to JS
   * 1. Replace "source" keys for "files" keys
   * 2. Create full font paths for .ttf or
   * .svg for Android 4.2 or 4.3 OSK
   * @param font String font JSON object as a string
   * @return String of modified font information with full paths. If font is invalid, return "''"
   */
  private String makeFontPaths(String font) {
    try {
      JSONObject fontObj = new JSONObject(font);
      JSONArray sourceArray;
      String fontFile;

      // Replace "sources" key with "files"
      if (fontObj.has(KMManager.KMKey_FontSource)) {
        fontObj.put(KMManager.KMKey_FontFiles, fontObj.get(KMManager.KMKey_FontSource));
        fontObj.remove(KMManager.KMKey_FontSource);
      }

      Object obj = fontObj.get(KMManager.KMKey_FontFiles);
      if (obj instanceof String) {
        fontFile = fontObj.getString(KMManager.KMKey_FontFiles);
        fontObj.put(KMManager.KMKey_FontFiles, keyboardRoot + obj);
        return fontObj.toString();
      } else if (obj instanceof JSONArray) {
        sourceArray = fontObj.optJSONArray(KMManager.KMKey_FontFiles);
        if (sourceArray != null) {
          for (int i = 0; i < sourceArray.length(); i++) {
            fontFile = sourceArray.getString(i);
            if (fontFile.contains(".ttf") ||
              (fontFile.contains(".svg") && (Build.VERSION.SDK_INT == Build.VERSION_CODES.JELLY_BEAN_MR1 || Build.VERSION.SDK_INT == Build.VERSION_CODES.JELLY_BEAN_MR2))) {
              fontObj.put(KMManager.KMKey_FontFiles, keyboardRoot + fontFile);
              fontObj.remove(KMManager.KMKey_FontSource);
              return fontObj.toString();
            }
          }
        }
      }
    } catch (JSONException e) {
      return "''";
    }

    return font;
  }

  @SuppressLint("InflateParams")
  protected void showKeyPreview(Context context, int px, int py, RectF baseKeyFrame, String text) {
    WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
    DisplayMetrics metrics = new DisplayMetrics();
    wm.getDefaultDisplay().getMetrics(metrics);
    float density = metrics.density;

    if (keyPreviewWindow != null && keyPreviewWindow.isShowing()) {
      View contentView = keyPreviewWindow.getContentView();
      KMKeyPreviewView keyPreview = (KMKeyPreviewView) contentView.findViewById(R.id.kmKeyPreviewView);
      TextView textView = (TextView) contentView.findViewById(R.id.textView1);
      textView.setText(text);
      Typeface font = KMManager.getFontTypeface(context, (oskFont != null) ? oskFontFilename() : textFontFilename());
      if (font != null) {
        textView.setTypeface(font);
      } else {
        textView.setTypeface(Typeface.SANS_SERIF);
      }

      int w = (int) (baseKeyFrame.width() * density);
      int h = (int) (baseKeyFrame.height() * density);
      RectF frame = keyPreview.setKeySize(w, h);
      keyPreview.redraw();

      float offset_y = getResources().getDimension(R.dimen.popup_offset_y);
      int posX, posY;
      if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
        int[] kbPos = new int[2];
        KMKeyboard.this.getLocationOnScreen(kbPos);
        posX = (int) (px * density - frame.width() / 2.0f);
        posY = kbPos[1] + (int) (py * density - frame.height() + offset_y);
      } else {
        int[] kbPos = new int[2];
        KMKeyboard.this.getLocationInWindow(kbPos);
        posX = (int) (px * density - frame.width() / 2.0f);
        posY = kbPos[1] + (int) (py * density - frame.height() + offset_y);
      }

      keyPreviewWindow.update(posX, posY, (int) frame.width(), (int) frame.height());
      return;
    }

    LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    View contentView = inflater.inflate(R.layout.key_preview_layout, null, false);
    KMKeyPreviewView keyPreview = (KMKeyPreviewView) contentView.findViewById(R.id.kmKeyPreviewView);
    TextView textView = (TextView) contentView.findViewById(R.id.textView1);
    textView.setText(text);
    Typeface font = KMManager.getFontTypeface(context, (oskFont != null) ? oskFontFilename() : textFontFilename());
    if (font != null) {
      textView.setTypeface(font);
    } else {
      textView.setTypeface(Typeface.SANS_SERIF);
    }

    int w = (int) (baseKeyFrame.width() * density);
    int h = (int) (baseKeyFrame.height() * density);
    RectF frame = keyPreview.setKeySize(w, h);
    keyPreview.redraw();
    keyPreviewWindow = new PopupWindow(contentView, (int) frame.width(), (int) frame.height(), false);
    keyPreviewWindow.setTouchable(true);
    keyPreviewWindow.setOnDismissListener(new OnDismissListener() {
      @Override
      public void onDismiss() {
        keyPreviewWindow = null;
      }
    });

    float offset_y = getResources().getDimension(R.dimen.popup_offset_y);
    int posX, posY;
    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      int[] kbPos = new int[2];
      KMKeyboard.this.getLocationOnScreen(kbPos);
      posX = (int) (px * density - frame.width() / 2.0f);
      posY = kbPos[1] + (int) (py * density - frame.height() + offset_y);
    } else {
      int[] kbPos = new int[2];
      KMKeyboard.this.getLocationInWindow(kbPos);
      posX = (int) (px * density - frame.width() / 2.0f);
      posY = kbPos[1] + (int) (py * density - frame.height() + offset_y);
    }

    dismissHelpBubble();
    //keyPreviewWindow.setAnimationStyle(R.style.KeyPreviewAnim);
    keyPreviewWindow.showAtLocation(KMKeyboard.this, Gravity.TOP | Gravity.LEFT, posX, posY);
  }

  protected void dismissKeyPreview(long delay) {
    // dismiss after delay
    Handler handler = new Handler();
    handler.postDelayed(new Runnable() {
      @Override
      public void run() {
        try {
          if (keyPreviewWindow != null && keyPreviewWindow.isShowing())
            keyPreviewWindow.dismiss();
        } catch (Exception e) {
        }
      }
    }, delay);
  }

  @SuppressLint("InflateParams")
  protected void showHelpBubble(Context context, float fx, float fy) {
    if (!isHelpBubbleEnabled || keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      return; // Help bubble is disabled for System-wide keyboard
    }

    if (KMManager.getGlobeKeyAction(keyboardType) == KMManager.GlobeKeyAction.GLOBE_KEY_ACTION_DO_NOTHING) {
      return; // Help bubble is disabled if globe key has no action
    }

    if (KMManager.getGlobeKeyAction(keyboardType) == KMManager.GlobeKeyAction.GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD) {
      // Help bubble is disabled if next keyboard is not available for this action
      ArrayList<HashMap<String, String>> keyboardsList = KMManager.getKeyboardsList(context);
      if (keyboardsList == null) {
        return;
      }

      if (keyboardsList.size() < 2) {
        return;
      }
    }

    WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
    DisplayMetrics metrics = new DisplayMetrics();
    wm.getDefaultDisplay().getMetrics(metrics);
    float density = metrics.density;

    int x = (int) (fx * density);
    int y = (int) (fy * density);

    int kbWidth = getWidth();
    float pvWidth = getResources().getDimension(R.dimen.help_bubble_width);
    float pvHeight = getResources().getDimension(R.dimen.help_bubble_height);

    float arrowWidth = getResources().getDimension(R.dimen.popup_arrow_width);
    float arrowHeight = getResources().getDimension(R.dimen.popup_arrow_height);
    float offset_x = getResources().getDimension(R.dimen.help_bubble_offset_x);
    float offset_y = getResources().getDimension(R.dimen.help_bubble_offset_y);

    LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    View contentView = inflater.inflate(R.layout.help_bubble_layout, null, false);
    KMPopoverView popoverView = (KMPopoverView) contentView.findViewById(R.id.kmPopoverView);
    popoverView.setBackgroundColor(Color.rgb(253, 244, 196));
    popoverView.setBackgroundColor2(Color.rgb(233, 224, 176));
    popoverView.setBorderColor(Color.rgb(128, 64, 64));
    popoverView.setSize((int) pvWidth, (int) pvHeight);
    popoverView.setArrowSize(arrowWidth, arrowHeight);

    float px = x + offset_x - pvWidth / 2.0f;
    float py = y + offset_y - pvHeight;
    if (px < 0) {
      px = 0;
    } else if ((px + pvWidth) > kbWidth) {
      px = kbWidth - pvWidth;
    }

    if (px == 0) {
      popoverView.setArrowPosX(x);
    } else if (px == (kbWidth - pvWidth)) {
      popoverView.setArrowPosX(x - px);
    } else {
      popoverView.setArrowPosX(pvWidth / 2.0f);
    }

    popoverView.redraw();

    dismissHelpBubble();
    helpBubbleWindow = new PopupWindow(contentView, (int) pvWidth, (int) pvHeight, false);
    helpBubbleWindow.setTouchable(true);
    helpBubbleWindow.setOnDismissListener(new OnDismissListener() {
      @Override
      public void onDismiss() {
        helpBubbleWindow = null;
      }
    });

    int posX, posY;
    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      int[] kbPos = new int[2];
      getLocationOnScreen(kbPos);
      posX = (int) px;
      posY = kbPos[1] + (int) py;
    } else {
      int[] kbPos = new int[2];
      getLocationInWindow(kbPos);
      posX = (int) px;
      posY = kbPos[1] + (int) py;
    }

    helpBubbleWindow.setAnimationStyle(R.style.PopupAnim);
    if (getWindowToken() != null) {
      helpBubbleWindow.showAtLocation(KMKeyboard.this, Gravity.TOP | Gravity.LEFT, posX, posY);
    } else {
      helpBubbleWindow = null;
      ShouldShowHelpBubble = true;
    }
  }

  protected boolean dismissHelpBubble() {
    try {
      if (helpBubbleWindow != null && helpBubbleWindow.isShowing()) {
        helpBubbleWindow.dismiss();
        return true;
      } else {
        return false;
      }
    } catch (Exception e) {
      return true;
    }
  }

  public static void addOnKeyboardEventListener(OnKeyboardEventListener listener) {
    if (kbEventListeners == null) {
      kbEventListeners = new ArrayList<OnKeyboardEventListener>();
    }

    if (listener != null && !kbEventListeners.contains(listener)) {
      kbEventListeners.add(listener);
    }
  }

  public static void removeOnKeyboardEventListener(OnKeyboardEventListener listener) {
    if (kbEventListeners != null) {
      kbEventListeners.remove(listener);
    }
  }

  /* Implement handleTouchEvent to catch long press gesture without using Android system default time
  private float touchDownStartX = 0;
  private float touchDownStartY = 0;
  private void handleTouchEvent(MotionEvent event) {
    Handler hander = new Handler();
    Runnable showPopup = new Runnable() {
      @Override
      public void run() {
        showSubKeys(getContext());
      }
    };

    float mt = 10;
    switch (event.getAction()) {
    case MotionEvent.ACTION_DOWN:
      touchDownStartX = event.getX();
      touchDownStartY = event.getY();
      hander.postDelayed(showPopup, 500);
      break;
    case MotionEvent.ACTION_CANCEL:
    case MotionEvent.ACTION_UP:
      hander.removeCallbacks(showPopup);
      popupKeysList = null;
      break;
    case MotionEvent.ACTION_MOVE:
      if (event.getX() > (touchDownStartX + mt) || event.getX() < (touchDownStartX - mt) || event.getY() > (touchDownStartY + mt) || event.getY() < (touchDownStartY - mt)) {
        hander.removeCallbacks(showPopup);
        popupKeysList = null;
      }
      break;
    }  
  }
  */

  /* 
   * Override onDraw method to render missing characters for Android API 17 & 18 font rendering bug. 
    private Bitmap keyboardImage;
    private Canvas keyboardCanvas;
    private Paint paint;
   * while initializing
      paint = new Paint();
      paint.setAntiAlias(true);
      paint.setColor(Color.BLACK);
      paint.setShadowLayer(0.5f, 0, 0.5f, Color.WHITE);
      paint.setTypeface(KMManager.getFontTypeface(context, kbFont));
      paint.setTextSize(37.5f);
   * after setting keyboard
      paint.setTypeface(KMManager.getFontTypeface(getContext(), kbFont));
      invalidate();

  @Override
  protected void onDraw(Canvas canvas) {
    super.onDraw(canvas);
    super.onDraw(keyboardCanvas);

    //Matrix m = new Matrix();
      //m.preScale(1, -1);
      //Bitmap mImage = Bitmap.createBitmap(keyboardImage, 0, 0, keyboardImage.getWidth(), keyboardImage.getHeight(), m, false);
    //canvas.drawBitmap(mImage, 0, 0, null);

    canvas.drawBitmap(keyboardImage, 0, 0, null);
    float w = 55;
    float h = 70;
    float x = 390;
    float y = 250;
    boolean render = true;
    for (int i = (int) x; i < x+w; i++) {
      for (int j = (int) y; j < y+h; j++) {
        int pixel = keyboardImage.getPixel(i, j);
        if (pixel == Color.BLACK) {
          render = false;
          break;
        }
      }

      if (render == false)
        break;
    }
    //canvas.drawRect(new RectF(x, y, x+w, y+h), paint_bm);
    if (render)
      canvas.drawText("\u100a", 400.0f, 292, paint);
  }

  @Override
  protected void onSizeChanged (int w, int h, int ow, int oh) {
    super.onSizeChanged(w, h, ow, oh);
    keyboardImage = Bitmap.createBitmap(w, h, Bitmap.Config.RGB_565);
    keyboardCanvas = new Canvas(keyboardImage);
  }
  */
}