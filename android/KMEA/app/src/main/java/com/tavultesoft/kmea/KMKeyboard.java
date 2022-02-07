/**
 * Copyright (C) 2017-2018 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.tavultesoft.kmea.BaseActivity;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.KeyboardController;
import com.tavultesoft.kmea.KMManager.KeyboardType;
import com.tavultesoft.kmea.KeyboardEventHandler.EventType;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.KMLog;
import com.tavultesoft.kmea.util.KMString;

import android.Manifest;
import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
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
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

import io.sentry.Breadcrumb;
import io.sentry.Sentry;
import io.sentry.SentryLevel;

final class KMKeyboard extends WebView {
  private static final String TAG = "KMKeyboard";
  private final Context context;
  private KeyboardType keyboardType = KeyboardType.KEYBOARD_TYPE_UNDEFINED;
  private String packageID;
  private String keyboardID;
  private String keyboardName;
  private String keyboardVersion;

  protected ArrayList<String> javascriptAfterLoad = new ArrayList<String>();

  private static String currentKeyboard = null;
  private static String txtFont = "";
  private static String oskFont = null;
  private static String keyboardRoot = "";
  private final String fontUndefined = "undefined";
  private GestureDetector gestureDetector;
  private static ArrayList<OnKeyboardEventListener> kbEventListeners = null;
  private boolean ShouldShowHelpBubble = false;
  private boolean isChiral = false;

  private int currentKeyboardErrorReports = 0;

  protected boolean keyboardSet = false;
  protected boolean keyboardPickerEnabled = true;
  protected boolean isHelpBubbleEnabled = true;

  public PopupWindow subKeysWindow = null;
  public PopupWindow keyPreviewWindow = null;
  public PopupWindow helpBubbleWindow = null;

  public ArrayList<HashMap<String, String>> subKeysList = null;
  public String[] subKeysWindowPos = {"0", "0"};

  // public something-something for the suggestion.
  public PopupWindow suggestionMenuWindow = null;
  public double[] suggestionWindowPos = {0, 0};
  public String suggestionJSON = null;

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
    setFocusable(false);
    clearCache(true);
    getSettings().setJavaScriptEnabled(true);
    getSettings().setAllowFileAccess(true);

    // Normally, this would be true to prevent the WebView from accessing the network.
    // But this needs to false for sending embedded KMW crash reports to Sentry (keymanapp/keyman#3825)
    if (KMManager.hasInternetPermission(context)) {
      // Throws SecurityException if INTERNET permission not granted
      getSettings().setBlockNetworkLoads(!KMManager.getMaySendCrashReport());
    }

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
        String msg = KMString.format("KMW JS Log: Line %d, %s:%s", cm.lineNumber(), cm.sourceId(), cm.message());
        if (KMManager.isDebugMode()) {
          if (cm.messageLevel() == ConsoleMessage.MessageLevel.ERROR) {
            Log.d(TAG, msg);
          }
        }

        // Send console errors to Sentry in case they're missed by KMW sentryManager
        // (Ignoring spurious message "No keyboard stubs exist = ...")
        // TODO: Fix base error rather than trying to ignore it "No keyboard stubs exist"

        if ((cm.messageLevel() == ConsoleMessage.MessageLevel.ERROR) && (!cm.message().startsWith("No keyboard stubs exist"))) {
          // Make Toast notification of error and send log about falling back to default keyboard (ignore language ID)
          // Sanitize sourceId info
          String NAVIGATION_PATTERN = "^(.*)?(keyboard\\.html#[^-]+)-.*$";
          String sourceID = cm.sourceId().replaceAll(NAVIGATION_PATTERN, "$1$2");
          sendKMWError(cm.lineNumber(), sourceID, cm.message());
          sendError(packageID, keyboardID, "");
        }

        return true;
      }
    });

    gestureDetector = new GestureDetector(context, new GestureDetector.SimpleOnGestureListener() {
      @Override
      public boolean onDown(MotionEvent event) {
        return true;
      }

      @Override
      public void onLongPress(MotionEvent event) {
        // This is also called for banner longpresses!  Need a way to differentiate the sources.
        if (subKeysList != null) {
          showSubKeys(context);
          return;
        } else if (KMManager.getGlobeKeyState() == KMManager.GlobeKeyState.GLOBE_KEY_STATE_DOWN) {
          KMManager.setGlobeKeyState(KMManager.GlobeKeyState.GLOBE_KEY_STATE_LONGPRESS);
          return;
        /* For future implementation
        else if(suggestionJSON != null) {
          showSuggestionLongpress(context);
          return;
        }*/
        }
      }

      @Override
      public boolean onSingleTapUp(MotionEvent event) {
        return false;
      }
    });
  }

  public void loadKeyboard() {
    keyboardSet = false;
    this.javascriptAfterLoad.clear();

    if(keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP)
      KMManager.InAppKeyboardLoaded = false;
    else
      KMManager.SystemKeyboardLoaded = false;

    String htmlPath = "file://" + getContext().getDir("data", Context.MODE_PRIVATE) + "/" + KMManager.KMFilename_KeyboardHtml;
    loadUrl(htmlPath);
    setBackgroundColor(0);
  }

  public void loadJavascript(String func) {
    this.javascriptAfterLoad.add(func);

    if((keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP && KMManager.InAppKeyboardLoaded) ||
      (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM && KMManager.SystemKeyboardLoaded)) {

      // If !this.keyboardSet, then pageLoaded hasn't fired yet.
      // When pageLoaded fires, it'll call `callJavascriptAfterLoad` safely.
      if(this.javascriptAfterLoad.size() == 1 && keyboardSet)
        callJavascriptAfterLoad();
    }
  }

  public void callJavascriptAfterLoad() {
    if(this.javascriptAfterLoad.size() > 0) {
      Handler handler = new Handler();
      handler.postDelayed(new Runnable() {
        @Override
        public void run() {
          if(javascriptAfterLoad.size() > 0) {
            loadUrl("javascript:" + javascriptAfterLoad.get(0));
            javascriptAfterLoad.remove(0);
            // Make sure we didn't reset the page in the middle of the queue!
            if(keyboardSet) {
              if (javascriptAfterLoad.size() > 0) {
                callJavascriptAfterLoad();
              }
            }
          }
        }
      }, 1);
    }
  }

  public void hideKeyboard() {
    String jsString = "hideKeyboard()";
    loadJavascript(jsString);
  }

  public void executeHardwareKeystroke(int code, int shift, int lstates, int eventModifiers) {
    String jsFormat = "executeHardwareKeystroke(%d,%d, %d, %d)";
    String jsString = KMString.format(jsFormat, code, shift, lstates, eventModifiers);
    loadJavascript(jsString);
  }

  @SuppressLint("ClickableViewAccessibility")
  @Override
  public boolean onTouchEvent(MotionEvent event) {
    // JH:  I'm not sure if we even USE the suggestionMenuWindow construct anywhere, but either way,
    // this if block is designed explicitly for handling the subKeysWindow.
    //
    // Come to think of it, I wonder if suggestionMenuWindow was work being done to link with
    // suggestion banner longpresses - if so, it's not yet ready for proper integration...
    // and would need its own rung in this if-else ladder.
    if (subKeysWindow != null && suggestionMenuWindow == null) {
      // Passes KMKeyboard (subclass of WebView)'s touch events off to our subkey window
      // if active, allowing for smooth, integrated gesture control.
      subKeysWindow.getContentView().findViewById(R.id.grid).dispatchTouchEvent(event);
    } else {
      //handleTouchEvent(event);
      gestureDetector.onTouchEvent(event);
      if (event.getAction() == MotionEvent.ACTION_UP) {
        subKeysList = null;
      } else if (event.getAction() == MotionEvent.ACTION_MOVE) {
        if (subKeysList != null && subKeysWindow == null) {
          // Display subkeys during move
          showSubKeys(context);
        }
      }
    }

    super.onTouchEvent(event);
    return true;
  }

  public void onResume() {
    DisplayMetrics dms = context.getResources().getDisplayMetrics();
    int kbWidth = (int) (dms.widthPixels / dms.density);
    // Ensure window is loaded for javascript functions
    loadJavascript(KMString.format(
      "window.onload = function(){ setOskWidth(\"%d\");"+
      "setOskHeight(\"0\"); };", kbWidth));
    if (ShouldShowHelpBubble) {
      ShouldShowHelpBubble = false;
      Handler handler = new Handler();
      handler.postDelayed(new Runnable() {
        @Override
        public void run() {
          loadJavascript("showHelpBubble()");
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

    RelativeLayout.LayoutParams params = KMManager.getKeyboardLayoutParams();
    this.setLayoutParams(params);

    int bannerHeight = KMManager.getBannerHeight(context);
    int oskHeight = KMManager.getKeyboardHeight(context);
    loadJavascript(KMString.format("setBannerHeight(%d)", bannerHeight));
    loadJavascript(KMString.format("setOskWidth(%d)", newConfig.screenWidthDp));
    loadJavascript(KMString.format("setOskHeight(%d)", oskHeight));

    if (dismissHelpBubble()) {
      Handler handler = new Handler();
      handler.postDelayed(new Runnable() {
        @Override
        public void run() {
          loadJavascript("showHelpBubble()");
        }
      }, 2000);
    }
  }

  public void dismissSubKeysWindow() {
    try {
      if (subKeysWindow != null && subKeysWindow.isShowing())
        subKeysWindow.dismiss();
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
    }
  }

  public void dismissSuggestionMenuWindow() {
    try {
      if (suggestionMenuWindow != null && suggestionMenuWindow.isShowing()) {
        suggestionMenuWindow.dismiss();
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
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
    return txtFont;
  }

  /**
   * Return the full path to the OSK font. Usually used for creating a Typeface font
   * @return String
   */
  public static String oskFontFilename() {
    return oskFont;
  }

  /**
   * Return the full path to the special OSK font,
   * which is with all the keyboard assets at the root app_data folder
   * @param filename
   * @return String
   */
  public String specialOSKFontFilename(String filename) {
    return context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator + filename;
  }

  public boolean setKeyboard(Keyboard k) {
    boolean retVal = false;
    if (k != null) {
      retVal = setKeyboard(
        k.getPackageID(),
        k.getKeyboardID(),
        k.getLanguageID(),
        k.getKeyboardName(),
        k.getLanguageName(),
        k.getFont(),
        k.getOSKFont(),
        k.getDisplayName());
    }

    return retVal;
  }

  public boolean setKeyboard(String packageID, String keyboardID, String languageID) {
    if (packageID == null || keyboardID == null || languageID == null)
      return false;

    boolean retVal = true;

    int index = KeyboardController.getInstance().getKeyboardIndex(packageID, languageID, keyboardID);
    Keyboard kbInfo = null;
    if (index != KeyboardController.INDEX_NOT_FOUND) {
      kbInfo = KeyboardController.getInstance().getKeyboardInfo(index);
    }

    if (!KMManager.shouldAllowSetKeyboard() || kbInfo == null) {
      sendError(packageID, keyboardID, languageID);
      kbInfo = KeyboardController.getInstance().getKeyboardInfo(0);
      retVal = false;
    } else {
      retVal = setKeyboard(kbInfo);
    }

    return retVal;
  }

  /**
   * preapre keyboard switch. The switch is executed in next reload.
   * @param packageID the package id
   * @param keyboardID the keyman keyboard id
   * @param languageID the langauge id
   * @param keyboardName the keyboard name
   * @return the result
   */
  public boolean prepareKeyboardSwitch(String packageID, String keyboardID, String languageID,  String keyboardName)
  {
    if (packageID == null || keyboardID == null || languageID == null)
      return false;

    boolean retVal = true;
    // keyboardVersion only needed for legacy cloud/ keyboards.
    // Otherwise, no need for the JSON overhead of determining the keyboard version from kmp.json
    String keyboardVersion = packageID.equals(KMManager.KMDefault_UndefinedPackageID) ?
      KMManager.getLatestKeyboardFileVersion(getContext(), packageID, keyboardID) : null;

    if (!KMManager.shouldAllowSetKeyboard() ||
        (packageID.equals(KMManager.KMDefault_UndefinedPackageID) && keyboardVersion == null)) {
      sendError(packageID, keyboardID, languageID);
      Keyboard kbInfo = KeyboardController.getInstance().getKeyboardInfo(0);
      packageID = kbInfo.getPackageID();
      keyboardID = kbInfo.getKeyboardID();
      languageID = kbInfo.getLanguageID();
      retVal = false;

      // Keyboard changed, so determine version again
      keyboardVersion = packageID.equals(KMManager.KMDefault_UndefinedPackageID) ?
        KMManager.getLatestKeyboardFileVersion(getContext(), packageID, keyboardID) : null;

    }
    String kbKey = KMString.format("%s_%s", languageID, keyboardID);

    setKeyboardRoot(packageID);

    // Escape single-quoted names for javascript call
    keyboardName = keyboardName.replaceAll("\'", "\\\\'"); // Double-escaped-backslash b/c regex.

    this.packageID = packageID;
    this.keyboardID = keyboardID;
    this.keyboardName = keyboardName;
    this.keyboardVersion = keyboardVersion;
    currentKeyboard = kbKey;
    saveCurrentKeyboardIndex();

    return retVal;
  }

  public boolean setKeyboard(String packageID, String keyboardID, String languageID,
                             String keyboardName, String languageName, String kFont,
                             String kOskFont) {
    return setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName,
                       kFont, kOskFont, null);
  }

  public boolean setKeyboard(String packageID, String keyboardID, String languageID,
                             String keyboardName, String languageName, String kFont,
                             String kOskFont, String displayName) {
    if (packageID == null || keyboardID == null || languageID == null || keyboardName == null || languageName == null) {
      return false;
    }

    // Reset the counter for showing / sending errors related to the selected keybard
    currentKeyboardErrorReports = 0;

    boolean retVal = true;
    // keyboardVersion only needed for legacy cloud/ keyboards.
    // Otherwise, no need for the JSON overhead of determining the keyboard version from kmp.json
    String keyboardVersion = packageID.equals(KMManager.KMDefault_UndefinedPackageID) ?
      KMManager.getLatestKeyboardFileVersion(getContext(), packageID, keyboardID) : null;

    if (!KMManager.shouldAllowSetKeyboard() ||
        (packageID.equals(KMManager.KMDefault_UndefinedPackageID) && keyboardVersion == null)) {
      sendError(packageID, keyboardID, languageID);
      Keyboard kbInfo = KeyboardController.getInstance().getKeyboardInfo(0);
      packageID = kbInfo.getPackageID();
      keyboardID = kbInfo.getKeyboardID();
      languageID = kbInfo.getLanguageID();
      keyboardName = kbInfo.getKeyboardName();
      languageName = kbInfo.getLanguageName();
      kFont = kbInfo.getFont();
      kOskFont = kbInfo.getOSKFont();
      retVal = false;

      // Keyboard changed, so determine version again
      keyboardVersion = packageID.equals(KMManager.KMDefault_UndefinedPackageID) ?
        KMManager.getLatestKeyboardFileVersion(getContext(), packageID, keyboardID) : null;
    }

    setKeyboardRoot(packageID);

    if(kOskFont == null || kOskFont.isEmpty())
      kOskFont = kFont;

    JSONObject jDisplayFont = makeFontPaths(kFont);
    JSONObject jOskFont = makeFontPaths(kOskFont);

    txtFont = getFontFilename(jDisplayFont);
    oskFont = getFontFilename(jOskFont);

    String kbKey = KMString.format("%s_%s", languageID, keyboardID);

    String keyboardPath = makeKeyboardPath(packageID, keyboardID, keyboardVersion);

    JSONObject reg = new JSONObject();
    try {
      reg.put("KN", keyboardName);
      reg.put("KI", "Keyboard_" + keyboardID);
      reg.put("KLC", languageID);
      reg.put("KL", languageName);
      reg.put("KF", keyboardPath);
      reg.put("KP", packageID);

      if (jDisplayFont != null) reg.put("KFont", jDisplayFont);
      if (jOskFont != null) reg.put("KOskFont", jOskFont);
      if (displayName != null) reg.put("displayName", displayName);
    } catch(JSONException e) {
      KMLog.LogException(TAG, "", e);
      return false;
    }

    String jsString = KMString.format("setKeymanLanguage(%s)", reg.toString());
    loadJavascript(jsString);

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
          loadJavascript("showHelpBubble()");
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

  // Display localized Toast notification that keyboard selection failed, so loading default keyboard.
  // Also sends a message to Sentry (not localized)
  private void sendError(String packageID, String keyboardID, String languageID) {
    this.currentKeyboardErrorReports++;

    if(this.currentKeyboardErrorReports == 1) {
      BaseActivity.makeToast(context, R.string.fatal_keyboard_error_short, Toast.LENGTH_LONG, packageID, keyboardID, languageID);
    }

    if(this.currentKeyboardErrorReports < 5) {
      // We'll only report up to 5 errors in a given keyboard to avoid spamming
      // errors and using unnecessary bandwidth doing so
      // Don't use localized string R.string.fatal_keyboard_error msg for Sentry
      String msg = KMString.format("Error in keyboard %1$s:%2$s for %3$s language.",
        packageID, keyboardID, languageID);
      Sentry.captureMessage(msg);
    }
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
    if (Sentry.isEnabled()) {
      Breadcrumb breadcrumb = new Breadcrumb();
      breadcrumb.setMessage("KMKeyboard.sendKMWError");
      breadcrumb.setCategory("KMWError");
      breadcrumb.setLevel(SentryLevel.ERROR);
      // Error info
      breadcrumb.setData("cm_lineNumber", lineNumber);
      breadcrumb.setData("cm_sourceID", sourceId);
      breadcrumb.setData("cm_message", message);

      // Keyboard info
      if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
        breadcrumb.setData("keyboardType", "INAPP");
      } else if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
        breadcrumb.setData("keyboardType", "SYSTEM");
      } else {
        breadcrumb.setData("keyboardType", "UNDEFINED");
      }

      if (this.packageID != null) {
        breadcrumb.setData("packageID", this.packageID);
      }
      if (this.keyboardID != null) {
        breadcrumb.setData("keyboardID", this.keyboardID);
      }
      if (this.keyboardName != null) {
        breadcrumb.setData("keyboardName", this.keyboardName);
      }
      if (this.keyboardVersion != null) {
        breadcrumb.setData("keyboardVersion", this.keyboardVersion);
      }
      Sentry.addBreadcrumb(breadcrumb);
      Sentry.captureMessage(message, SentryLevel.ERROR);
    }
  }

  /**
   * Extract Unicode numbers (\\u_xxxx_yyyy) from a layer to character string.
   * Ignores empty strings and layer names
   * Refer to web/source/osk/oskKey.ts
   * @param ktext Unicode string in the format \\uxxxx_yyyy
   * @return String to display on subkey
   */
  protected String convertKeyText(String ktext) {
    String title = "";
    String[] values = ktext.split("\\\\u");
    int length = values.length;
    for (int j = 0; j < length; j++) {
      if (!values[j].isEmpty() && !values[j].contains("-")) {
        // Split U_xxxx_yyyy
        String[] codePoints = values[j].split("_");
        for (String codePoint : codePoints) {
          int codePointValue = Integer.parseInt(codePoint, 16);
          if ((0x0 <= codePointValue && codePointValue <= 0x1F) || (0x80 <= codePointValue && codePointValue <= 0x9F)
              || (codePointValue > 0x10FFFF)) {
            continue;
          } else {
            title += new String(Character.toChars(codePointValue));
          }
        }
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
   * @param fontObj JSONObject - Font JSON object
   * @return String - Filename for the font. If font is invalid, return ""
   */
  private String getFontFilename(JSONObject fontObj) {
    String font = "";
    if (fontObj == null) {
      return font;
    }
    try {
      JSONArray sourceArray = fontObj.optJSONArray(KMManager.KMKey_FontFiles);
      if (sourceArray != null) {
        String fontFile;
        int length = sourceArray.length();
        for (int i = 0; i < length; i++) {
          fontFile = sourceArray.getString(i);
          if (FileUtils.hasFontExtension(fontFile)) {
            font = fontFile;
            break;
          }
        }
      } else {
        String fontFile = fontObj.optString(KMManager.KMKey_FontFiles);
        if (fontFile != null) {
          if (FileUtils.hasFontExtension(fontFile)) {
            font = fontFile;
          }
        }
      }
    } catch (JSONException e) {
      KMLog.LogException(TAG, "", e);
      font = "";
    }

    return font;
  }

  @SuppressLint("InflateParams")
  private void showSuggestionLongpress(Context context) {
    if(suggestionJSON == null || suggestionMenuWindow != null) {
      return;
    }

    // Construct from resources.
    LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    View contentView = inflater.inflate(R.layout.predictive_submenu, null, false);

    suggestionMenuWindow = new PopupWindow(contentView, 300, 120, false);
    suggestionMenuWindow.setFocusable(false);
    suggestionMenuWindow.setContentView(contentView);

    Button rotateSuggestions = contentView.findViewById(R.id.rotateSuggestionBtn);
    rotateSuggestions.setClickable(false);

    // Compute the actual display position (offset coordinate by actual screen pos of kbd)
    WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
    DisplayMetrics metrics = new DisplayMetrics();
    wm.getDefaultDisplay().getMetrics(metrics);
    float density = metrics.density;

    int posX, posY;
    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      int[] kbPos = new int[2];
      KMKeyboard.this.getLocationOnScreen(kbPos);
      posX = (int) (this.suggestionWindowPos[0] * density);
      posY = kbPos[1] + (int) (this.suggestionWindowPos[1] * density);
    } else {
      int[] kbPos = new int[2];
      KMKeyboard.this.getLocationInWindow(kbPos);
      posX = (int) (this.suggestionWindowPos[0] * density);
      posY = kbPos[1] + (int) (this.suggestionWindowPos[1] * density);
    }

    suggestionMenuWindow.setTouchable(true);
    suggestionMenuWindow.setTouchInterceptor(new View.OnTouchListener() {
      @SuppressLint("ClickableViewAccessibility")
      @Override
      public boolean onTouch(View v, MotionEvent event) {
        int action = event.getAction();
        int tx = (int) event.getRawX();
        int ty = (int) event.getRawY();

        if (action == MotionEvent.ACTION_UP) {
//          int count = ((ViewGroup) v).getChildCount();
//          for (int i = 0; i < count; i++) {
//            FrameLayout frame = (FrameLayout) ((ViewGroup) v).getChildAt(i);
//            Button button = (Button) frame.getChildAt(0);
//            if (button.isPressed()) {
//              button.performClick();
//              break;
//            }
//          }
          dismissSuggestionMenuWindow();
        } else if (action == MotionEvent.ACTION_MOVE) {
//          int count = ((ViewGroup) v).getChildCount();
//          for (int i = 0; i < count; i++) {
//            FrameLayout frame = (FrameLayout) ((ViewGroup) v).getChildAt(i);
//            Button button = (Button) frame.getChildAt(0);
//            int[] pos = new int[2];
//            button.getLocationOnScreen(pos);
//            Rect rect = new Rect();
//            button.getDrawingRect(rect);
//            rect.offset(pos[0], pos[1]);
//            if (rect.contains(tx, ty)) {
//              button.setPressed(true);
//            } else {
//              button.setPressed(false);
//            }
//          }
        }
        return false;
      }
    });

    suggestionMenuWindow.setOnDismissListener(new OnDismissListener() {
      @Override
      public void onDismiss() {
        suggestionJSON = null;
        suggestionMenuWindow = null;
        String jsString = "popupVisible(0)";
        loadJavascript(jsString);
      }
    });

    suggestionMenuWindow.showAtLocation(KMKeyboard.this, Gravity.TOP | Gravity.LEFT, posX , posY);
    String jsString = "popupVisible(1)";
    loadJavascript(jsString);

    return;
  }

  @SuppressLint({"InflateParams", "ClickableViewAccessibility"})
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

    // Calculate desired size for subkey display, # of rows/cols, etc.
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

    // Construct from resources.
    LayoutInflater inflater = (LayoutInflater) context.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    View contentView = inflater.inflate(R.layout.subkeys_popup_layout, null, false);

    // Configure the popover view with desired size and construct its popup "arrow."
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

    // Add needed subkeys to the popup view.
    GridLayout grid = (GridLayout) contentView.findViewById(R.id.grid);
    grid.setColumnCount(columns);

    for (int i = 0; i < subKeysCount; i++) {
      Button button = (Button) inflater.inflate(R.layout.subkey_layout, null);
      button.setId(i + 1);
      button.setLayoutParams(new FrameLayout.LayoutParams((int) buttonWidth, (int) buttonHeight));
      // May as well set them here, keeping them in a closure than a prone-to-change field.
      // Helps keep things from totally breaking when the event handler triggering subkey menu
      // generation and the menu's event handler stop talking to each other.
      final ArrayList<HashMap<String, String>> subkeyList = subKeysList;
      button.setOnClickListener(new OnClickListener() {
        @Override
        public void onClick(View v) {
          int index = v.getId() - 1;
          String keyId = subkeyList.get(index).get("keyId");
          String keyText = getSubkeyText(keyId, subkeyList.get(index).get("keyText"));
          String jsFormat = "executePopupKey('%s','%s')";
          String jsString = KMString.format(jsFormat, keyId, keyText);
          loadJavascript(jsString);
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
          return true;
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
          return true;
        } else if (action == MotionEvent.ACTION_DOWN) {
          // Must return true if we want the others to properly process if and when this handler
          // becomes decoupled from the keyboard's touch handler.
          return true;
        }
        return false;
      }
    });

    // Now to finalize the actual window.
    subKeysWindow = new PopupWindow(contentView, (int) pvWidth, (int) pvHeight, false);
    subKeysWindow.setTouchable(true);
    subKeysWindow.setOnDismissListener(new OnDismissListener() {
      @Override
      public void onDismiss() {
        subKeysList = null;
        subKeysWindow = null;
        String jsString = "popupVisible(0)";
        loadJavascript(jsString);
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

    // And now to actually display it.
    subKeysWindow.showAtLocation(KMKeyboard.this, Gravity.TOP | Gravity.LEFT, posX, posY);
    String jsString = "popupVisible(1)";
    loadJavascript(jsString);
  }

  // Attempt to get the subkey text.
  // If the subkey popup text is empty, parse the ID
  private String getSubkeyText(String keyID, String keyText) {
    String text = keyText;
    if (text.isEmpty()) {
      if(keyID.indexOf("U_") != -1 && keyID.indexOf("+") != -1 ) {
        // Chop off any appended '+____' portion of the key ID.
        keyID = keyID.substring(0, keyID.indexOf("+"));
      }
      text = keyID.replaceAll("U_", "\\\\u");
    }
    return text;
  }

  /**
   * Take a font JSON object and adjust to pass to JS
   * 1. Replace "source" keys for "files" keys
   * 2. Create full font paths for .ttf or .svg
   * @param font String font JSON object as a string
   * @return JSONObject of modified font information with full paths. If font is invalid, return `null`
   */
  private JSONObject makeFontPaths(String font) {

    if(font == null || font.equals("")) {
      return null;
    }

    try {
      if (FileUtils.hasFontExtension(font)) {
        JSONObject jfont = new JSONObject();
        jfont.put(KMManager.KMKey_FontFamily, font.substring(0, font.length()-4));
        JSONArray jfiles = new JSONArray();
        jfiles.put(keyboardRoot + font);
        jfont.put(KMManager.KMKey_FontFiles, jfiles);
        return jfont;
      }

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
        return fontObj;
      } else if (obj instanceof JSONArray) {
        sourceArray = fontObj.optJSONArray(KMManager.KMKey_FontFiles);
        if (sourceArray != null) {
          for (int i = 0; i < sourceArray.length(); i++) {
            fontFile = sourceArray.getString(i);
            if (FileUtils.hasFontExtension(fontFile)) {
              fontObj.put(KMManager.KMKey_FontFiles, keyboardRoot + fontFile);
              fontObj.remove(KMManager.KMKey_FontSource);
              return fontObj;
            }
          }
        }
      }
    } catch (JSONException e) {
      KMLog.LogException(TAG, "Failed to make font for '"+font+"'", e);
      return null;
    }

    return null;
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
    if (keyPreviewWindow != null) {
      keyPreviewWindow.showAtLocation(KMKeyboard.this, Gravity.TOP | Gravity.LEFT, posX, posY);
    }
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
          KMLog.LogException(TAG, "", e);
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
      List<Keyboard> keyboardsList = KMManager.getKeyboardsList(context);
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
      KMLog.LogException(TAG, "", e);
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

  public void reloadAfterError() {
    Keyboard firstKeyboard = KeyboardController.getInstance().getKeyboardInfo(0);
    if (firstKeyboard != null) {
      // Revert to first keyboard in the list
      setKeyboard(firstKeyboard);
    } else {
      // Fallback to sil_euro_latin (though 3rd party keyboards wont have it)
      setKeyboard(KMManager.getDefaultKeyboard(context));
    }
  }

  public void setSpacebarText(KMManager.SpacebarText mode) {
    String jsString = KMString.format("setSpacebarText('%s')", mode.toString());
    loadJavascript(jsString);
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