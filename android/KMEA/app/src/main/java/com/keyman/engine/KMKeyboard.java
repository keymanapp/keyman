/**
 * Copyright (C) 2017-2018 SIL International. All rights reserved.
 */

package com.keyman.engine;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.KMManager.KeyboardType;
import com.keyman.engine.KeyboardEventHandler.EventType;
import com.keyman.engine.KeyboardEventHandler.OnKeyboardEventListener;
import com.keyman.engine.util.CharSequenceUtil;
import com.keyman.engine.util.DependencyUtil;
import com.keyman.engine.util.DependencyUtil.LibraryType;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMString;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.pm.ApplicationInfo;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Handler;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.GestureDetector;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.WindowManager;
import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;
import android.view.inputmethod.InputConnection;
import android.webkit.ConsoleMessage;
import android.webkit.WebChromeClient;
import android.webkit.WebSettings;
import android.webkit.WebView;
import android.widget.Button;
import android.widget.PopupWindow;
import android.widget.PopupWindow.OnDismissListener;
import android.widget.RelativeLayout;
import android.widget.Toast;

import io.sentry.Breadcrumb;
import io.sentry.Sentry;
import io.sentry.SentryLevel;

final class KMKeyboard extends WebView {
  private static final String TAG = "KMKeyboard";
  private final Context context;
  private String packageID;
  private String keyboardID;
  private String keyboardName;
  private String keyboardVersion;

  private boolean shouldIgnoreTextChange = false;
  private boolean shouldIgnoreSelectionChange = false;

  protected KeyboardType keyboardType = KeyboardType.KEYBOARD_TYPE_UNDEFINED;
  protected ArrayList<String> javascriptAfterLoad = new ArrayList<>();

  private static String currentKeyboard = null;

  /**
   * Current banner state.
   */
  protected static KMManager.BannerType currentBanner = KMManager.BannerType.HTML;

  private static String txtFont = "";
  private static String oskFont = null;
  private static String keyboardRoot = "";
  private final String fontUndefined = "undefined";
  private GestureDetector gestureDetector;
  private static ArrayList<OnKeyboardEventListener> kbEventListeners = null;

  // Stores the current html string for use by the Banner
  // when predictive text is not active
  protected String htmlBannerString = "";

  // Facilitates a 'lazy init' - we'll only check the preference when it matters,
  // rather than at construction time.
  private Boolean _shouldShowHelpBubble = null;
  private boolean isChiral = false;

  private int currentKeyboardErrorReports = 0;

  protected boolean keyboardSet = false;
  protected boolean keyboardPickerEnabled = true;

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

  public boolean getShouldShowHelpBubble() {
    if (this._shouldShowHelpBubble == null) {
      SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
      this._shouldShowHelpBubble = prefs.getBoolean(KMManager.KMKey_ShouldShowHelpBubble, true);
    }

    return this._shouldShowHelpBubble;
  }

  public void setShouldShowHelpBubble(boolean flag) {
    this._shouldShowHelpBubble = flag;
  }

  protected boolean shouldIgnoreTextChange() {
    return shouldIgnoreTextChange;
  }

  protected void setShouldIgnoreTextChange(boolean ignore) {
    this.shouldIgnoreTextChange = ignore;
  }

  protected boolean shouldIgnoreSelectionChange() {
    return shouldIgnoreSelectionChange;
  }

  protected void setShouldIgnoreSelectionChange(boolean ignore) {
    this.shouldIgnoreSelectionChange = ignore;
  }

  protected boolean updateText(String text) {
    boolean result = false;
    JSONObject reg = new JSONObject();
    String kmText = "";
    if (text != null) {
      // Use JSON to handle passing string to Javascript
      try {
        reg.put("text", text.toString());
      } catch (JSONException e) {
        KMLog.LogException(TAG, "", e);
      }
    }

    if (KMManager.isKeyboardLoaded(this.keyboardType) && !shouldIgnoreTextChange) {
      this.loadJavascript(KMString.format("updateKMText(%s)", reg.toString()));
      result = true;
    }

    shouldIgnoreTextChange = false;
    return result;
  }

  /**
   * Updates the selection range of the current context.
   * Returns boolean - true if the selection range was updated successfully
   */
  protected boolean updateSelectionRange() {

    InputConnection ic = KMManager.getInputConnection(this.keyboardType);
    if (ic == null) {
      // Unable to get connection to the text
      return false;
    }

    ExtractedText icText = ic.getExtractedText(new ExtractedTextRequest(), 0);
    if (icText == null) {
      // Failed to get text becausee either input connection became invalid or client is taking too long to respond
      // https://developer.android.com/reference/android/view/inputmethod/InputConnection#getExtractedText(android.view.inputmethod.ExtractedTextRequest,%20int)
      return false;
    }

    String rawText = icText.text.toString();
    updateText(rawText.toString());

    int selMin = icText.selectionStart, selMax = icText.selectionEnd;

    int textLength = rawText.length();

    if (selMin < 0 || selMax < 0) {
      // There is no selection or cursor
      // Reference https://developer.android.com/reference/android/text/Selection#getSelectionEnd(java.lang.CharSequence)
      return false;
    } else if (selMin > textLength || selMax > textLength) {
      // Selection is past end of existing text -- should not be possible but we
      // are seeing it happen; #11506
      return false;
    }

    if (selMin > selMax) {
      // Selection is reversed so "swap"
      selMin = icText.selectionEnd;
      selMax = icText.selectionStart;
    }

    /*
      The values of selStart & selEnd provided by the system are in code units,
      not code-points.  We need to account for surrogate pairs here.

      Fortunately, it uses UCS-2 encoding... just like JS.

      References:
      - https://stackoverflow.com/a/23980211
      - https://android.googlesource.com/platform/frameworks/base/+/152944f/core/java/android/view/inputmethod/InputConnection.java#326
      */

    // Count the number of characters which are surrogate pairs.
    int pairsAtStart = CharSequenceUtil.countSurrogatePairs(rawText.substring(0, selMin), rawText.length());
    String selectedText = rawText.substring(selMin, selMax);
    int pairsSelected = CharSequenceUtil.countSurrogatePairs(selectedText, selectedText.length());

    selMin -= pairsAtStart;
    selMax -= (pairsAtStart + pairsSelected);
    this.loadJavascript(KMString.format("updateKMSelectionRange(%d,%d)", selMin, selMax));

    return true;
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
    getSettings().setTextZoom(100);

    getSettings().setUseWideViewPort(true);
    getSettings().setLoadWithOverviewMode(true);

    // When `.isTestMode() == true`, the setWebContentsDebuggingEnabled method is not available
    // and thus will trigger unit-test failures.
    if (!KMManager.isTestMode() && (
      (context.getApplicationInfo().flags & ApplicationInfo.FLAG_DEBUGGABLE) != 0 ||
      KMManager.getTier(null) != KMManager.Tier.STABLE
    )) {
      // Enable debugging of WebView via adb. Not used during unit tests
      // Refer: https://developer.chrome.com/docs/devtools/remote-debugging/webviews/#configure_webviews_for_debugging
      setWebContentsDebuggingEnabled(true);
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

        if (cm.messageLevel() == ConsoleMessage.MessageLevel.ERROR) {
          // Make Toast notification of error and send log about falling back to default keyboard (ignore language ID)
          // Sanitize sourceId info
          String NAVIGATION_PATTERN = "^(.*)?(keyboard\\.html#[^-]+)-.*$";
          String sourceID = cm.sourceId().replaceAll(NAVIGATION_PATTERN, "$1$2");
          sendKMWError(cm.lineNumber(), sourceID, cm.message());
          // This duplicates the sendKMWError message, which itself duplicates the reporting now
          // managed by sentry-manager on the js side in patch in #6890. It does not give us
          // additional useful information. So we don't re-send to Sentry.
          sendError(packageID, keyboardID, "", false);
        }

        return true;
      }
    });

    gestureDetector = new GestureDetector(context, new GestureDetector.SimpleOnGestureListener() {
      @Override
      public boolean onDown(MotionEvent event) {
         // https://developer.android.com/training/gestures/detector#detect-a-subset-of-supported-gestures
         // If we return false, the system assumes we want to ignore the rest of the gesture
         return true;
      }

      @Override
      public boolean onScroll(MotionEvent e1, MotionEvent e2, float distanceX, float distanceY) {
        return false;
      }

      @Override
      public void onLongPress(MotionEvent event) {
         if (KMManager.getGlobeKeyState() == KMManager.GlobeKeyState.GLOBE_KEY_STATE_DOWN) {
          KMManager.setGlobeKeyState(KMManager.GlobeKeyState.GLOBE_KEY_STATE_LONGPRESS);
          KMManager.handleGlobeKeyAction(context, true, keyboardType);
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

    if(keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      KMManager.InAppKeyboardWebViewClient.setKeyboardLoaded(false);
    } else {
      KMManager.SystemKeyboardWebViewClient.setKeyboardLoaded(false);
    }

    String htmlPath = "file://" + getContext().getDir("data", Context.MODE_PRIVATE) + "/" + KMManager.KMFilename_KeyboardHtml;
    loadUrl(htmlPath);
    setBackgroundColor(0);
  }

  public void loadJavascript(String func) {
    this.javascriptAfterLoad.add(func);

    if((keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP && KMManager.InAppKeyboardWebViewClient.getKeyboardLoaded()) ||
      (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM && KMManager.SystemKeyboardWebViewClient.getKeyboardLoaded())) {

      // If !this.keyboardSet, then pageLoaded hasn't fired yet.
      // When pageLoaded fires, it'll call `callJavascriptAfterLoad` safely.
      if(this.javascriptAfterLoad.size() == 1 && keyboardSet)
        callJavascriptAfterLoad();
    }
  }

  public void callJavascriptAfterLoad() {
    if(this.javascriptAfterLoad.size() > 0) {
      // Don't call this WebView method on just ANY thread - run it on the main UI thread.
      // https://stackoverflow.com/a/22611010
      this.postDelayed(new Runnable() {
        @Override
        public void run() {
          StringBuilder allCalls = new StringBuilder();
          if(javascriptAfterLoad.size() == 0) {
            return;
          }

          while(javascriptAfterLoad.size() > 0) {
            String entry = javascriptAfterLoad.remove(0);
            allCalls.append(entry);
            allCalls.append(";");
          }

          // Ensure strings safe for Javascript. TODO: font strings
          loadUrl("javascript:" + Uri.encode(allCalls.toString()));

          if(javascriptAfterLoad.size() > 0 && keyboardSet) {
            callJavascriptAfterLoad();
          }
        }
      }, 1);
    }
  }

  public void hideKeyboard() {

    String jsString = "hideKeyboard()";
    loadJavascript(jsString);
  }

  public void showKeyboard() {
    String jsString = "showKeyboard()";
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
    int action = event.getAction();
    // JH:  I'm not sure if we even USE the suggestionMenuWindow construct anywhere, but either way,
    // this if block is designed explicitly for handling the subKeysWindow.
    //
    // Come to think of it, I wonder if suggestionMenuWindow was work being done to link with
    // suggestion banner longpresses - if so, it's not yet ready for proper integration...
    // and would need its own rung in this if-else ladder.
    gestureDetector.onTouchEvent(event);

    if (action == MotionEvent.ACTION_UP) {
      // Cleanup popups. #6636
    }

    return super.onTouchEvent(event);
  }

  public void onResume() {
    DisplayMetrics dms = context.getResources().getDisplayMetrics();
    // Keyboard width = device width.
    int kbWidth = (int) dms.widthPixels;
    int bannerHeight = KMManager.getBannerHeight(context);
    int oskHeight = KMManager.getKeyboardHeight(context);

    this.setDimensions(kbWidth, bannerHeight, oskHeight);
    if (this.getShouldShowHelpBubble()) {
      this.showHelpBubbleAfterDelay(2000);
    }
  }

  public void onPause() {
    dismissHelpBubble();
  }

  public void onDestroy() {
    dismissHelpBubble();
  }

  @Override
  public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);

    RelativeLayout.LayoutParams params = KMManager.getKeyboardLayoutParams();
    // I suspect this is the part we should actually be calling directly...
    this.setLayoutParams(params);
    this.invalidate();
    this.requestLayout();

    this.dismissHelpBubble();

    if(this.getShouldShowHelpBubble()) {
      this.showHelpBubbleAfterDelay(2000);
    }
  }

  @Override
  public void onSizeChanged(int width, int height, int oldWidth, int oldHeight) {
    super.onSizeChanged(width, height, oldWidth, oldHeight);

    int bannerHeight = KMManager.getBannerHeight(context);
    int oskHeight = KMManager.getKeyboardHeight(context);

    if(bannerHeight + oskHeight != height) {
      // We'll proceed, but cautiously and with logging.
      KMLog.LogInfo(TAG, "Height mismatch: onSizeChanged = " + height + ", our version = " + (bannerHeight + oskHeight));
    }

    this.setDimensions(width, bannerHeight, oskHeight);
  }

  /**
   * Sets the dimensions of the WebView-hosted keyboard.
   * @param width   The keyboard's width
   * @param height  The keyboard's height.
   */
  public void setDimensions(int width, int bannerHeight, int oskHeight) {
    loadJavascript(KMString.format(
      "setDimensions({bannerHeight: %d, keyboardWidth: %d, keyboardHeight: %d})",
      bannerHeight,
      width,
      oskHeight
    ));
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

  protected void toggleSuggestionBanner(HashMap<String, String> associatedLexicalModel, boolean keyboardChanged) {
    //reset banner state if new language has no lexical model
    if (currentBanner == KMManager.BannerType.SUGGESTION
        && associatedLexicalModel == null) {
      currentBanner = KMManager.BannerType.HTML;
    }

    showBanner(true);
    // Since there's always a banner, no need to update setLayoutParams()
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
      sendError(packageID, keyboardID, languageID, true);
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
      sendError(packageID, keyboardID, languageID, true);
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
      sendError(packageID, keyboardID, languageID, true);
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

    this.dismissHelpBubble();
    if(this.getShouldShowHelpBubble()) {
      this.showHelpBubbleAfterDelay(2000);
    }

    KeyboardEventHandler.notifyListeners(kbEventListeners, keyboardType, EventType.KEYBOARD_CHANGED, currentKeyboard);

    return retVal;
  }

  public void showBanner(boolean flag) {
    String jsString = KMString.format("showBanner(%b)", flag);
    loadJavascript(jsString);
  }

  public KMManager.BannerType getBanner() {
    return currentBanner;
  }

  public void setBanner(KMManager.BannerType bannerType) {
    currentBanner = bannerType;
  }

  public String getHTMLBanner() {
    return this.htmlBannerString;
  }

  public void setHTMLBanner(String contents) {
    this.htmlBannerString = contents;
    String jsString = KMString.format("setBannerHTML(%s)",
      JSONObject.quote(this.htmlBannerString));
    loadJavascript(jsString);
  }

  public void setChirality(boolean flag) {
    this.isChiral = flag;
  }

  public boolean getChirality() {

    return this.isChiral;

  }

  // Display localized Toast notification that keyboard selection failed, so loading default keyboard.
  // Also sends a message to Sentry (not localized)
  private void sendError(String packageID, String keyboardID, String languageID, boolean reportToSentry) {
    this.currentKeyboardErrorReports++;

    if(this.currentKeyboardErrorReports == 1) {
      BaseActivity.makeToast(context, R.string.fatal_keyboard_error_short, Toast.LENGTH_LONG, packageID, keyboardID, languageID);
    }

    if(this.currentKeyboardErrorReports < 5 && DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled() && reportToSentry) {
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
    if (DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled()) {
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
      //
      // We now rely on Sentry within KMW to capture these errors
      // We'll continue to capture breadcrumbs so we can associate
      // java-side errors with javascript-side errors.
      //Sentry.captureMessage(message, SentryLevel.ERROR);
      //
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
    float density = KMManager.getWindowDensity(context);

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

  protected void showHelpBubble() {
    if(keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      return; // Help bubble is disabled for System-wide keyboard
    }

    if(KMManager.getGlobeKeyAction(keyboardType) == KMManager.GlobeKeyAction.GLOBE_KEY_ACTION_DO_NOTHING) {
      return; // Help bubble is disabled if globe key has no action
    }

    try {
      String hintText = BaseActivity.getString(this.context, R.string.help_bubble_text);

      // To ensure that the localized text is properly escaped, we'll use JSON utilities.  Since
      // there's no direct string encoder, we'll just wrap it in an object and unwrap it in JS.
      JSONObject textWrapper = new JSONObject();
      textWrapper.put("text", hintText);

      // signalHelpBubbleDismissal - defined in android-host.js, gives a helpBubbleDismissed signal.
      loadJavascript("keyman.showGlobeHint(" + textWrapper.toString() + ".text, signalHelpBubbleDismissal);");
    } catch(JSONException e) {
      KMLog.LogException(TAG, "", e);
      return;
    }
  }

  protected void showHelpBubbleAfterDelay(int milliseconds) {
    this.showHelpBubbleAfterDelay(milliseconds, false);
  }

  protected void showHelpBubbleAfterDelay(int milliseconds, boolean delayedCheck) {
    Handler handler = new Handler();
    handler.postDelayed(new Runnable() {
      @Override
      public void run() {
        if(delayedCheck) {
          if(getShouldShowHelpBubble()) {
            showHelpBubble();
          }
        } else {
          showHelpBubble();
        }
      }
    }, milliseconds);
  }

  protected void dismissHelpBubble() {
    loadJavascript("keyman.hideGlobeHint();");
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
    JSONObject reg = new JSONObject();
    if (mode != null) {
      // Use JSON to handle passing string to Javascript
      try {
        reg.put("text", mode.toString());
      } catch (JSONException e) {
        KMLog.LogException(TAG, "", e);
      }
    }

    this.loadJavascript(KMString.format("setSpacebarText(%s)", reg.toString()));
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