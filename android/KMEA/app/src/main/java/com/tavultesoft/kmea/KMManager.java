/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.Manifest;
import android.annotation.SuppressLint;
import android.app.Dialog;
import android.app.KeyguardManager;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.res.AssetManager;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.RectF;
import android.graphics.Typeface;
import android.inputmethodservice.InputMethodService;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.text.InputType;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputMethodManager;
import android.webkit.JavascriptInterface;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import androidx.core.content.ContextCompat;

import io.sentry.Breadcrumb;
import io.sentry.Sentry;
import io.sentry.SentryLevel;

import com.tavultesoft.kmea.KeyboardEventHandler.EventType;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;
import com.tavultesoft.kmea.cloud.CloudDataJsonUtil;
import com.tavultesoft.kmea.cloud.CloudDownloadMgr;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.KeyboardController;
import com.tavultesoft.kmea.logic.ResourcesUpdateTool;
import com.tavultesoft.kmea.packages.JSONUtils;
import com.tavultesoft.kmea.packages.LexicalModelPackageProcessor;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.BCP47;
import com.tavultesoft.kmea.util.CharSequenceUtil;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.KMLog;
import com.tavultesoft.kmea.util.KMString;
import com.tavultesoft.kmea.util.MapCompat;
import com.tavultesoft.kmea.util.WebViewUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public final class KMManager {

  private static final String TAG = "KMManager";

  private static ResourcesUpdateTool updateTool;

  // Keyboard types
  public enum KeyboardType {
    KEYBOARD_TYPE_UNDEFINED,
    KEYBOARD_TYPE_INAPP,
    KEYBOARD_TYPE_SYSTEM;
  }

  // Keyboard states
  public enum KeyboardState {
    KEYBOARD_STATE_UNDEFINED,
    KEYBOARD_STATE_NEEDS_DOWNLOAD,
    KEYBOARD_STATE_NEEDS_UPDATE,
    KEYBOARD_STATE_UP_TO_DATE;
  }

  // Globe key actions
  public enum GlobeKeyAction {
    GLOBE_KEY_ACTION_SHOW_MENU,
    // GLOBE_KEY_ACTION_SWITCH_TO_PREVIOUS_KEYBOARD,     // Switch to previous Keyman keyboard (reserved for flick)
    GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD,         // Switch to next Keyman keyboard
    GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD, // Advance to next system keyboard
    GLOBE_KEY_ACTION_SHOW_SYSTEM_KEYBOARDS,
    GLOBE_KEY_ACTION_DO_NOTHING,
  }

  public enum GlobeKeyState {
    GLOBE_KEY_STATE_UP,
    GLOBE_KEY_STATE_DOWN,
    GLOBE_KEY_STATE_LONGPRESS
  }

  public enum FormFactor {
    PHONE,
    TABLET;
  }

  public enum Tier {
    ALPHA,
    BETA,
    STABLE
  }

  public enum SpacebarText {
    LANGUAGE,
    KEYBOARD,
    LANGUAGE_KEYBOARD,
    BLANK;

    // Maps to enum SpacebarText in kmwbase.ts
    public static SpacebarText fromString(String mode) {
      if(mode == null) return LANGUAGE_KEYBOARD;
      switch(mode) {
        case "language": return LANGUAGE;
        case "keyboard": return KEYBOARD;
        case "languageKeyboard": return LANGUAGE_KEYBOARD;
        case "blank": return BLANK;
      }
      return LANGUAGE_KEYBOARD;
    }

    public String toString()  {
      String modes[] = { "language", "keyboard", "languageKeyboard", "blank" };
      return modes[this.ordinal()];
    }
  };

  private static InputMethodService IMService;
  private static boolean debugMode = false;
  private static boolean shouldAllowSetKeyboard = true;
  private static boolean didCopyAssets = false;

  private static boolean didLogHardwareKeystrokeException = false;

  private static GlobeKeyAction inappKbGlobeKeyAction = GlobeKeyAction.GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD;
  private static GlobeKeyAction sysKbGlobeKeyAction = GlobeKeyAction.GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD;
  // This is used to keep track of the starting system keyboard index while the screen is locked
  private static int startingKeyboardIndexOnLockScreen = -1;

  // This is used to keep track of the globe key shortpress and longpress
  private static GlobeKeyState globeKeyState = GlobeKeyState.GLOBE_KEY_STATE_UP;

  private static KMManager.SpacebarText spacebarText = KMManager.SpacebarText.LANGUAGE_KEYBOARD; // must match default given in kmwbase.ts

  protected static boolean InAppKeyboardLoaded = false;
  protected static boolean SystemKeyboardLoaded = false;
  protected static boolean InAppKeyboardShouldIgnoreTextChange = false;
  protected static boolean InAppKeyboardShouldIgnoreSelectionChange = false;
  protected static boolean SystemKeyboardShouldIgnoreTextChange = false;
  protected static boolean SystemKeyboardShouldIgnoreSelectionChange = false;
  protected static KMKeyboard InAppKeyboard = null;
  protected static KMKeyboard SystemKeyboard = null;
  protected static HashMap<String, String> currentLexicalModel = null;
  protected static WebViewUtils.EngineWebViewVersionStatus engineWebViewVersionStatus =
    WebViewUtils.EngineWebViewVersionStatus.UNDETERMINED;

  public final static String predictionPrefSuffix = ".mayPredict";
  public final static String correctionPrefSuffix = ".mayCorrect";

  /**
   * Banner state value: "blank" - no banner available.
   */
  protected static final String KM_BANNER_STATE_BLANK = "blank";
  /**
   * Banner state value: "suggestion" - dictionary suggestions are shown.
   */
  protected static final String KM_BANNER_STATE_SUGGESTION = "suggestion";

  //TODO: should be part of kmkeyboard
  /**
   * Current banner state.
   */
  protected static String currentBanner = KM_BANNER_STATE_BLANK;


  // Special override for when keyboard is entering a password text field.
  // When mayPredictOverride is true, the option {'mayPredict' = false} is set in the lm-layer
  // regardless what the Settings preference is.
  private static boolean mayPredictOverride = false;

  // Boolean for whether a keyboard can send embedded KMW crash reports to Sentry
  // When maySendCrashReport is false, KMW will still attempt to send crash reports, but it
  // will be blocked.
  private static boolean maySendCrashReport = true;

  // Keyman public keys
  public static final String KMKey_ID = "id";
  public static final String KMKey_Name = "name";
  public static final String KMKey_Version = "version";
  public static final String KMKey_PackageID = "packageId";
  public static final String KMKey_LanguageID = "langId";
  public static final String KMKey_LanguageName = "langName";
  public static final String KMKey_KeyboardCount = "kbCount";
  public static final String KMKey_Icon = "icon";
  public static final String KMKey_Keyboard = "keyboard";
  public static final String KMKey_KeyboardID = "kbId";
  public static final String KMKey_KeyboardName = "kbName";
  public static final String KMKey_KeyboardVersion = "version";
  public static final String KMKey_Font = "font";
  public static final String KMKey_DisplayFont = "displayFont";
  public static final String KMKey_OskFont = "oskFont";
  public static final String KMKey_FontSource = "source";
  public static final String KMKey_FontFiles = "files";
  public static final String KMKey_FontFamily = "family";
  public static final String KMKey_KeyboardModified = "lastModified";
  public static final String KMKey_KeyboardRTL = "rtl";
  public static final String KMKey_KeyboardHeightPortrait = "keyboardHeightPortrait";
  public static final String KMKey_KeyboardHeightLandscape = "keyboardHeightLandscape";

  public static final String KMKey_CustomHelpLink = "CustomHelpLink";
  public static final String KMKey_KMPLink = "kmp";
  public static final String KMKey_UserKeyboardIndex = "UserKeyboardIndex";
  public static final String KMKey_DisplayKeyboardSwitcher = "DisplayKeyboardSwitcher";
  public static final String KMKey_LexicalModel = "lm";
  public static final String KMKey_LexicalModelID = "lmId";
  public static final String KMKey_LexicalModelName = "lmName";
  public static final String KMKey_LexicalModelVersion = "lmVersion";
  public static final String KMKey_LexicalModelPackageFilename = "kmpPackageFilename";

  // DEPRECATED keys
  public static final String KMKey_CustomKeyboard = "CustomKeyboard";
  public static final String KMKey_CustomModel = "CustomModel";
  public static final String KMKey_HelpLink = "helpLink";

  // Keyman internal keys
  protected static final String KMKey_ShouldShowHelpBubble = "ShouldShowHelpBubble";

  // Default Legacy Asset Paths
  // Previous keyboards installed from the cloud  went to /languages/ and /fonts/
  // Keyboards that have an undefined packageID were installed into /cloud/
  public static final String KMDefault_LegacyAssetLanguages = "languages";
  public static final String KMDefault_LegacyAssetFonts = "fonts";
  public static final String KMDefault_UndefinedPackageID = "cloud";

  // Keyboards now get installed into /packages/packageID/
  public static final String KMDefault_AssetPackages = "packages";
  public static final String KMDefault_LexicalModelPackages = "models";

  // Default Keyboard Info
  public static final String KMDefault_PackageID = "sil_euro_latin";
  public static final String KMDefault_KeyboardID = "sil_euro_latin";
  public static final String KMDefault_LanguageID = "en";
  public static final String KMDefault_KeyboardName = "EuroLatin (SIL) Keyboard";
  public static final String KMDefault_LanguageName = "English";
  public static final String KMDefault_KeyboardFont = "DejaVuSans.ttf";
  public static final String KMDefault_KeyboardKMP = KMDefault_PackageID + FileUtils.KEYMANPACKAGE;

  // Default Dictionary Info
  public static final String KMDefault_DictionaryPackageID = "nrc.en.mtnt";
  public static final String KMDefault_DictionaryModelID = "nrc.en.mtnt";
  public static final String KMDefault_DictionaryModelName = "English dictionary (MTNT)";
  public static final String KMDefault_DictionaryVersion = "0.1.4";
  public static final String KMDefault_DictionaryKMP = KMDefault_DictionaryPackageID + FileUtils.MODELPACKAGE;

  // Keyman files
  protected static final String KMFilename_KeyboardHtml = "keyboard.html";
  protected static final String KMFilename_JSEngine = "keymanandroid.js";
  protected static final String KMFilename_JSEngine_Sourcemap = "keyman.js.map";
  protected static final String KMFilename_JSSentry = "keyman-sentry.js";
  protected static final String KMFilename_KmwCss = "kmwosk.css";
  protected static final String KMFilename_Osk_Ttf_Font = "keymanweb-osk.ttf";

  // Deprecated by KeyboardController.KMFilename_Installed_KeyboardsList
  public static final String KMFilename_KeyboardsList = "keyboards_list.dat";

  public static final String KMFilename_LexicalModelsList = "lexical_models_list.dat";

  private static Context appContext;

  public static String getResourceRoot() {
    return appContext.getDir("data", Context.MODE_PRIVATE).toString() + File.separator;
  }

  public static String getPackagesDir() {
    return getResourceRoot() + KMDefault_AssetPackages + File.separator;
  }

  public static String getLexicalModelsDir() {
    return getResourceRoot() + KMDefault_LexicalModelPackages + File.separator;
  }

  public static String getCloudDir() {
    return getResourceRoot() + KMDefault_UndefinedPackageID + File.separator;
  }

  public static FormFactor getFormFactor() {
    String device_type = appContext.getResources().getString(R.string.device_type);

    return device_type.equals("AndroidMobile") ? FormFactor.PHONE : FormFactor.TABLET;
  }

  /**
   * Extract KMEA tier from versionName. Uses parameter so we can unit test.
   * @param versionName String - If not provided, determine tier from
   *                    com.tavultesoft.kmea.BuildConfig.KEYMAN_ENGINE_VERSION_NAME
   * @return Tier (ALPHA, BETA, STABLE)
   */
  public static Tier getTier(String versionName) {
    if (versionName == null || versionName.isEmpty()) {
      versionName = com.tavultesoft.kmea.BuildConfig.KEYMAN_ENGINE_VERSION_NAME;
    }
    Pattern pattern = Pattern.compile("^(\\d+\\.\\d+\\.\\d+)-(alpha|beta|stable).*");
    Matcher matcher = pattern.matcher(versionName);
    if (matcher.matches() && matcher.groupCount() >= 2) {
      switch (matcher.group(2)) {
        case "alpha": return Tier.ALPHA;
        case "beta": return Tier.BETA;
        default:
          return Tier.STABLE;
      }
    }
    return Tier.STABLE;
  }

  /**
   * Extract KMEA major version #.# from KEYMAN_ENGINE_VERSION_NAME
   * @return String
   */
  public static String getMajorVersion() {
    // Regex needs to match the entire string
    String appVersion = com.tavultesoft.kmea.BuildConfig.KEYMAN_ENGINE_VERSION_NAME;
    Pattern pattern = Pattern.compile("^(\\d+\\.\\d+)\\.\\d+.*");
    Matcher matcher = pattern.matcher(appVersion);
    if (matcher.matches() && matcher.groupCount() >= 1) {
      appVersion = matcher.group(1);
    }

    return appVersion;
  }

  /**
   * Extract KMEA version #.#.# from KEYMAN_ENGINE_VERSION_NAME
   * @return String
   */
  public static String getVersion() {
    // Regex needs to match the entire string
    String appVersion = com.tavultesoft.kmea.BuildConfig.KEYMAN_ENGINE_VERSION_NAME;
    Pattern pattern = Pattern.compile("^(\\d+\\.\\d+\\.\\d+).*");
    Matcher matcher = pattern.matcher(appVersion);
    if (matcher.matches() && matcher.groupCount() >= 1) {
      appVersion = matcher.group(1);
    }

    return appVersion;
  }

  /**
   * Get the Keyman Engine mode.
   * @return WebViewUtils.EngineWebViewVersionStatus
   */
  public static WebViewUtils.EngineWebViewVersionStatus getEngineWebViewVersionStatus() {
    return engineWebViewVersionStatus;
  }

  /**
   * Set the Keyman Engine mode based on the Chrome version
   * @param aContext
   * @param webView - If provided, the Chrome version of the WebView is determined
   */
  public static void setEngineWebViewVersionStatus(Context aContext, WebView webView) {
    engineWebViewVersionStatus = WebViewUtils.getEngineWebViewVersionStatus(aContext, webView, "");
  }

  // Check if a keyboard namespace is reserved
  public static boolean isReservedNamespace(String packageID) {
    if (packageID.equals(KMDefault_UndefinedPackageID)) {
      return true;
    }
    return false;
  }

  public static void initialize(final Context context, KeyboardType keyboardType) {
    appContext = context.getApplicationContext();

    if (!didCopyAssets || isTestMode()) {
      // Copy and install assets
      copyAssets(appContext);

      migrateOldKeyboardFiles(appContext);
      // UpdateOldKeyboardsList() handled with KeyboardController later
      didCopyAssets = true;
    }

    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      initInAppKeyboard(appContext);
    } else if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      initSystemKeyboard(appContext);
    } else {
      String msg = "Cannot initialize: Invalid keyboard type";
      KMLog.LogError(TAG, msg);
    }

    JSONUtils.initialize(new File(getPackagesDir()));

    KeyboardController.getInstance().initialize(appContext);
    migrateCloudKeyboards(appContext);

    CloudDownloadMgr.getInstance().initialize(appContext);
  }

  public static void executeResourceUpdate(Context aContext)
  {
    getUpdateTool().checkForResourceUpdates(aContext,true);
  }

  public static ResourcesUpdateTool getUpdateTool() {
    if(updateTool==null) {
      updateTool = new ResourcesUpdateTool();
      updateTool.createNotificationChannel(appContext);

      KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(updateTool);

    }
    return updateTool;
  }

  public static void setInputMethodService(InputMethodService service) {
    IMService = service;
  }
  public static InputMethodService getInputMethodService() { return IMService; }

  public static boolean executeHardwareKeystroke(int code, int shift, int lstates, int eventModifiers) {
    if (SystemKeyboard != null) {
      return executeHardwareKeystroke(code, shift, KeyboardType.KEYBOARD_TYPE_SYSTEM, lstates, eventModifiers);
    } else if (InAppKeyboard != null) {
      return executeHardwareKeystroke(code, shift, KeyboardType.KEYBOARD_TYPE_INAPP, lstates, eventModifiers);
    }

    return false;
  }

  public static boolean executeHardwareKeystroke(
    int code, int shift, KeyboardType keyboard, int lstates, int eventModifiers) {
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP && InAppKeyboard != null) {
      InAppKeyboard.executeHardwareKeystroke(code, shift, lstates, eventModifiers);
      return true;
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM && SystemKeyboard != null) {
      SystemKeyboard.executeHardwareKeystroke(code, shift, lstates, eventModifiers);
      return true;
    } else if (!didLogHardwareKeystrokeException) {
      KMLog.LogError(TAG, "executeHardwareKeystroke: " + keyboard.toString() + " keyboard is null");
      didLogHardwareKeystrokeException = true;
    }

    return false;
  }

  /**
   * Handle the globe key action on the lock screen
   * @param context
   */
  private static void doGlobeKeyLockscreenAction(Context context) {
    // Override globe key action if screen is locked:
    // 1. Switch to next Keyman keyboard (no menu)
    // 2. When all the Keyman keyboards have been cycled through, advance to the next system keyboard
    if (startingKeyboardIndexOnLockScreen == getCurrentKeyboardIndex(context) ||
        KeyboardController.getInstance().get().size() == 1) {
      // all the Keyman keyboards have been cycled through
      advanceToNextInputMode();
    } else {
      if (startingKeyboardIndexOnLockScreen == -1) {
        // Initialize the system keyboard starting index while the screen is locked
        startingKeyboardIndexOnLockScreen = getCurrentKeyboardIndex(context);
      }
      switchToNextKeyboard(context);
    }
  }

  /**
   * Handle the globe key longpress action.
   * @param context
   * @param keyboard
   */
  private static void doGlobeKeyLongpressAction(Context context, KeyboardType keyboard) {
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard == null || !InAppKeyboard.keyboardPickerEnabled) {
        return;
      }
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard == null || !SystemKeyboard.keyboardPickerEnabled) {
        return;
      }
    } else {
      // assertion failure?
      KMLog.LogError(TAG, "doGlobeKeyLongpressAction with keyboard type " + keyboard);
      return;
    }

    showKeyboardPicker(context, keyboard);
  }

  /**
   * Handle the globe key shortpress action
   * @param context
   * @param keyboard KeyboardType of KEYBOARD_TYPE_INAPP or KEYBOARD_TYPE_SYSTEM
   */
  private static void doGlobeKeyShortpressAction(Context context, KeyboardType keyboard) {
    GlobeKeyAction action = GlobeKeyAction.GLOBE_KEY_ACTION_DO_NOTHING;
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard == null || !InAppKeyboard.keyboardPickerEnabled) {
        return;
      }
      action = inappKbGlobeKeyAction;
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard == null || !SystemKeyboard.keyboardPickerEnabled) {
        return;
      }
      action = sysKbGlobeKeyAction;
    } else {
      // assertion failure?
      KMLog.LogError(TAG, "doGlobeKeyShortpressAction with keyboard type " + keyboard);
      return;
    }

    if (action == GlobeKeyAction.GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD &&
      KeyboardController.getInstance().get().size() == 1) {
      // Override when keyboard switch and only 1 keyboard installed ==>show menu
      action = GlobeKeyAction.GLOBE_KEY_ACTION_SHOW_MENU;
    }

    switch (action) {
      case GLOBE_KEY_ACTION_SHOW_MENU:
        showKeyboardPicker(context, keyboard);
        break;
      case GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD:
        switchToNextKeyboard(context);
        break;
      case GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD:
        // Only do this for system keyboard
        if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
          advanceToNextInputMode();
        }
        break;
      case GLOBE_KEY_ACTION_SHOW_SYSTEM_KEYBOARDS:
        // Only do this for system keyboard
        if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
          InputMethodManager imm = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
          imm.showInputMethodPicker();
        }
        break;
      default:
        // Do nothing
    }
  }

  /**
   * Adjust the keyboard dimensions. If the suggestion banner is active, use the
   * combined banner height and keyboard height
   * @return RelativeLayout.LayoutParams
   */
  public static RelativeLayout.LayoutParams getKeyboardLayoutParams() {
    int bannerHeight = getBannerHeight(appContext);
    int kbHeight = getKeyboardHeight(appContext);
    RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(
      RelativeLayout.LayoutParams.MATCH_PARENT, bannerHeight + kbHeight);
    params.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.TRUE);
   return params;
  }

  private static void initInAppKeyboard(Context appContext) {
    if (InAppKeyboard == null) {
      InAppKeyboard = new KMKeyboard(appContext, KeyboardType.KEYBOARD_TYPE_INAPP);
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      InAppKeyboard.setLayoutParams(params);
      InAppKeyboard.setVerticalScrollBarEnabled(false);
      InAppKeyboard.setHorizontalScrollBarEnabled(false);
      InAppKeyboard.setWebViewClient(new KMInAppKeyboardWebViewClient(appContext));
      InAppKeyboard.addJavascriptInterface(new KMInAppKeyboardJSHandler(appContext, InAppKeyboard), "jsInterface");
      InAppKeyboard.loadKeyboard();

      setEngineWebViewVersionStatus(appContext, InAppKeyboard);
    }
  }

  private static void initSystemKeyboard(Context appContext) {
    if (SystemKeyboard == null) {
      SystemKeyboard = new KMKeyboard(appContext, KeyboardType.KEYBOARD_TYPE_SYSTEM);
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      SystemKeyboard.setLayoutParams(params);
      SystemKeyboard.setVerticalScrollBarEnabled(false);
      SystemKeyboard.setHorizontalScrollBarEnabled(false);
      SystemKeyboard.setWebViewClient(new KMSystemKeyboardWebViewClient(appContext));
      SystemKeyboard.addJavascriptInterface(new KMSystemKeyboardJSHandler(appContext, SystemKeyboard), "jsInterface");
      SystemKeyboard.loadKeyboard();

      setEngineWebViewVersionStatus(appContext, SystemKeyboard);
    }
  }

  public static String getLanguagePredictionPreferenceKey(String langID) {
    return langID + predictionPrefSuffix;
  }

  public static String getLanguageCorrectionPreferenceKey(String langID) {
    return langID + correctionPrefSuffix;
  }

  public static void hideSystemKeyboard() {
    if (SystemKeyboard != null) {
      SystemKeyboard.hideKeyboard();
    }
  }

  public static boolean isKeyboardLoaded(KeyboardType type) {
    if (type == KeyboardType.KEYBOARD_TYPE_INAPP) {
      return InAppKeyboardLoaded;
    } else if (type == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      return SystemKeyboardLoaded;
    } else {
      String msg = "Keyboard type undefined";
      KMLog.LogError(TAG, msg);
      return false;
    }
  }

  @SuppressLint("InflateParams")
  public static View createInputView(InputMethodService inputMethodService) {
    //final Context context = appContext;
    IMService = inputMethodService;
    Context appContext = IMService.getApplicationContext();
    final FrameLayout mainLayout = new FrameLayout(appContext);
    mainLayout.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT));

    RelativeLayout keyboardLayout = new RelativeLayout(appContext);
    keyboardLayout.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT));
    ViewGroup parent = (ViewGroup) SystemKeyboard.getParent();
    if (parent != null)
      parent.removeView(SystemKeyboard);
    keyboardLayout.addView(SystemKeyboard);

    mainLayout.addView(keyboardLayout);
    //mainLayout.addView(overlayLayout);
    return mainLayout;
  }

  public static void onStartInput(EditorInfo attribute, boolean restarting) {
    if (!restarting && SystemKeyboard != null) {
      String packageName = attribute.packageName;
      int inputType = attribute.inputType;
      if (packageName.equals("android") && inputType == (InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD)) {
        SystemKeyboard.keyboardPickerEnabled = false;
      } else {
        SystemKeyboard.keyboardPickerEnabled = true;
      }
    }
  }

  public static void onResume() {
    if (InAppKeyboard != null) {
      InAppKeyboard.resumeTimers();
      InAppKeyboard.onResume();
    }
    if (SystemKeyboard != null) {
      SystemKeyboard.resumeTimers();
      SystemKeyboard.onResume();
    }
  }

  public static void onPause() {
    if (InAppKeyboard != null) {
      InAppKeyboard.onPause();
    }
    if (SystemKeyboard != null) {
      SystemKeyboard.onPause();
    }

    // Lock screen triggers onPause, so clear globeKeyState
    globeKeyState = GlobeKeyState.GLOBE_KEY_STATE_UP;
  }

  public static void onDestroy() {
    if (InAppKeyboard != null) {
      InAppKeyboard.onDestroy();
    }
    if (SystemKeyboard != null) {
      SystemKeyboard.onDestroy();
    }
    updateTool.destroyNotificationChannel(appContext);
    CloudDownloadMgr.getInstance().shutdown(appContext);
  }

  public static void onConfigurationChanged(Configuration newConfig) {
    // KMKeyboard
    if (InAppKeyboard != null) {
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      InAppKeyboard.setLayoutParams(params);
      InAppKeyboard.onConfigurationChanged(newConfig);
    }
    if (SystemKeyboard != null) {
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      SystemKeyboard.setLayoutParams(params);
      SystemKeyboard.onConfigurationChanged(newConfig);
    }
  }

  /**
   * Query the AndroidManifest file to see if a permission is granted.
   * @param permission - The manifest permission to query
   * @return boolean - true if the manifest permission is granted
    */
  protected static boolean hasPermission(Context context, String permission) {
    // API to check permission depends on Android SDK level
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
      return (context.checkSelfPermission(permission) == PackageManager.PERMISSION_GRANTED);
    }

    return ContextCompat.checkSelfPermission(context, permission) == PackageManager.PERMISSION_GRANTED;
  }

  /**
   * Check if KMManager has an active network connection.
   * Requires Manifest.permission.ACCESS_NETWORK_STATE to be granted
   * @param context - The context
   * @return boolean - true if manifest permission ACCESS_NETWORK_STATE is granted and the device
   * has an active network connection
   */
  @SuppressLint("MissingPermission")
  public static boolean hasConnection(Context context) {
    if (hasPermission(context, Manifest.permission.ACCESS_NETWORK_STATE)) {
      ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
      NetworkInfo networkInfo = cm.getActiveNetworkInfo();
      return networkInfo != null && networkInfo.isConnectedOrConnecting();
    }

    return false;
  }

  public static boolean hasInternetPermission(Context context) {
    return hasPermission(context, Manifest.permission.INTERNET);
  }

  private static void copyAssets(Context context) {
    AssetManager assetManager = context.getAssets();
    try {
      // Copy KMW files
      copyAsset(context, KMFilename_KeyboardHtml, "", true);
      copyAsset(context, KMFilename_JSEngine, "", true);
      copyAsset(context, KMFilename_JSSentry, "", true);
      if(KMManager.isDebugMode()) {
        copyAsset(context, KMFilename_JSEngine_Sourcemap, "", true);
      }
      copyAsset(context, KMFilename_KmwCss, "", true);
      copyAsset(context, KMFilename_Osk_Ttf_Font, "", true);

      // Copy default keyboard font
      copyAsset(context, KMDefault_KeyboardFont, "", true);

      // Keyboard packages directory
      File packagesDir = new File(getPackagesDir());
      if (!packagesDir.exists()) {
        packagesDir.mkdir();
      }

      /*
      // default cloud keyboard
      // Intend to deprecate cloud/keyboards in Keyman 15.0
      */
      File cloudDir = new File(getCloudDir());
      if (!cloudDir.exists()) {
        cloudDir.mkdir();
      }
      String[] keyboardFiles = assetManager.list(KMDefault_UndefinedPackageID);
      for (String keyboardFile : keyboardFiles) {
        copyAsset(context, keyboardFile, KMDefault_UndefinedPackageID, true);
      }

      // Copy lexical model directory and subfolders
      File lexicalModelsDir = new File(getLexicalModelsDir());
      if (!lexicalModelsDir.exists()) {
        lexicalModelsDir.mkdir();
      }

      String[] modelNames = assetManager.list(KMDefault_LexicalModelPackages);
      for (String modelName : modelNames) {
        File lexicalModelDir = new File(lexicalModelsDir, modelName);
        if (!lexicalModelDir.exists()) {
          lexicalModelDir.mkdir();
        }
        String[] modelFiles = assetManager.list(KMDefault_LexicalModelPackages + File.separator + modelName);
        for (String modelFile : modelFiles) {
          copyAsset(context, modelFile, KMDefault_LexicalModelPackages + File.separator + modelName, true);
        }
      }

      File resourceRoot = new File(getResourceRoot());
      PackageProcessor kmpProcessor = new PackageProcessor(resourceRoot);
      LexicalModelPackageProcessor lmkmpProcessor = new LexicalModelPackageProcessor(resourceRoot);
      String assetFiles[] = assetManager.list("");
      for (String assetFile : assetFiles) {
        File kmpFile, tempPackagePath;
        boolean isPackageFile = FileUtils.hasKeymanPackageExtension(assetFile) ||
          FileUtils.hasLexicalModelPackageExtension(assetFile);

        // Copy asset KMP files
        if (isPackageFile) {
          kmpFile = new File(getResourceRoot(), assetFile);
          if (kmpFile.exists()) {
            // Skip if kmp file already exists.
            // Admittedly, we'll miss out on kmp updates with Keyman upgrades
            continue;
          }
          copyAsset(context, assetFile, "", true);
          tempPackagePath = kmpProcessor.unzipKMP(kmpFile);

          // Determine package info
          JSONObject pkgInfo = kmpProcessor.loadPackageInfo(tempPackagePath);
          if (pkgInfo == null) {
            KMLog.LogError(TAG, "Invalid kmp.json in default asset " + assetFile);
          }
          String assetPackageID = kmpProcessor.getPackageID(kmpFile);
          String assetPackageTarget = kmpProcessor.getPackageTarget(pkgInfo);

          // Only install if asset package is not a downgrade
          try {
            if (assetPackageTarget.equals(PackageProcessor.PP_TARGET_KEYBOARDS)) {
              if (!kmpProcessor.isDowngrade(kmpFile, true)) {
                // Not using the list of entries returned from processKMP()
                // because the main App will add the keyboard/lexical model info
                kmpProcessor.processKMP(kmpFile, tempPackagePath, PackageProcessor.PP_KEYBOARDS_KEY);
              }
            } else if (assetPackageTarget.equals(PackageProcessor.PP_TARGET_LEXICAL_MODELS)) {
              if (!lmkmpProcessor.isDowngrade(kmpFile)) {
                lmkmpProcessor.processKMP(kmpFile, tempPackagePath, PackageProcessor.PP_LEXICAL_MODELS_KEY);
              }
            }
          } catch (JSONException e) {
            KMLog.LogException(TAG, "Unable to determine isDowngrade for " + kmpFile.toString(), e);
          }

          // Cleanup tempPackagePath
          if (tempPackagePath.exists()) {
            FileUtils.deleteDirectory(tempPackagePath);
          }
        }
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "Failed to copy assets. Error: ", e);
    }
  }

  private static int copyAsset(Context context, String filename, String directory, boolean overwrite) {
    int result;
    AssetManager assetManager = context.getAssets();
    try {
      if (directory == null)
        directory = "";

      directory = directory.trim();

      String dirPath;
      if (directory.length() != 0) {
        directory = directory + File.separator;
        dirPath = getResourceRoot() + directory;
      } else {
        dirPath = getResourceRoot();
      }

      File file = new File(dirPath, filename);
      if (!file.exists() || overwrite) {
        InputStream inputStream = assetManager.open(directory + filename);
        FileOutputStream outputStream = new FileOutputStream(file);
        FileUtils.copy(inputStream, outputStream);

        result = 1;
      } else {
        result = 0;
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "Failed to copy asset. Error: ", e);
      result = -1;
    }
    return result;
  }

  /**
   * Migrates legacy keyboards in the /languages/ and /fonts/ directories so that:
   * 1) Default eurolatin2 or us.js keyboards are deleted
   * 2) Keyboards missing a version number are assigned "-1.0"
   * 3) Remaining keyboards are moved to /cloud/ (default packageID of "cloud")
   * 4) Remove legacy /languages/ and /fonts/ directories
   *
   * Fonts used in legacy keyboards are also migrated from /fonts/ to /cloud/
   *
   * At some point, we might migrate /cloud/ keyboards into packages
   * 5) For now, delete cloud/sil_euro_latin*.js
   *
   * Assumption: No legacy keyboards exist in /packages/*.js
   * @param context
   */
  private static void migrateOldKeyboardFiles(Context context) {
    String legacyLanguagesPath = getResourceRoot() + KMDefault_LegacyAssetLanguages + File.separator;
    String legacyFontsPath = getResourceRoot() + KMDefault_LegacyAssetFonts + File.separator;
    File legacyLanguagesDir = new File(legacyLanguagesPath);
    File legacyFontsDir = new File(legacyFontsPath);
    File migratedDir = new File(getCloudDir());
    if (!legacyLanguagesDir.exists() && !legacyFontsDir.exists() && !migratedDir.exists()) {
      return;
    }

    if (!migratedDir.exists()) {
      migratedDir.mkdir();
    }

    FileFilter keyboardFilter = new FileFilter() {
      @Override
      public boolean accept(File pathname) {
        if (pathname.isFile() && FileUtils.hasJavaScriptExtension(pathname.getName())) {
          return true;
        }
        return false;
      }
    };

    try {
      if (legacyLanguagesDir.exists()) {


        File[] files = legacyLanguagesDir.listFiles(keyboardFilter);
        for (File file : files) {
          String filename = file.getName();

          // Keyboards can have filenames "keyboardID.js" or "keyboardID-version.js"
          boolean versionNumberMissing = filename.lastIndexOf("-") < 0;
          String keyboardID = (versionNumberMissing) ? filename.substring(0, filename.length() - 4) :
            filename.substring(0, filename.indexOf("-"));

          // Handle keyboards missing version number
          if (versionNumberMissing) {
            if (filename.equals("us.js")) {
              file.delete();
            } else {
              // Append default version number to keyboard and move
              filename = filename.substring(0, filename.lastIndexOf(".js")) + "-1.0.js";
              File migratedFile = new File(migratedDir, filename);
              if (!migratedFile.exists()) {
                file.renameTo(new File(migratedDir, filename));
              }
            }
            // Handle keyboards with version number
          } else {
            // Remove legacy default keyboards
            if (filename.startsWith(KMDefault_KeyboardID + "-")) {
              file.delete();
              continue;
            }

            // Migrate /languages/*.js keyboards
            File migratedFile = new File(migratedDir, filename);
            if (!migratedFile.exists()) {
              file.renameTo(new File(migratedDir, filename));
            }
          }

        }

        FileUtils.deleteDirectory(legacyLanguagesDir);
      }

      // Migrate legacy fonts
      if (legacyFontsDir.exists()) {
        FileUtils.copyDirectory(legacyFontsDir, migratedDir);
        FileUtils.deleteDirectory(legacyFontsDir);
      }

      // cloud/ keyboards migrated to packages/ in migrateCloudKeyboards()
      // For now, can delete sil_euro_latin because it will be in packages/
      removeCloudKeyboard(KMDefault_KeyboardID);

    } catch (IOException e) {
      KMLog.LogException(TAG, "Failed to migrate assets. Error: ", e);
    }
  }

  /**
   * Migrate keyboards list from the legacy dat file (HashMap) and returns a list of keyboards.
   * 1. These deprecated keyboards: "us", "european", "european2" are also removed
   * and replaced with "sil_euro_latin".
   * 2. Undefined package ID set to "cloud"
   * 3. Undefined version set to the latest file version
   * 4. Check sil_euro_latin only added once
   * Using the old keyboards list as parameter instead of keyboards file so this can be
   * unit test.
   * @param context Context
   * @param dat_list ArrayList<HashMap<String, String>> Old keyboards list
   * @return List<Keyboard> Migrated keyboards list
   */
  public static List<Keyboard> updateOldKeyboardsList(Context context, ArrayList<HashMap<String, String>> dat_list) {
    List<Keyboard> list = new ArrayList<Keyboard>();
    if (dat_list == null) {
      return list;
    }

    boolean defaultKeyboardInstalled = false;
    for(HashMap<String, String> kbdMap : dat_list) {
      final boolean isNewKeyboard = false;
      String packageID = MapCompat.getOrDefault(kbdMap, KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
      String keyboardID = kbdMap.get(KMManager.KMKey_KeyboardID);
      if (keyboardID == null) {
        KMLog.LogError(TAG, "updateOldKeyboardsList: skipping keyboard with undefined keyboard ID");
        continue;
      }
      String kbVersion = kbdMap.get(KMManager.KMKey_Version);
      if (kbVersion == null) {
        String latestKbVersion = getLatestKeyboardFileVersion(context, packageID, keyboardID);
        if (latestKbVersion != null) {
          kbVersion = latestKbVersion;
        }
      }
      Keyboard k = new Keyboard(
        packageID,
        keyboardID,
        kbdMap.get(KMManager.KMKey_KeyboardName),
        kbdMap.get(KMManager.KMKey_LanguageID),
        kbdMap.get(KMManager.KMKey_LanguageName),
        kbVersion,
        MapCompat.getOrDefault(kbdMap, KMManager.KMKey_CustomHelpLink, ""),
        MapCompat.getOrDefault(kbdMap, KMManager.KMKey_KMPLink, ""),
        isNewKeyboard,
        MapCompat.getOrDefault(kbdMap, KMManager.KMKey_Font, null),
        MapCompat.getOrDefault(kbdMap, KMManager.KMKey_OskFont, null)
      );

      // Check that sil_euro_latin and its deprecated keyboards only get added once
      if (keyboardID.equals("us") || keyboardID.equals("european") || keyboardID.equals("european2") ||
          keyboardID.equals("sil_euro_latin")) {
        if (!defaultKeyboardInstalled) {
          list.add(KMManager.getDefaultKeyboard(context));
          defaultKeyboardInstalled = true;
        }
      } else {
        // Otherwise add the keyboard
        list.add(k);
      }
    }

    return list;
  }

  /**
   * Loop through the installed keyboards list:
   * If the keyboard exists in cloud/ and packages/, migrate keyboard entry to packages/
   * Then remove the keyboard files in cloud/
   * @param context
   */
  public static void migrateCloudKeyboards(Context context) {
    boolean keyboardMigrated = false;
    for(int i=0; i<KeyboardController.getInstance().get().size(); i++) {
      // Cloud keyboard info
      Keyboard k = KeyboardController.getInstance().getKeyboardInfo(i);
      if (k.getPackageID().equals(KMManager.KMDefault_UndefinedPackageID)) {
        String keyboardID = k.getKeyboardID();
        // We'll want to preserve language fields
        String languageID = k.getLanguageID();
        String languageName = k.getLanguageName();

        // Look for a non-cloud packageID. See if there's one from the updateKMP field,
        // otherwise fall back to keyboardID
        String packageID = keyboardID;
        if (k.getUpdateKMP() != null && !k.getUpdateKMP().isEmpty()) {
          packageID = FileUtils.getFilename(k.getUpdateKMP()).replace(FileUtils.KEYMANPACKAGE, "");
        }

        // See if there's a matching keyboard of (package ID, keyboardID, ignoring language ID)
        int keyboardIndex = KeyboardController.getInstance().getKeyboardIndex(
          packageID, keyboardID, "");
        if (keyboardIndex != KeyboardController.INDEX_NOT_FOUND) {
          Keyboard keyboardFromPackage = KeyboardController.getInstance().getKeyboardInfo(keyboardIndex);
          // Create a copy of keyboardFromPackage, updating the language ID and language Name
          Keyboard migratedKeyboard = new Keyboard(keyboardFromPackage);
          migratedKeyboard.setLanguage(languageID, languageName);
          KeyboardController.getInstance().set(i, migratedKeyboard);

          // Remove the cloud keyboard files
          removeCloudKeyboard(keyboardID);
          keyboardMigrated = true;
        }
      }
    }

    if (keyboardMigrated) {
      KeyboardController.getInstance().save(context);
    }
  }

  /**
   * Remove all the keyboard files in the "cloud" directory matching a pattern
   * [keyboard ID]-[version].js
   *
   * @param keyboardID String of the keyboard ID
   */
  public static void removeCloudKeyboard(final String keyboardID) {
    String path = getCloudDir();
    File dir = new File(path);

    FileFilter keyboardFilter = new FileFilter() {
      @Override
      /**
       * Filter for JS keyboards that match [keyboardID]-[version].js
       */
      public boolean accept(File pathname) {
        String name = pathname.getName();

        String patternStr = KMString.format("^([A-Za-z0-9-_]+)-([0-9.]+)(\\.js)$");
        Pattern pattern = Pattern.compile(patternStr);
        Matcher matcher = pattern.matcher(name);
        if (matcher.matches() && (matcher.groupCount() == 3) &&
            (matcher.group(1).equals(keyboardID)) && (matcher.group(2) != null)) {
          return true;
        }
        return false;
      }
    };

    File[] files = dir.listFiles(keyboardFilter);
    if (files.length == 0) {
      return;
    }

    for (File file : files) {
      file.delete();
    }
  }

  /**
   * Sets mayPredictOverride true if the InputType field is a hidden password text field
   * (either TYPE_TEXT_VARIATION_PASSWORD or TYPE_TEXT_VARIATION_WEB_PASSWORD
   * but not TYPE_TEXT_VARIATION_VISIBLE_PASSWORD)
   * Also true for numeric text fields
   * @param inputType android.text.InputType
   */
  public static void setMayPredictOverride(int inputType) {
    mayPredictOverride =
      ((inputType == (InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD)) ||
       (inputType == (InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_WEB_PASSWORD)) ||
       isNumericField(inputType));
  }

  /**
   * Get the value of mayPredictOverride
   * @return boolean
   */
  public static boolean getMayPredictOverride() {
    return mayPredictOverride;
  }

  /**
   * Determines if the InputType field is a numeric field
   * @param inputType
   * @return boolean
   */
  public static boolean isNumericField(int inputType) {
    return (((inputType & InputType.TYPE_MASK_CLASS) == InputType.TYPE_CLASS_NUMBER) ||
      ((inputType & InputType.TYPE_MASK_CLASS) == InputType.TYPE_CLASS_PHONE));
  }

  /**
   * If override is true, embedded KMW crash reports are allowed to be sent to sentry.keyman.com
   * @param override - boolean
   */
  public static void setMaySendCrashReport(boolean override) {
    maySendCrashReport = override;
  }

  /**
   * Get the value of maySendCrashReport. Default is true
   * @return boolean
   */
  public static boolean getMaySendCrashReport() { return maySendCrashReport; };

  /**
   * Get the font typeface from a fully pathed font name
   * @param context
   * @param fontFilename String - full path to the font file
   * @return Typeface - null if font file doesn't exist or is Woff on Android 7.0 / 7.1
   */
  public static Typeface getFontTypeface(Context context, String fontFilename) {
    try {
      if ((fontFilename != null) && FileUtils.hasFontExtension(fontFilename)) {
        // Ignore .woff files if Android 7.0 / 7.1 (Issue #4896)
        if ((Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) &&
            (Build.VERSION.SDK_INT <= Build.VERSION_CODES.N_MR1) &&
            fontFilename.toLowerCase().endsWith(FileUtils.WOFFFONT)) {
          return null;
        }

        File file = new File(fontFilename);
        if (file.exists()) {
          return Typeface.createFromFile(file);
        }
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "Failed to create Typeface: " + fontFilename, e);
    }

    // No valid font to load
    return null;
  }

  public static List<Keyboard> getKeyboardsList(Context context) {
    return KeyboardPickerActivity.getKeyboardsList(context);
  }

  public static ArrayList<HashMap<String, String>> getLexicalModelsList(Context context) {
    return KeyboardPickerActivity.getLexicalModelsList(context);
  }

  public static Dataset getInstalledDataset(Context context) {
    return KeyboardPickerActivity.getInstalledDataset(context);
  }

  public static boolean registerLexicalModel(HashMap<String, String> lexicalModelInfo) {
    String pkgID = lexicalModelInfo.get(KMKey_PackageID);
    String modelID = lexicalModelInfo.get(KMKey_LexicalModelID);
    String languageID = lexicalModelInfo.get(KMKey_LanguageID);
    boolean modelFileExists = true;
    File modelFile = new File(getLexicalModelsDir(), pkgID + File.separator + modelID + ".model.js");
    String path = "file://" + modelFile.getAbsolutePath();

    // Disable sugestions if lexical-model file doesn't exist
    if (!modelFile.exists()) {
      modelFileExists = false;
      setBannerOptions(false);
      KMLog.LogError(TAG, modelFile.getAbsolutePath() + " does not exist");
    }

    JSONObject modelObj = new JSONObject();
    JSONArray languageJSONArray = new JSONArray();
    try {
      modelObj.put("id", modelID);
      languageJSONArray.put(languageID);
      modelObj.put("languages", languageJSONArray);
      modelObj.put("path", path);
      modelObj.put("CustomHelpLink", lexicalModelInfo.get(KMKey_CustomHelpLink));
    } catch (JSONException e) {
      KMLog.LogException(TAG, "Invalid lexical model to register", e);
      return false;
    }

    // Escape single quotes, and then convert double quotes to single quotes for javascript call
    String model = String.valueOf(modelObj);
    model = model.replaceAll("\'", "\\\\'"); // Double-escaped-backslash b/c regex.
    model = model.replaceAll("\"", "'");

    // When entering password field, mayPredict should override to false
    SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean mayPredict = (mayPredictOverride) ? false :
      prefs.getBoolean(getLanguagePredictionPreferenceKey(languageID), true);
    boolean mayCorrect = prefs.getBoolean(getLanguageCorrectionPreferenceKey(languageID), true);

    RelativeLayout.LayoutParams params;
    if (InAppKeyboard != null && InAppKeyboardLoaded && !InAppKeyboardShouldIgnoreTextChange && modelFileExists) {
      params = getKeyboardLayoutParams();
      InAppKeyboard.setLayoutParams(params);
      InAppKeyboard.loadJavascript(KMString.format("enableSuggestions(%s, %s, %s)", model, mayPredict, mayCorrect));
    }
    if (SystemKeyboard != null && SystemKeyboardLoaded && !SystemKeyboardShouldIgnoreTextChange && modelFileExists) {
      params = getKeyboardLayoutParams();
      SystemKeyboard.setLayoutParams(params);
      SystemKeyboard.loadJavascript(KMString.format("enableSuggestions(%s, %s, %s)", model, mayPredict, mayCorrect));
    }
    return true;
  }

  public static boolean deregisterLexicalModel(String modelID) {
    // Check if current lexical model needs to be cleared
    if (currentLexicalModel != null && currentLexicalModel.get(KMManager.KMKey_LexicalModelID).equalsIgnoreCase(modelID)) {
      currentLexicalModel = null;
    }

    String url = KMString.format("deregisterModel('%s')", modelID);
    if (InAppKeyboard != null) { // && InAppKeyboardLoaded) {
      InAppKeyboard.loadJavascript(url);
    }

    if (SystemKeyboard != null) { // && SystemKeyboardLoaded) {
      SystemKeyboard.loadJavascript(url);
    }
    return true;
  }

  public static boolean setBannerOptions(boolean mayPredict) {
    String url = KMString.format("setBannerOptions(%s)", mayPredict);
    if (InAppKeyboard != null) {
      InAppKeyboard.loadJavascript(url);
    }

    if (SystemKeyboard != null) {
      SystemKeyboard.loadJavascript(url);
    }
    return true;
  }

  public static boolean addLexicalModel(Context context, HashMap<String, String> lexicalModelInfo) {
    return KeyboardPickerActivity.addLexicalModel(context, lexicalModelInfo);
  }

  /**
   * registerAssociatedLexicalModel - Registers a lexical model with the associated language ID.
   *         If a new model gets loaded, returns true.
   * @param langId - String of the language ID
   * @return boolean - True if a new model is loaded
   */
  public static boolean registerAssociatedLexicalModel(String langId) {
    boolean status = false;
    HashMap<String, String> lmInfo = getAssociatedLexicalModel(langId);
    if (lmInfo != null && lmInfo != currentLexicalModel) {
      registerLexicalModel(lmInfo);
      status = true;
    }
    currentLexicalModel = lmInfo;

    return status;
  }

  /**
   * Search the installed lexical models list and see if there's an
   * associated model for a given language ID
   * @param langId - String of the language ID
   * @return HashMap<String, String> Model information if it exists. Otherwise null
   */
  public static HashMap<String, String> getAssociatedLexicalModel(String langId) {
    ArrayList<HashMap<String, String>> lexicalModelsList = getLexicalModelsList(appContext);
    if (lexicalModelsList != null) {
      int length = lexicalModelsList.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> lmInfo = lexicalModelsList.get(i);
        if (BCP47.languageEquals(langId, lmInfo.get(KMManager.KMKey_LanguageID))) {
          return lmInfo;
        }
      }
    }

    return null;
  }

  public static boolean addKeyboard(Context context, Keyboard keyboardInfo) {
    String packageID = keyboardInfo.getPackageID();
    String keyboardID = keyboardInfo.getKeyboardID();
    keyboardInfo.setNewKeyboard(true);

    // Log Sentry analytic event, ignoring default keyboard
    if (Sentry.isEnabled() && !(packageID.equalsIgnoreCase(KMManager.KMDefault_PackageID) &&
      keyboardID.equalsIgnoreCase(KMManager.KMDefault_KeyboardID))) {
      Breadcrumb breadcrumb = new Breadcrumb();
      breadcrumb.setMessage("KMManager.addKeyboard");
      breadcrumb.setCategory("addKeyboard");
      breadcrumb.setLevel(SentryLevel.INFO);
      breadcrumb.setData("packageID", packageID);
      breadcrumb.setData("keyboardID", keyboardID);
      breadcrumb.setData("keyboardName", keyboardInfo.getKeyboardName());
      breadcrumb.setData("keyboardVersion", keyboardInfo.getVersion());
      breadcrumb.setData("languageID", keyboardInfo.getLanguageID());
      breadcrumb.setData("languageName", keyboardInfo.getLanguageName());

      Sentry.addBreadcrumb(breadcrumb);

      // For now, not sending a Sentry.captureMessage()
      // This means the breadcrumb won't get sent until a crash happens.
    }

    return KeyboardPickerActivity.addKeyboard(context, keyboardInfo);
  }

  // Intend to deprecate in Keyman 15.0
  public static boolean addKeyboard(Context context, HashMap<String, String> keyboardInfo) {
    String packageID = keyboardInfo.get(KMManager.KMKey_PackageID);
    String keyboardID =  keyboardInfo.get(KMManager.KMKey_KeyboardID);
    String keyboardName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
    String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
    String languageName = keyboardInfo.get(KMManager.KMKey_LanguageName);
    String version = keyboardInfo.get(KMManager.KMKey_KeyboardVersion);
    String helpLink = keyboardInfo.get(KMManager.KMKey_CustomHelpLink);
    String kmpLink = MapCompat.getOrDefault(keyboardInfo, KMManager.KMKey_KMPLink, "");
    String font = keyboardInfo.get(KMManager.KMKey_Font);
    String oskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
    boolean isNewKeyboard = true;

    Keyboard k = new Keyboard(packageID, keyboardID, keyboardName,
          languageID, languageName, version, helpLink, kmpLink,
      isNewKeyboard, font, oskFont);
    return addKeyboard(context, k);
  }

  public static boolean removeKeyboard(Context context, int position) {
    return KeyboardPickerActivity.removeKeyboard(context, position);
  }

  /**
   * Some apps have the user select which keyboard to add. To ensure there's always a fallback
   * system keyboard when the keyboard list is empty, use this method.
   * If setDefaultKeyboard() is never used, this will return default sil_euro_latin.
   * @param context Context
   * @return Keyboard Default fallback keyboard
   */
  public static Keyboard getDefaultKeyboard(Context context) {
    return Keyboard.getDefaultKeyboard(context);
  }

  /**
   * Set a default fallback keyboard. If null, getDefaultKeyboard() will return sil_euro_latin.
   * @param k Keyboard info for the fallback keyboard.
   */
  public static void setDefaultKeyboard(Keyboard k) {
    Keyboard.setDefaultKeyboard(k);
  }

  public static boolean setKeyboard(String packageID, String keyboardID, String languageID) {
    boolean result1 = false;
    boolean result2 = false;

    if (InAppKeyboard != null && InAppKeyboardLoaded)
      result1 = InAppKeyboard.setKeyboard(packageID, keyboardID, languageID);

    if (SystemKeyboard != null && SystemKeyboardLoaded)
      result2 = SystemKeyboard.setKeyboard(packageID, keyboardID, languageID);

    return (result1 || result2);
  }

  public static boolean setKeyboard(Keyboard keyboardInfo) {
    boolean result1 = false;
    boolean result2 = false;

    if (InAppKeyboard != null && InAppKeyboardLoaded && keyboardInfo != null) {
      result1 = InAppKeyboard.setKeyboard(keyboardInfo);
    }

    if (SystemKeyboard != null && SystemKeyboardLoaded && keyboardInfo != null)
      result2 = SystemKeyboard.setKeyboard(keyboardInfo);

    if (keyboardInfo != null) {
      registerAssociatedLexicalModel(keyboardInfo.getLanguageID());
    }

    return (result1 || result2);
  }

  /**
   * Prepare keyboard switch for inapp keyboard and systemkeyboard
   * @param packageID the package id
   * @param keyboardID the keyboard id
   * @param languageID the language id
   * @param keyboardName keyboard name
   * @return the success result
   */
  public static boolean prepareKeyboardSwitch(String packageID, String keyboardID, String languageID, String keyboardName) {


    boolean result1 = false;
    boolean result2 = false;

    if (InAppKeyboard != null && InAppKeyboardLoaded)
    {
      result1 = InAppKeyboard.prepareKeyboardSwitch(packageID, keyboardID, languageID,keyboardName);
    }
    if (SystemKeyboard != null && SystemKeyboardLoaded)
    {
      result2 = SystemKeyboard.prepareKeyboardSwitch(packageID, keyboardID, languageID,keyboardName);
    }

    if(result1 || result2)
    {
      //reset banner state if new language has no lexical model
      if(currentBanner.equals(KMManager.KM_BANNER_STATE_SUGGESTION)
        && getAssociatedLexicalModel(languageID)==null)
        currentBanner = KMManager.KM_BANNER_STATE_BLANK;

      if(result1)
        InAppKeyboard.setLayoutParams(getKeyboardLayoutParams());
      if(result2)
        SystemKeyboard.setLayoutParams(getKeyboardLayoutParams());
    }

    registerAssociatedLexicalModel(languageID);

    return (result1 || result2);
  }

  public static boolean setKeyboard(String packageID, String keyboardID, String languageID, String keyboardName, String languageName, String kFont, String kOskFont) {
    boolean result1 = false;
    boolean result2 = false;

    if (InAppKeyboard != null && (InAppKeyboardLoaded || isTestMode()))
      result1 = InAppKeyboard.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);

    if (SystemKeyboard != null && SystemKeyboardLoaded)
      result2 = SystemKeyboard.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);

    registerAssociatedLexicalModel(languageID);

    return (result1 || result2);
  }

  public static boolean setKeyboard(Context context, int position) {
    Keyboard keyboardInfo = getKeyboardInfo(context, position);
    if (keyboardInfo == null)
      return false;

    return setKeyboard(keyboardInfo);
  }

  public static void switchToNextKeyboard(Context context) {
    int index = KeyboardController.getInstance().getKeyboardIndex(KMKeyboard.currentKeyboard());
    index++;
    if (index >= KeyboardController.getInstance().get().size()) {
      index = 0;
    }
    Keyboard kbInfo = KeyboardController.getInstance().getKeyboardInfo(index);
    if (kbInfo == null) {
      index = 0;
      kbInfo = KeyboardController.getInstance().getKeyboardInfo(index);
    }

    if (InAppKeyboard != null) {
      InAppKeyboard.setKeyboard(kbInfo);
    }

    if (SystemKeyboard != null) {
      SystemKeyboard.setKeyboard(kbInfo);
    }

    registerAssociatedLexicalModel(kbInfo.getLanguageID());
  }

  protected static IBinder getToken() {
    if (IMService == null) {
      return null;
    }
    final Dialog dialog = IMService.getWindow();
    if (dialog == null) {
      return null;
    }
    final Window window = dialog.getWindow();
    if (window == null) {
      return null;
    }
    return window.getAttributes().token;
  }

  public static void advanceToNextInputMode() {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      if (IMService != null) {
        IMService.switchToNextInputMethod(false);
      }
    } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN) {
      // This method is added in API level 16 and deprecated in API level 28
      // Reference: https://developer.android.com/reference/android/view/inputmethod/InputMethodManager.html#switchToNextInputMethod(android.os.IBinder,%20boolean)
      InputMethodManager imm = (InputMethodManager) appContext.getSystemService(Context.INPUT_METHOD_SERVICE);
      imm.switchToNextInputMethod(getToken(), false);
    }
  }

  // TODO: Refactor InAppKeyboard / SystemKeyboard logic
  public static KMKeyboard getKMKeyboard(KeyboardType keyboard) {
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP) {
      return InAppKeyboard;
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      return SystemKeyboard;
    } else {
      // What should we do if KeyboardType.KEYBOARD_TYPE_UNDEFINED?
      String msg = "Keyboard type undefined";
      KMLog.LogError(TAG, msg);
      return null;
    }
  }

  public static String getKeyboardTextFontFilename() {
    return KMKeyboard.textFontFilename();
  }

  public static Typeface getKeyboardTextFontTypeface(Context context) {
    return getFontTypeface(context, getKeyboardTextFontFilename());
  }

  public static String getKeyboardOskFontFilename() {
    return KMKeyboard.oskFontFilename();
  }

  public static Typeface getKeyboardOskFontTypeface(Context context) {
    return getFontTypeface(context, getKeyboardOskFontFilename());
  }

  public static KeyboardState getKeyboardState(Context context, String packageID, String keyboardID, String languageID) {
    KeyboardState kbState = KeyboardState.KEYBOARD_STATE_UNDEFINED;
    if (packageID == null || keyboardID == null || languageID == null) {
      return kbState;
    }

    packageID = packageID.trim();
    keyboardID = keyboardID.trim();
    languageID = languageID.trim();
    if (keyboardID.isEmpty() || languageID.isEmpty()) {
      return kbState;
    }

    // Check latest installed keyboard version
    Keyboard kbInfo = null;
    String kbVersion = null;
    kbState = KeyboardState.KEYBOARD_STATE_NEEDS_DOWNLOAD;
    int index = KeyboardController.getInstance().getKeyboardIndex(packageID, languageID, keyboardID);
    if (index != KeyboardController.INDEX_NOT_FOUND) {
      kbInfo = KeyboardController.getInstance().getKeyboardInfo(index);
      if (kbInfo != null) {
        kbVersion = kbInfo.getVersion();
        if (kbVersion != null) {
          // See if update is available
          if (kbInfo.hasUpdateAvailable()) {
            kbState = KeyboardState.KEYBOARD_STATE_NEEDS_UPDATE;
          } else {
            kbState = KeyboardState.KEYBOARD_STATE_UP_TO_DATE;
          }
        }
      }
    }

    return kbState;
  }

  /**
   * Determine the latest version number for an installed keyboard.
   * When packageID = "cloud", search through the JS keyboard files for the latest (greatest) version number.
   * For other packageIDs, parse the kmp.json
   * @param context
   * @param packageID
   * @param keyboardID
   * @return kbFileVersion String. null if the keyboard doesn't exist
   */
  public static String getLatestKeyboardFileVersion(Context context, final String packageID, final String keyboardID) {
    String kbFileVersion = null;
    String path;
    if (packageID.equals(KMDefault_UndefinedPackageID)) {
      path = getCloudDir();

      File dir = new File(path);
      FileFilter keyboardFilter = new FileFilter() {
        @Override
        /**
         * Filter for JS keyboards that match keyboardID and have a non-zero size
         */
        public boolean accept(File pathname) {
          String name = pathname.getName();
          if (pathname.isFile() && name.startsWith(keyboardID) && FileUtils.hasJavaScriptExtension(name) && pathname.length() > 0) {
            return true;
          }
          return false;
        }
      };

      File[] files = dir.listFiles(keyboardFilter);
      if (files.length == 0) {
        return kbFileVersion;
      }

      for (File file : files) {
        String filename = file.getName();
        String base = KMString.format("%s-", keyboardID);
        int index = filename.indexOf(base);
        if (index == 0) {
          int firstIndex = base.length();
          int lastIndex = filename.toLowerCase().lastIndexOf(".js");
          String v = filename.substring(firstIndex, lastIndex);
          if (kbFileVersion != null) {
            if (FileUtils.compareVersions(v, kbFileVersion) == FileUtils.VERSION_GREATER) {
              kbFileVersion = v;
            }
          } else if (FileUtils.compareVersions(v, v) == FileUtils.VERSION_EQUAL) {
            kbFileVersion = v;
          }
        }
      }
    } else {
      path = getPackagesDir() + packageID + File.separator + PackageProcessor.PP_DEFAULT_METADATA;

      File kmpJSONFile = new File(path);
      if (!kmpJSONFile.exists()) {
        if (!KMManager.isTestMode()) {
          KMLog.LogError(TAG, path + " not found. Returning version 1.0");
        }
        return "1.0";
      }
      JSONParser jsonParser = new JSONParser();
      JSONObject kmpObject = jsonParser.getJSONObjectFromFile(kmpJSONFile);

      return PackageProcessor.getKeyboardVersion(kmpObject, keyboardID);
    }

    return kbFileVersion;
  }

  /**
   * Determine the latest version number for an installed lexical model by parsing kmp.json.
   * @param context
   * @param packageID
   * @return String.
   */
  public static String getLexicalModelPackageVersion(Context context, final String packageID) {
    String path = getLexicalModelsDir() + packageID + File.separator + PackageProcessor.PP_DEFAULT_METADATA;

    try {
      File kmpJSONFile = new File(path);
      if (!kmpJSONFile.exists()) {
        return null;
      }
      JSONParser jsonParser = new JSONParser();
      JSONObject kmpObject = jsonParser.getJSONObjectFromFile(kmpJSONFile);

      return LexicalModelPackageProcessor.getPackageVersion(kmpObject);
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
      return null;
    }
  }

  public static void addKeyboardDownloadEventListener(OnKeyboardDownloadEventListener listener) {
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(listener);
  }

  public static void addKeyboardEventListener(OnKeyboardEventListener listener) {
    KMTextView.addOnKeyboardEventListener(listener);
    KMKeyboard.addOnKeyboardEventListener(listener);
  }

  public static void removeKeyboardDownloadEventListener(OnKeyboardDownloadEventListener listener) {
    KMKeyboardDownloaderActivity.removeKeyboardDownloadEventListener(listener);
  }

  public static void removeKeyboardEventListener(OnKeyboardEventListener listener) {
    KMTextView.removeOnKeyboardEventListener(listener);
    KMKeyboard.removeOnKeyboardEventListener(listener);
  }

  public static int getBannerHeight(Context context) {
    int bannerHeight = 0;
    if (currentBanner.equals(KM_BANNER_STATE_SUGGESTION)) {
      bannerHeight = (int) context.getResources().getDimension(R.dimen.banner_height);
    }
    return bannerHeight;
  }

  public static int getKeyboardHeight(Context context) {
    int defaultHeight = (int) context.getResources().getDimension(R.dimen.keyboard_height);
    SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    int orientation = context.getResources().getConfiguration().orientation;
    if (orientation == Configuration.ORIENTATION_PORTRAIT) {
      return prefs.getInt(KMManager.KMKey_KeyboardHeightPortrait, defaultHeight);
    } else if (orientation == Configuration.ORIENTATION_LANDSCAPE) {
      return prefs.getInt(KMManager.KMKey_KeyboardHeightLandscape, defaultHeight);
    }

    return defaultHeight;
  }

  public static void applyKeyboardHeight(Context context, int height) {
    if (InAppKeyboard != null && InAppKeyboardLoaded) {
      InAppKeyboard.loadJavascript(KMString.format("setOskHeight('%s')", height));
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      InAppKeyboard.setLayoutParams(params);
    }
    if (SystemKeyboard != null && SystemKeyboardLoaded) {
      SystemKeyboard.loadJavascript(KMString.format("setOskHeight('%s')", height));
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      SystemKeyboard.setLayoutParams(params);
    }
  }

  public static void setDebugMode(boolean value) {
    debugMode = value;
  }

  public static boolean isDebugMode() {
    return debugMode;
  }

  /**
   * @return check for unit test.
   */
  public static boolean isTestMode() {
    return Boolean.parseBoolean(System.getProperty("kmeaTestMode"));
  }

  public static void setShouldAllowSetKeyboard(boolean value) {
    shouldAllowSetKeyboard = value;
    if (shouldAllowSetKeyboard == false) {
      setKeyboard(KMDefault_PackageID, KMDefault_KeyboardID,
        KMDefault_LanguageID, KMDefault_KeyboardName, KMDefault_LanguageName, KMDefault_KeyboardFont, null);
    }
  }

  public static boolean shouldAllowSetKeyboard() {
    return shouldAllowSetKeyboard;
  }

  public static void showKeyboardPicker(Context context, KeyboardType kbType) {
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      Intent i = new Intent(context, KeyboardPickerActivity.class);
      i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
      i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
      i.putExtra(KMKey_DisplayKeyboardSwitcher, false);
      context.startActivity(i);
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      Intent i = new Intent(context, KeyboardPickerActivity.class);
      i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
      i.addFlags(Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS);
      i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK);
      i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
      i.putExtra(KMKey_DisplayKeyboardSwitcher, true);
      context.startActivity(i);
    }
  }

  public static void setKeyboardPickerFont(Typeface typeface) {
    KeyboardPickerActivity.listFont = typeface;
  }

  // This API is deprecated in Keyman 14.0
  public static void showLanguageList(Context context) {
    return;
  }

  public static void setNumericLayer(KeyboardType kbType) {
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard != null && InAppKeyboardLoaded && !InAppKeyboardShouldIgnoreTextChange) {
        InAppKeyboard.loadJavascript("setNumericLayer()");
      }
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard != null && SystemKeyboardLoaded && !SystemKeyboardShouldIgnoreTextChange) {
        SystemKeyboard.loadJavascript("setNumericLayer()");
      }
    }
  }

  public static boolean updateText(KeyboardType kbType, String text) {
    boolean result = false;
    String kmText = "";
    if (text != null) {
      kmText = text.toString().replace("\\", "\\u005C").replace("'", "\\u0027").replace("\n", "\\n");
    }

    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard != null && InAppKeyboardLoaded && !InAppKeyboardShouldIgnoreTextChange) {
        InAppKeyboard.loadJavascript(KMString.format("updateKMText('%s')", kmText));
        result = true;
      }

      InAppKeyboardShouldIgnoreTextChange = false;
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard != null && SystemKeyboardLoaded && !SystemKeyboardShouldIgnoreTextChange) {
        SystemKeyboard.loadJavascript(KMString.format("updateKMText('%s')", kmText));
        result = true;
      }

      SystemKeyboardShouldIgnoreTextChange = false;
    }

    return result;
  }

  public static boolean updateSelectionRange(KeyboardType kbType, int selStart, int selEnd) {
    boolean result = false;
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard != null && InAppKeyboardLoaded && !InAppKeyboardShouldIgnoreSelectionChange) {
        InAppKeyboard.loadJavascript(KMString.format("updateKMSelectionRange(%d,%d)", selStart, selEnd));
        result = true;
      }

      InAppKeyboardShouldIgnoreSelectionChange = false;
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard != null && SystemKeyboardLoaded && !SystemKeyboardShouldIgnoreSelectionChange) {
        InputConnection ic = (IMService != null ? IMService.getCurrentInputConnection() : null);
        if (ic != null) {
          ExtractedText icText = ic.getExtractedText(new ExtractedTextRequest(), 0);
          if (icText != null) {
            updateText(kbType, icText.text.toString());
          }
        }

        SystemKeyboard.loadJavascript(KMString.format("updateKMSelectionRange(%d,%d)", selStart, selEnd));
        result = true;
      }

      SystemKeyboardShouldIgnoreSelectionChange = false;
    }

    return result;
  }

  public static void resetContext(KeyboardType kbType) {
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard != null && InAppKeyboardLoaded) {
        InAppKeyboard.loadJavascript("resetContext()");
      }
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard != null && SystemKeyboardLoaded) {
        SystemKeyboard.loadJavascript("resetContext()");
      }
    }
  }

  public static int getCurrentKeyboardIndex(Context context) {
    String key = KMKeyboard.currentKeyboard();
    return KeyboardController.getInstance().getKeyboardIndex(key);
  }

  public static Keyboard getCurrentKeyboardInfo(Context context) {
    int index = getCurrentKeyboardIndex(context);
    return KeyboardController.getInstance().getKeyboardInfo(index);
  }

  public static int getKeyboardIndex(Context context, String keyboardID, String languageID) {
    int index = KeyboardController.INDEX_NOT_FOUND;

    if (keyboardID != null & languageID != null) {
      String kbKey = KMString.format("%s_%s", languageID, keyboardID);
      index = KeyboardController.getInstance().getKeyboardIndex(kbKey);
    }

    return index;
  }

  public static Keyboard getKeyboardInfo(Context context, int index) {
    return KeyboardController.getInstance().getKeyboardInfo(index);
  }

  public static HashMap<String, String> getLexicalModelInfo(Context context, int index) {
    return KeyboardPickerActivity.getLexicalModelInfo(context, index);
  }

  public static boolean keyboardExists(Context context, String packageID, String keyboardID, String languageID) {
    boolean result = false;

    if (packageID != null && keyboardID != null && languageID != null) {
      File keyboardFile = new File(getPackagesDir(), packageID + File.separator + KMString.format("%s.js", keyboardID));
      result = KeyboardController.getInstance().keyboardExists(packageID, keyboardID, languageID) &&
        keyboardFile.exists();
    }

    return result;
  }

  public static boolean lexicalModelExists(Context context, String packageID, String languageID, String modelID) {
    boolean result = false;

    if (packageID != null && languageID != null &&  modelID != null) {
      String lmKey = KMString.format("%s_%s_%s", packageID, languageID, modelID);
      result = KeyboardPickerActivity.containsLexicalModel(context, lmKey);
    }

    return result;
  }

  public static boolean isHelpBubbleEnabled() {
    boolean retVal = true;

    if (InAppKeyboard != null) {
      retVal = InAppKeyboard.isHelpBubbleEnabled;
    } else if (SystemKeyboard != null) {
      retVal = SystemKeyboard.isHelpBubbleEnabled;
    }

    return retVal;
  }

  public static void setHelpBubbleEnabled(boolean newValue) {
    if (InAppKeyboard != null) {
      InAppKeyboard.isHelpBubbleEnabled = newValue;
    }

    if (SystemKeyboard != null) {
      SystemKeyboard.isHelpBubbleEnabled = newValue;
    }
  }

  public static SpacebarText getSpacebarText() {
    return spacebarText;
  }

  public static void setSpacebarText(SpacebarText mode) {
    spacebarText = mode;
    if(InAppKeyboard != null) {
      InAppKeyboard.setSpacebarText(mode);
    }
    if(SystemKeyboard != null) {
      SystemKeyboard.setSpacebarText(mode);
    }
  }

  public static boolean canAddNewKeyboard() {
    return KeyboardPickerActivity.canAddNewKeyboard;
  }

  public static void setCanAddNewKeyboard(boolean newValue) {
    KeyboardPickerActivity.canAddNewKeyboard = newValue;
  }

  public static boolean canRemoveKeyboard() {
    return KeyboardPickerActivity.canRemoveKeyboard;
  }

  public static void setCanRemoveKeyboard(boolean newValue) {
    KeyboardPickerActivity.canRemoveKeyboard = newValue;
  }

  public static boolean shouldCheckKeyboardUpdates() {
    return KeyboardPickerActivity.shouldCheckKeyboardUpdates;
  }

  public static void setShouldCheckKeyboardUpdates(boolean newValue) {
    KeyboardPickerActivity.shouldCheckKeyboardUpdates = newValue;
  }

  /**
   * Get the default short press action for the globe key
   * @param kbType - KeyboardType.KEYBOARD_TYPE_SYSTEM or KeyboardType.KEYBOARD_INAPP
   * @return GlobeKeyAction
   */
  public static GlobeKeyAction getGlobeKeyAction(KeyboardType kbType) {
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      return inappKbGlobeKeyAction;
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      return sysKbGlobeKeyAction;
    } else {
      return GlobeKeyAction.GLOBE_KEY_ACTION_DO_NOTHING;
    }
  }

  public static void setGlobeKeyAction(KeyboardType kbType, GlobeKeyAction action) {
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      inappKbGlobeKeyAction = action;
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      sysKbGlobeKeyAction = action;
    }
  }

  public static GlobeKeyState getGlobeKeyState() {
    return globeKeyState;
  }

  public static void setGlobeKeyState(GlobeKeyState state) {
    globeKeyState = state;
  }

  /**
   * Handle the globe key action
   * @param globeKeyDown boolean if the globe key state is GLOBE_KEY_STATE_DOWN
   * @param keyboardType KeyboardType KEYBOARD_TYPE_INAPP or KEYBOARD_TYPE_SYSTEM
   */
  private static void handleGlobeKeyAction(Context context, boolean globeKeyDown, KeyboardType keyboardType) {
    // Update globeKeyState
    if (globeKeyState != GlobeKeyState.GLOBE_KEY_STATE_LONGPRESS) {
      globeKeyState = globeKeyDown ? GlobeKeyState.GLOBE_KEY_STATE_DOWN : GlobeKeyState.GLOBE_KEY_STATE_UP;
    }

    if (KMManager.shouldAllowSetKeyboard()) {
      KeyguardManager keyguardManager = (KeyguardManager) appContext.getSystemService(Context.KEYGUARD_SERVICE);
      // inKeyguardRestrictedInputMode() deprecated, so check isKeyguardLocked() to determine if screen is locked
      if (keyguardManager.isKeyguardLocked()) {
        if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM && globeKeyState == GlobeKeyState.GLOBE_KEY_STATE_UP) {
          doGlobeKeyLockscreenAction(context);
        }
        // clear globeKeyState
        globeKeyState = GlobeKeyState.GLOBE_KEY_STATE_UP;
      } else {
        // If screen isn't locked, reset the starting index
        startingKeyboardIndexOnLockScreen = -1;

        // Normal handling for globe key
        if (globeKeyState == GlobeKeyState.GLOBE_KEY_STATE_LONGPRESS) {
          // Longpress globe
          doGlobeKeyLongpressAction(context, keyboardType);

          // clear globeKeyState
          globeKeyState = GlobeKeyState.GLOBE_KEY_STATE_UP;
        } else if (globeKeyState == GlobeKeyState.GLOBE_KEY_STATE_UP) {
          // Shortpress globe
          doGlobeKeyShortpressAction(context, keyboardType);
        }
      }
    } else {
      // clear globeKeyState
      globeKeyState = GlobeKeyState.GLOBE_KEY_STATE_UP;
    }
  }

  protected static final class KMInAppKeyboardWebViewClient extends WebViewClient {
    public static Context context;

    KMInAppKeyboardWebViewClient(Context context) {
      KMInAppKeyboardWebViewClient.context = context;
    }

    @Override
    public void onPageStarted(WebView view, String url, Bitmap favicon) {
    }

    @Override
    public void onPageFinished(WebView view, String url) {
      Log.d("KMEA", "onPageFinished: [inapp] " + url);
      shouldOverrideUrlLoading(view, url);
    }

    private void pageLoaded(WebView view, String url) {
      Log.d("KMEA", "pageLoaded: [inapp] " + url);
      if (InAppKeyboard == null) {
        KMLog.LogError(TAG, "pageLoaded and InAppKeyboard null");
        return;
      }
      InAppKeyboard.keyboardSet = false;
      currentLexicalModel = null;

      if (url.startsWith("file")) { // TODO: is this test necessary?
        InAppKeyboardLoaded = true;

        SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
        if (index < 0) {
          index = 0;
        }
        Keyboard keyboardInfo = KMManager.getKeyboardInfo(context, index);
        String langId = null;
        if (keyboardInfo != null) {
          langId = keyboardInfo.getLanguageID();
          InAppKeyboard.setKeyboard(keyboardInfo);
        } else {
          // Revert to default (index 0) or fallback keyboard
          keyboardInfo = KMManager.getKeyboardInfo(context, 0);
          if (keyboardInfo == null) {
            // Not logging to Sentry because some keyboard apps like FV don't install keyboards until the user chooses
            keyboardInfo = KMManager.getDefaultKeyboard(context);
          }
          if (keyboardInfo != null) {
            langId = keyboardInfo.getLanguageID();
            InAppKeyboard.setKeyboard(keyboardInfo);
          }
        }

        registerAssociatedLexicalModel(langId);

        Handler handler = new Handler();
        handler.postDelayed(new Runnable() {
          @Override
          public void run() {
            SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
            if (prefs.getBoolean(KMManager.KMKey_ShouldShowHelpBubble, true)) {
              InAppKeyboard.loadJavascript("showHelpBubble()");
            }
          }
        }, 2000);

        InAppKeyboard.callJavascriptAfterLoad();
        InAppKeyboard.setSpacebarText(spacebarText);

        KeyboardEventHandler.notifyListeners(KMTextView.kbEventListeners, KeyboardType.KEYBOARD_TYPE_INAPP, EventType.KEYBOARD_LOADED, null);

        // Special handling for in-app TextView context keymanapp/keyman#3809
        if (KMTextView.activeView != null && KMTextView.activeView.getClass() == KMTextView.class) {
          KMTextView.updateTextContext();
        }
      }
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
      Log.d("KMEA", "shouldOverrideUrlLoading [inapp]: " + url);
      if (InAppKeyboard == null) {
        KMLog.LogError(TAG, "shouldOverrideUrlLoading and InAppKeyboard null");
        return false;
      }

      // URL has actual path to the keyboard.html file as a prefix!  We need to replace
      // just the first intended '#' to get URI-based query param processing.
      // At some point, other parts of the function should be redone to allow use of ? instead
      // of # in our WebView command "queries" entirely.
      String cmd = url.replace("keyboard.html#", "keyboard.html?");
      Uri urlCommand = Uri.parse(cmd);
      if (url.indexOf("pageLoaded") >= 0) {
        pageLoaded(view, url);
      } else if (url.indexOf("hideKeyboard") >= 0) {
        if (KMTextView.activeView != null && KMTextView.activeView.getClass() == KMTextView.class) {
          InAppKeyboard.dismissHelpBubble();
          KMTextView textView = (KMTextView) KMTextView.activeView;
          textView.dismissKeyboard();
        }
      } else if (urlCommand.getQueryParameter("globeKeyAction") != null) {
        InAppKeyboard.dismissHelpBubble();
        if (!InAppKeyboard.isHelpBubbleEnabled) {
          return false;
        }

        SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(KMManager.KMKey_ShouldShowHelpBubble, false);
        editor.commit();

        handleGlobeKeyAction(context, urlCommand.getBooleanQueryParameter("keydown", false),
          KeyboardType.KEYBOARD_TYPE_INAPP);
      } else if (url.indexOf("showHelpBubble") >= 0) {
        int start = url.indexOf("keyPos=") + 7;
        String value = url.substring(start);
        if (!value.isEmpty()) {
          String[] globeKeyPos = value.split("\\,");
          float fx = Float.valueOf(globeKeyPos[0]);
          float fy = Float.valueOf(globeKeyPos[1]);
          if (InAppKeyboard.getVisibility() == View.VISIBLE)
            InAppKeyboard.showHelpBubble(context, fx, fy);
        }
      } else if (url.indexOf("showKeyPreview") >= 0) {
        String deviceType = context.getResources().getString(R.string.device_type);
        if (deviceType.equals("AndroidTablet")) {
          return false;
        }

        if (InAppKeyboard.subKeysWindow != null) {
          return false;
        }

        int start = url.indexOf("x=") + 2;
        int end = url.indexOf("+y=");
        float x = Float.valueOf(url.substring(start, end));

        start = url.indexOf("y=") + 2;
        end = url.indexOf("+w=");
        float y = Float.valueOf(url.substring(start, end));

        start = url.indexOf("w=") + 2;
        end = url.indexOf("+h=");
        float w = Float.valueOf(url.substring(start, end));

        start = url.indexOf("h=") + 2;
        end = url.indexOf("+t=");
        float h = Float.valueOf(url.substring(start, end));

        start = url.indexOf("t=") + 2;
        String t = url.substring(start);
        String text = InAppKeyboard.convertKeyText(t);

        float left = x - w / 2.0f;
        float right = left + w;
        float top = y - 1;
        float bottom = top + h;

        RectF keyFrame = new RectF(left, top, right, bottom);
        InAppKeyboard.showKeyPreview(context, (int) x, (int) y, keyFrame, text);
      } else if (url.indexOf("dismissKeyPreview") >= 0) {
        InAppKeyboard.dismissKeyPreview(100);
      } else if (url.indexOf("showMore") >= 0) {
        if (InAppKeyboard.subKeysWindow != null && InAppKeyboard.subKeysWindow.isShowing()) {
          return false;
        }

        int start = url.indexOf("keyPos=") + 7;
        int end = url.indexOf("+keys=");
        InAppKeyboard.subKeysWindowPos = url.substring(start, end).split("\\,");

        start = end + 6;
        end = url.indexOf("+font=");
        if (end < 0) {
          end = url.length();
          InAppKeyboard.specialOskFont = "";
        } else {
          InAppKeyboard.specialOskFont = KMFilename_Osk_Ttf_Font;
        }

        String keys = url.substring(start, end);

        String[] keyList = keys.split("\\;");
        int klCount = keyList.length;
        InAppKeyboard.subKeysList = new ArrayList<HashMap<String, String>>();
        for (int i = 0; i < klCount; i++) {
          String[] values = keyList[i].split("\\:");
          String keyId = (values.length > 0) ? values[0] : "";
          String keyText = (values.length > 1) ? values[1] : "";

          HashMap<String, String> hashMap = new HashMap<String, String>();
          hashMap.put("keyId", keyId);
          hashMap.put("keyText", keyText);
          InAppKeyboard.subKeysList.add(hashMap);
        }
      } else if (url.indexOf("refreshBannerHeight") >= 0) {
        int start = url.indexOf("change=") + 7;
        String change = url.substring(start);
        boolean isModelActive = change.equals("active");
        SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        boolean modelPredictionPref = false;
        if (currentLexicalModel != null) {
          modelPredictionPref = prefs.getBoolean(getLanguagePredictionPreferenceKey(currentLexicalModel.get(KMManager.KMKey_LanguageID)), true);
        }
        currentBanner = (isModelActive && modelPredictionPref) ?
          KM_BANNER_STATE_SUGGESTION : KM_BANNER_STATE_BLANK;
        RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
        InAppKeyboard.setLayoutParams(params);
      } else if (url.indexOf("suggestPopup") >= 0) {
        double x = Float.parseFloat(urlCommand.getQueryParameter("x"));
        double y = Float.parseFloat(urlCommand.getQueryParameter("y"));
        double width = Float.parseFloat(urlCommand.getQueryParameter("w"));
        double height = Float.parseFloat(urlCommand.getQueryParameter("h"));
        String suggestionJSON = urlCommand.getQueryParameter("suggestion");

        JSONParser parser = new JSONParser();
        JSONObject obj = parser.getJSONObjectFromURIString(suggestionJSON);

        /*  // For future implementation
        InAppKeyboard.suggestionWindowPos = new double[]{x, y};
        InAppKeyboard.suggestionJSON = suggestionJSON;

        try {
          Log.v("KMEA", "Suggestion display: " + obj.getString("displayAs"));
          Log.v("KMEA", "Suggestion's banner coords: " + x + ", " + y + ", " + width + ", " + height);
          Log.v("KMEA", "Is a <keep> suggestion: "); // likely outdated now that tags exist.
        } catch (JSONException e) {
          //e.printStackTrace();
          Log.v("KMEA", "JSON parsing error: " + e.getMessage());
        }
        */
      } else if (url.indexOf("reloadAfterError") >= 0) {
        InAppKeyboard.reloadAfterError();
      }
      return false;
    }
  }

  protected static final class KMSystemKeyboardWebViewClient extends WebViewClient {
    public static Context context;

    KMSystemKeyboardWebViewClient(Context context) {
      KMSystemKeyboardWebViewClient.context = context;
    }

    @Override
    public void onPageStarted(WebView view, String url, Bitmap favicon) {
    }

    @Override
    public void onPageFinished(WebView view, String url) {
      Log.d("KMEA", "onPageFinished: [system] " + url);
      shouldOverrideUrlLoading(view, url);
    }

    private void pageLoaded(WebView view, String url) {
      Log.d("KMEA", "pageLoaded: [system] " + url);
      if (SystemKeyboard == null) {
        KMLog.LogError(TAG, "pageLoaded and SystemKeyboard null");
        return;
      }

      SystemKeyboard.keyboardSet = false;
      currentLexicalModel = null;

      if (url.startsWith("file:")) { // TODO: is this test necessary?
        SystemKeyboardLoaded = true;

        SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
        if (index < 0) {
          index = 0;
        }
        Keyboard keyboardInfo = KMManager.getKeyboardInfo(context, index);
        String langId = null;
        if (keyboardInfo != null) {
          langId  = keyboardInfo.getLanguageID();
          SystemKeyboard.setKeyboard(keyboardInfo);
        } else {
          // Revert to default (index 0) or fallback keyboard
          keyboardInfo = KMManager.getKeyboardInfo(context, 0);
          if (keyboardInfo == null) {
            KMLog.LogError(TAG, "No keyboards installed. Reverting to fallback");
            keyboardInfo = KMManager.getDefaultKeyboard(context);
          }
          if (keyboardInfo != null) {
            langId = keyboardInfo.getLanguageID();
            SystemKeyboard.setKeyboard(keyboardInfo);
          }
        }

        registerAssociatedLexicalModel(langId);

        Handler handler = new Handler();
        handler.postDelayed(new Runnable() {
          @Override
          public void run() {
            SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
            if (prefs.getBoolean(KMManager.KMKey_ShouldShowHelpBubble, true)) {
              SystemKeyboard.loadJavascript("showHelpBubble()");
            }
          }
        }, 2000);

        KeyboardEventHandler.notifyListeners(KMTextView.kbEventListeners, KeyboardType.KEYBOARD_TYPE_SYSTEM, EventType.KEYBOARD_LOADED, null);

        SystemKeyboard.callJavascriptAfterLoad();
        SystemKeyboard.setSpacebarText(spacebarText);
      }
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
      if (SystemKeyboard == null) {
        KMLog.LogError(TAG, "shouldOverrideUrlLoading and SystemKeyboard null");
        return false;
      }

      // URL has actual path to the keyboard.html file as a prefix!  We need to replace
      // just the first intended '#' to get URI-based query param processing.

      // At some point, other parts of the function should be redone to allow use of ? instead
      // of # in our WebView command "queries" entirely.
      String cmd = url.replace("keyboard.html#", "keyboard.html?");
      Uri urlCommand = Uri.parse(cmd);
      if(url.indexOf("pageLoaded") >= 0) {
        pageLoaded(view, url);
      } else if (url.indexOf("hideKeyboard") >= 0) {
        SystemKeyboard.dismissHelpBubble();
        IMService.requestHideSelf(0);
      } else if (urlCommand.getQueryParameter("globeKeyAction") != null) {
        SystemKeyboard.dismissHelpBubble();
        if (!SystemKeyboard.isHelpBubbleEnabled) {
          return false;
        }

        SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(KMManager.KMKey_ShouldShowHelpBubble, false);
        editor.commit();

        handleGlobeKeyAction(context, urlCommand.getBooleanQueryParameter("keydown", false),
          KeyboardType.KEYBOARD_TYPE_SYSTEM);
      } else if (url.indexOf("showHelpBubble") >= 0) {
        int start = url.indexOf("keyPos=") + 7;
        String value = url.substring(start);
        if (!value.isEmpty()) {
          String[] globeKeyPos = value.split("\\,");
          float fx = Float.valueOf(globeKeyPos[0]);
          float fy = Float.valueOf(globeKeyPos[1]);
          SystemKeyboard.showHelpBubble(context, fx, fy);
        }
      } else if (url.indexOf("showKeyPreview") >= 0) {
        String deviceType = context.getString(R.string.device_type);
        if (deviceType.equals("AndroidTablet")) {
          return false;
        }

        if (SystemKeyboard.subKeysWindow != null) {
          return false;
        }

        int start = url.indexOf("x=") + 2;
        int end = url.indexOf("+y=");
        float x = Float.valueOf(url.substring(start, end));

        start = url.indexOf("y=") + 2;
        end = url.indexOf("+w=");
        float y = Float.valueOf(url.substring(start, end));

        start = url.indexOf("w=") + 2;
        end = url.indexOf("+h=");
        float w = Float.valueOf(url.substring(start, end));

        start = url.indexOf("h=") + 2;
        end = url.indexOf("+t=");
        float h = Float.valueOf(url.substring(start, end));

        start = url.indexOf("t=") + 2;
        String t = url.substring(start);
        String text = SystemKeyboard.convertKeyText(t);

        float left = x - w / 2.0f;
        float right = left + w;
        float top = y - 1;
        float bottom = top + h;

        RectF keyFrame = new RectF(left, top, right, bottom);
        SystemKeyboard.showKeyPreview(context, (int) x, (int) y, keyFrame, text);
      } else if (url.indexOf("dismissKeyPreview") >= 0) {
        SystemKeyboard.dismissKeyPreview(100);
      } else if (url.indexOf("showMore") >= 0) {
        if (SystemKeyboard.subKeysWindow != null && SystemKeyboard.subKeysWindow.isShowing()) {
          return false;
        }

        int start = url.indexOf("keyPos=") + 7;
        int end = url.indexOf("+keys=");
        SystemKeyboard.subKeysWindowPos = url.substring(start, end).split("\\,");

        start = end + 6;
        end = url.indexOf("+font=");
        if (end < 0) {
          end = url.length();
          SystemKeyboard.specialOskFont = "";
        } else {
          SystemKeyboard.specialOskFont = KMFilename_Osk_Ttf_Font;
        }

        String keys = url.substring(start, end);

        String[] keyList = keys.split("\\;");
        int klCount = keyList.length;
        SystemKeyboard.subKeysList = new ArrayList<HashMap<String, String>>();
        for (int i = 0; i < klCount; i++) {
          String[] values = keyList[i].split("\\:");
          String keyId = (values.length > 0) ? values[0] : "";
          String keyText = (values.length > 1) ? values[1] : "";

          HashMap<String, String> hashMap = new HashMap<String, String>();
          hashMap.put("keyId", keyId);
          hashMap.put("keyText", keyText);
          SystemKeyboard.subKeysList.add(hashMap);
        }
      } else if (url.indexOf("refreshBannerHeight") >= 0) {
        int start = url.indexOf("change=") + 7;
        String change = url.substring(start);
        boolean isModelActive = change.equals("active");
        SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        boolean modelPredictionPref = false;
        if(currentLexicalModel != null) {
          modelPredictionPref = prefs.getBoolean(getLanguagePredictionPreferenceKey(currentLexicalModel.get(KMManager.KMKey_LanguageID)), true);
        }
        currentBanner = (isModelActive && modelPredictionPref) ?
          KM_BANNER_STATE_SUGGESTION : KM_BANNER_STATE_BLANK;
        RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
        SystemKeyboard.setLayoutParams(params);
      } else if (url.indexOf("suggestPopup") >= 0) {
        double x = Float.parseFloat(urlCommand.getQueryParameter("x"));
        double y = Float.parseFloat(urlCommand.getQueryParameter("y"));
        double width = Float.parseFloat(urlCommand.getQueryParameter("w"));
        double height = Float.parseFloat(urlCommand.getQueryParameter("h"));
        String suggestionJSON = urlCommand.getQueryParameter("suggestion");

        JSONParser parser = new JSONParser();
        JSONObject obj = parser.getJSONObjectFromURIString(suggestionJSON);

        /*  // For future implementation
        SystemKeyboard.suggestionWindowPos = new double[]{x, y};
        SystemKeyboard.suggestionJSON = suggestionJSON;

        try {
          Log.v("KMEA", "Suggestion display: " + obj.getString("displayAs"));
          Log.v("KMEA", "Suggestion's banner coords: " + x + ", " + y + ", " + width + ", " + height);
          Log.v("KMEA", "Is a <keep> suggestion: "); // likely outdated now that tags exist.
        } catch (JSONException e) {
          //e.printStackTrace();
          Log.v("KMEA", "JSON parsing error: " + e.getMessage());
        }
        */
      } else if (url.indexOf("reloadAfterError") >= 0) {
        SystemKeyboard.reloadAfterError();
      }
      return false;
    }
  }

  private static final class KMInAppKeyboardJSHandler extends KMKeyboardJSHandler {

    KMInAppKeyboardJSHandler(Context context, KMKeyboard k) {
      super(context, k);
    }
    private static final String HANDLER_TAG = "IAK: JS Handler";

    @JavascriptInterface
    public boolean dispatchKey(final int code, final int eventModifiers) {
      Handler mainLoop = new Handler(Looper.getMainLooper());
      mainLoop.post(new Runnable() {
        public void run() {
          if (InAppKeyboard == null) {
            KMLog.LogError(TAG, "dispatchKey failed: InAppKeyboard is null");
            return;
          }

          if (InAppKeyboard.subKeysWindow != null || KMTextView.activeView == null || KMTextView.activeView.getClass() != KMTextView.class) {
            if ((KMTextView.activeView == null) && isDebugMode()) {
              Log.w(HANDLER_TAG, "dispatchKey failed: activeView is null");
            }
            return;
          }

          // Handle tab or enter since KMW didn't process it
          KMTextView textView = (KMTextView) KMTextView.activeView;
          if (code == KMScanCodeMap.scanCodeMap[KMScanCodeMap.KEY_TAB]) {
            KeyEvent event = new KeyEvent(0, 0, 0, KeyEvent.KEYCODE_TAB, 0, eventModifiers, 0, 0, 0);
            textView.dispatchKeyEvent(event);
          } else if (code == KMScanCodeMap.scanCodeMap[KMScanCodeMap.KEY_ENTER]) {
            KeyEvent event = new KeyEvent(0, 0, 0, KeyEvent.KEYCODE_ENTER, 0, eventModifiers, 0, 0, 0);
            textView.dispatchKeyEvent(event);
          }
        }
      });
      return true;
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public void insertText(final int dn, final String s, final int dr) {
      if(dr != 0) {
        Log.d(TAG, "Right deletions requested but are not presently supported by the in-app keyboard.");
      }

      Handler mainLoop = new Handler(Looper.getMainLooper());
      mainLoop.post(new Runnable() {
        public void run() {
          if (InAppKeyboard == null) {
            KMLog.LogError(TAG, "insertText failed: InAppKeyboard is null");
            return;
          }

          if (InAppKeyboard.subKeysWindow != null || KMTextView.activeView == null || KMTextView.activeView.getClass() != KMTextView.class) {
            if ((KMTextView.activeView == null) && isDebugMode()) {
              Log.w("IAK: JS Handler", "insertText failed: activeView is null");
            }
            return;
          }

          InAppKeyboard.dismissHelpBubble();

          KMTextView textView = (KMTextView) KMTextView.activeView;
          textView.beginBatchEdit();

          int start = textView.getSelectionStart();
          int end = textView.getSelectionEnd();
          // Workaround for Android TextView bug where end < start
          // Reference: https://issuetracker.google.com/issues/36911048
          if (end < start) {
            Log.d(TAG, "Swapping TextView selection end:" + end + " and start:" + start);
            int temp = end;
            end = start;
            start = temp;
          }

          if (dn <= 0) {
            if (start == end) {
              if (s.length() > 0 && s.charAt(0) == '\n') {
                textView.keyDownUp(KeyEvent.KEYCODE_ENTER);
              } else {
                // *** TO DO: Try to find a solution to the bug on API < 17, insert overwrites on next line
                if (s.length() > 0) {
                  InAppKeyboardShouldIgnoreTextChange = true;
                  InAppKeyboardShouldIgnoreSelectionChange = true;
                  textView.getText().insert(start, s);
                }
              }
            } else {
              if (s.length() > 0 && s.charAt(0) == '\n') {
                InAppKeyboardShouldIgnoreTextChange = true;
                InAppKeyboardShouldIgnoreSelectionChange = true;
                textView.getText().replace(start, end, "");
                textView.keyDownUp(KeyEvent.KEYCODE_ENTER);
              } else {
                if (s.length() == 0) {
                  textView.getText().delete(start, end);
                } else {
                  InAppKeyboardShouldIgnoreTextChange = true;
                  InAppKeyboardShouldIgnoreSelectionChange = true;
                  textView.getText().replace(start, end, s);
                }
              }
            }
          } else {
            for (int i = 0; i < dn; i++) {
              CharSequence chars = textView.getText().subSequence(0, start);
              if (chars != null && chars.length() > 0) {
                char c = chars.charAt(start - 1);
                InAppKeyboardShouldIgnoreTextChange = true;
                InAppKeyboardShouldIgnoreSelectionChange = true;
                if (Character.isLowSurrogate(c)) {
                  textView.getText().delete(start - 2, end);
                } else {
                  textView.getText().delete(start - 1, end);
                }

                start = textView.getSelectionStart();
                end = textView.getSelectionEnd();
              }
            }

            if (s.length() > 0) {
              InAppKeyboardShouldIgnoreTextChange = true;
              InAppKeyboardShouldIgnoreSelectionChange = true;
              textView.getText().insert(start, s);
            }
          }

          textView.endBatchEdit();
        }
      });
    }
  }

  private static final class KMSystemKeyboardJSHandler extends KMKeyboardJSHandler {
    KMSystemKeyboardJSHandler(Context context, KMKeyboard k) {
      super(context, k);
    }
    private static final String HANDLER_TAG = "SWK: JS Handler";

    @JavascriptInterface
    public boolean dispatchKey(final int code, final int eventModifiers) {
      Handler mainLoop = new Handler(Looper.getMainLooper());
      mainLoop.post(new Runnable() {
        public void run() {
          if (SystemKeyboard == null) {
            KMLog.LogError(TAG, "dispatchKey failed: SystemKeyboard is null");
            return;
          }

          if (SystemKeyboard.subKeysWindow != null) {
            return;
          }

          InputConnection ic = IMService.getCurrentInputConnection();
          if (ic == null) {
            if (isDebugMode()) {
              Log.w(HANDLER_TAG, "insertText failed: InputConnection is null");
            }
            return;
          }

          SystemKeyboard.dismissHelpBubble();

          // Handle tab or enter since KMW didn't process it
          Log.d(HANDLER_TAG, "dispatchKey called with code: " + code + ", eventModifiers: " + eventModifiers);
          if (code == KMScanCodeMap.scanCodeMap[KMScanCodeMap.KEY_TAB]) {
            KeyEvent event = new KeyEvent(0, 0, 0, KeyEvent.KEYCODE_TAB, 0, eventModifiers, 0, 0, 0);
            ic.sendKeyEvent(event);
          } else if (code == KMScanCodeMap.scanCodeMap[KMScanCodeMap.KEY_ENTER]) {
            KeyEvent event = new KeyEvent(0, 0, 0, KeyEvent.KEYCODE_ENTER, 0, eventModifiers, 0, 0, 0);
            ic.sendKeyEvent(event);
          }
        }
      });
      return true;
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public void insertText(final int dn, final String s, final int dr) {
      Handler mainLoop = new Handler(Looper.getMainLooper());
      mainLoop.post(new Runnable() {
        public void run() {
          if (SystemKeyboard == null) {
            KMLog.LogError(TAG, "insertText failed: SystemKeyboard is null");
            return;
          }

          if (SystemKeyboard.subKeysWindow != null) {
            return;
          }

          InputConnection ic = IMService.getCurrentInputConnection();
          if (ic == null) {
            if (isDebugMode()) {
              Log.w(HANDLER_TAG, "insertText failed: InputConnection is null");
            }
            return;
          }

          SystemKeyboard.dismissHelpBubble();

          ic.beginBatchEdit();

          // Delete any existing selected text.
          ExtractedText icText = ic.getExtractedText(new ExtractedTextRequest(), 0);
          if (icText != null) { // This can be null if the input connection becomes invalid.
            int start = icText.startOffset + icText.selectionStart;
            int end = icText.startOffset + icText.selectionEnd;
            if (end > start) {
              if (s.length() == 0) {
                ic.setSelection(start, start);
                ic.deleteSurroundingText(0, end - start);
                ic.endBatchEdit();
                return;
              } else {
                SystemKeyboardShouldIgnoreSelectionChange = true;
                ic.setSelection(start, start);
                ic.deleteSurroundingText(0, end - start);
              }
            }
          }

          if (s.length() > 0 && s.charAt(0) == '\n') {
            keyDownUp(KeyEvent.KEYCODE_ENTER);
            ic.endBatchEdit();
            return;
          }

          // Perform left-deletions
          if (dn > 0) {
            performLeftDeletions(ic, dn);
          }

          // Perform right-deletions
          for (int i = 0; i < dr; i++) {
            CharSequence chars = ic.getTextAfterCursor(1, 0);
            if (chars != null && chars.length() > 0) {
              char c = chars.charAt(0);
              SystemKeyboardShouldIgnoreSelectionChange = true;
              if (Character.isHighSurrogate(c)) {
                ic.deleteSurroundingText(0, 2);
              } else {
                ic.deleteSurroundingText(0, 1);
              }
            }
          }

          if (s.length() > 0) {
            SystemKeyboardShouldIgnoreSelectionChange = true;

            // Commit the string s. Use newCursorPosition 1 so cursor will end up after the string.
            ic.commitText(s, 1);
          }

          ic.endBatchEdit();
        }
      });
    }

    private void keyDownUp(int keyEventCode) {
      IMService.getCurrentInputConnection().sendKeyEvent(new KeyEvent(KeyEvent.ACTION_DOWN, keyEventCode));
      IMService.getCurrentInputConnection().sendKeyEvent(new KeyEvent(KeyEvent.ACTION_UP, keyEventCode));
    }

    /*
    // TODO: Chromium has a bug where deleteSurroundingText deletes an entire grapheme cluster
    // instead of one code-point. See Chromium issue #1024738
    // https://bugs.chromium.org/p/chromium/issues/detail?id=1024738
    //
    // We'll retrieve up to (dn*2+16) characters before the cursor to collect enough characters
    // for surrogate pairs + a long grapheme cluster.
    // This buffer will be used to put back characters as-needed
    */
    private static void performLeftDeletions(InputConnection ic, int dn) {
      int originalBufferLength = dn*2 + 16; // characters
      CharSequence charsBackup = getCharacterSequence(ic, originalBufferLength);

      int lastIndex = charsBackup.length()-1;

      // Exit if there's no context to delete
      if (lastIndex < 0) {
        return;
      }

      int numPairs = CharSequenceUtil.countSurrogatePairs(charsBackup, dn);

      // Chop dn+numPairs code points from the end of charsBackup
      // subSequence indices are start(inclusive) to end(exclusive)
      CharSequence expectedChars = charsBackup.subSequence(0, charsBackup.length() - (dn + numPairs));
      ic.deleteSurroundingText(dn + numPairs, 0);
      CharSequence newContext = getCharacterSequence(ic, originalBufferLength - 2*dn);

      CharSequence charsToRestore = CharSequenceUtil.restoreChars(expectedChars, newContext);
      if (charsToRestore.length() > 0) {
        // Restore expectedChars that Chromium deleted.
        // Use newCusorPosition 1 so cursor will be after the inserted string
        ic.commitText(charsToRestore, 1);
      }
    }

    /**
     * Get a character sequence from the InputConnection.
     * Sometimes the WebView can split a surrogate pair at either end,
     * so chop that and update the cursor
     * @param ic - the InputConnection
     * @param length - number of characters to get
     * @return CharSequence
     */
    private static CharSequence getCharacterSequence(InputConnection ic, int length) {
      if (ic == null || length <= 0) {
        return "";
      }

      CharSequence sequence = ic.getTextBeforeCursor(length, 0);
      if (sequence == null || sequence.length() <= 0) {
        return "";
      }

      // Move the cursor back if there's a split surrogate pair
      if (Character.isHighSurrogate(sequence.charAt(sequence.length()-1))) {
        ic.commitText("", -1);
        sequence = ic.getTextBeforeCursor(length, 0);
      }

      if (sequence == null || sequence.length() <= 0) {
        return "";
      }

      if (Character.isLowSurrogate(sequence.charAt(0))) {
        // Adjust if the first char is also a split surrogate pair
        // subSequence indices are start(inclusive) to end(exclusive)
        sequence = sequence.subSequence(1, sequence.length());
      }

      return sequence;
    }
  }
}
