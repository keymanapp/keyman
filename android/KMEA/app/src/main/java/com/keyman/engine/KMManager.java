/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.keyman.engine;

import java.io.File;
import java.io.FileFilter;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
import android.graphics.Point;
import android.graphics.Typeface;
import android.inputmethodservice.InputMethodService;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Build;
import android.os.IBinder;
import android.text.InputType;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.View;
import android.view.Display;
import android.view.Surface;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.WindowMetrics;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.view.inputmethod.InputMethodManager;
import android.webkit.WebView;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import androidx.core.content.ContextCompat;

import io.sentry.Breadcrumb;
import io.sentry.Sentry;
import io.sentry.SentryLevel;

import com.keyman.engine.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.keyman.engine.KeyboardEventHandler.OnKeyboardEventListener;
import com.keyman.engine.cloud.CloudDownloadMgr;
import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.logic.ResourcesUpdateTool;
import com.keyman.engine.packages.JSONUtils;
import com.keyman.engine.packages.LexicalModelPackageProcessor;
import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.BCP47;
import com.keyman.engine.util.DependencyUtil;
import com.keyman.engine.util.DependencyUtil.LibraryType;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.MapCompat;
import com.keyman.engine.util.WebViewUtils;

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
    // GLOBE_KEY_ACTION_SWITCH_TO_PREVIOUS_KEYBOARD,      // Switch to previous Keyman keyboard (reserved for flick)
    GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD,             // Switch to next Keyman keyboard
    GLOBE_KEY_ACTION_ADVANCE_TO_PREVIOUS_SYSTEM_KEYBOARD, // Advance to previous system keyboard
    GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD,     // Advance to next system keyboard
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

  // Maps to enum BannerType in bannerView.ts
  public enum BannerType {
    BLANK,
    IMAGE,
    SUGGESTION,
    HTML;

    public static BannerType fromString(String mode) {
      if (mode == null) return BLANK;
      switch (mode) {
        case "BLANK":
          return BLANK;
        case "image":
          return IMAGE;
        case "suggestion":
          return SUGGESTION;
        case "html":
          return HTML;
      }
      return BLANK;
    }

    public String toString() {
      String modes[] = { "blank", "image", "suggestion", "html"};
      return modes[this.ordinal()];
    }
  }

  // Enum for how the System Keyboard ENTER key is handled for the EditorInfo action
  // Reference: https://developer.android.com/reference/android/view/inputmethod/EditorInfo#summary
  public enum EnterModeType {
    GO,        // Go action
    SEARCH,    // Search action
    SEND,      // Send action
    NEXT,      // Next action
    DONE,      // Done action
    PREVIOUS,  // Previous action
    NEWLINE,   // Send newline character
    DEFAULT,   // Default ENTER action
  }

  // Enum for whether the suggestion banner allows predictions, corrections, auto-corrections
  public enum SuggestionType {
    // Suggestion Disabled - No Predictions, No corrections, No auto-corrections
    SUGGESTIONS_DISABLED,

    // Suggestions Enabled
    PREDICTIONS_ONLY,              // Predictions with no corrections
    PREDICTIONS_WITH_CORRECTIONS,  // Predictions with corrections
    PREDICTIONS_WITH_AUTO_CORRECT; // Predictions with auto-corrections

    public static SuggestionType fromInt(int mode) {
      switch (mode) {
        case 0:
          return SUGGESTIONS_DISABLED;
        case 1:
          return PREDICTIONS_ONLY;
        case 2:
          return PREDICTIONS_WITH_CORRECTIONS;
        case 3:
          return PREDICTIONS_WITH_AUTO_CORRECT;
      }
      return SUGGESTIONS_DISABLED;
    }

    public int toInt() {
      return this.ordinal();
    }
  }

  protected static InputMethodService IMService;

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

  protected static KMKeyboard InAppKeyboard = null;
  protected static KMKeyboard SystemKeyboard = null;
  protected static KMKeyboardWebViewClient InAppKeyboardWebViewClient = null;
  protected static KMKeyboardWebViewClient SystemKeyboardWebViewClient = null;
  protected static HashMap<String, String> currentLexicalModel = null;
  protected static WebViewUtils.EngineWebViewVersionStatus engineWebViewVersionStatus =
    WebViewUtils.EngineWebViewVersionStatus.UNDETERMINED;

  public final static String predictionPrefSuffix = ".mayPredict";
  public final static String correctionPrefSuffix = ".mayCorrect";
  public final static String autoCorrectionPrefSuffix = ".mayAutoCorect";

  // Special override for when the keyboard may have haptic feedback when typing.
  // haptic feedback disabled for hardware keystrokes
  private static boolean mayHaveHapticFeedback = false;

  // Special override for when keyboard is entering a password text field.
  // When mayPredictOverride is true, the option {'mayPredict' = false} is set in the lm-layer
  // regardless what the Settings preference is.
  private static boolean mayPredictOverride = false;

  // Determine how system keyboard handles ENTER key
  public static EnterModeType enterMode = EnterModeType.DEFAULT;

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
  public static final String KMKey_KMPInstall_Mode = "kmpInstallMode";
  public static final String KMKey_KeyboardModified = "lastModified";
  public static final String KMKey_KeyboardRTL = "rtl";
  public static final String KMKey_KeyboardHeightPortrait = "keyboardHeightPortrait";
  public static final String KMKey_KeyboardHeightLandscape = "keyboardHeightLandscape";

  public static final String KMKey_LongpressDelay = "longpressDelay";

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

  // Default KeymanWeb longpress delay constants in milliseconds
  public static final int KMDefault_LongpressDelay = 500;
  public static final int KMMinimum_LongpressDelay = 300;
  public static final int KMMaximum_LongpressDelay = 1500;

  // Default prediction/correction setting
  public static final int KMDefault_Suggestion = SuggestionType.PREDICTIONS_WITH_CORRECTIONS.toInt();

  // Keyman files
  protected static final String KMFilename_KeyboardHtml = "keyboard.html";
  protected static final String KMFilename_JSEngine = "keymanweb-webview.js";
  protected static final String KMFilename_JSSentry = "sentry.min.js";
  protected static final String KMFilename_JSSentryInit = "keyman-sentry.js";
  protected static final String KMFilename_AndroidHost = "android-host.js";
  protected static final String KMFilename_KmwCss = "kmwosk.css";
  protected static final String KMFilename_KmwGlobeHintCss = "globe-hint.css";
  protected static final String KMFilename_Osk_Ttf_Font = "keymanweb-osk.ttf";
  protected static final String KMFilename_JSPolyfill = "es6-shim.min.js";
  protected static final String KMFilename_JSPolyfill2 = "other-polyfills.js";
  protected static final String KMFilename_JSPolyfill3 = "map-polyfill.js";

  // Deprecated by KeyboardController.KMFilename_Installed_KeyboardsList
  public static final String KMFilename_KeyboardsList = "keyboards_list.dat";

  public static final String KMFilename_LexicalModelsList = "lexical_models_list.dat";

  public static final String KMBLACK_BANNER = "<div style=\"background: black; width: 100%; height: 100%; position: absolute; left: 0; top: 0\"></div>";
  public static final String KMGRAY_BANNER = "<div style=\"background: #b4b4b8; width: 100%; height: 100%; position: absolute; left: 0; top: 0\"></div>";

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
   *                    com.keyman.engine.BuildConfig.KEYMAN_ENGINE_VERSION_NAME
   * @return Tier (ALPHA, BETA, STABLE)
   */
  public static Tier getTier(String versionName) {
    if (versionName == null || versionName.isEmpty()) {
      versionName = com.keyman.engine.BuildConfig.KEYMAN_ENGINE_VERSION_NAME;
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
    String appVersion = com.keyman.engine.BuildConfig.KEYMAN_ENGINE_VERSION_NAME;
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
    String appVersion = com.keyman.engine.BuildConfig.KEYMAN_ENGINE_VERSION_NAME;
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

    if (keyboardType == KeyboardType.KEYBOARD_TYPE_UNDEFINED) {
      String msg = "Cannot initialize: Invalid keyboard type";
      KMLog.LogError(TAG, msg);
    } else {
      initKeyboard(appContext, keyboardType);
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

  /**
   * Get the input connection based on the keyboard type.
   * @param {KeyboardType} keyboard
   * @return InputConnection
   */
  protected static InputConnection getInputConnection(KeyboardType keyboard) {
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP) {
      return KMTextView.activeView.onCreateInputConnection(new EditorInfo());
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM && IMService != null) {
      return IMService.getCurrentInputConnection();
    }

    KMLog.LogError(TAG, "Unable to determine input connection");
    return null;
  }

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
      // Override when keyboard switch and only 1 keyboard installed
      if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP) {
        // In-app keyboard shows keyboard menu
        action = GlobeKeyAction.GLOBE_KEY_ACTION_SHOW_MENU;
      } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
        // System keyboard switches back to previous system keyboard (Issue #6175)
        action = GlobeKeyAction.GLOBE_KEY_ACTION_ADVANCE_TO_PREVIOUS_SYSTEM_KEYBOARD;
      }
    }

    switch (action) {
      case GLOBE_KEY_ACTION_SHOW_MENU:
        showKeyboardPicker(context, keyboard);
        break;
      case GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD:
        switchToNextKeyboard(context);
        break;
      case GLOBE_KEY_ACTION_ADVANCE_TO_PREVIOUS_SYSTEM_KEYBOARD:
        // Only do this for system keyboard
        if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
          advanceToPreviousInputMethod();
        }
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

  private static void initKeyboard(Context appContext, KeyboardType keyboardType) {
    KMKeyboard keyboard = null;
    KMKeyboardWebViewClient webViewClient = null;

    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP && InAppKeyboard == null) {
      InAppKeyboard = new KMKeyboard(appContext, KeyboardType.KEYBOARD_TYPE_INAPP);
      InAppKeyboardWebViewClient = new KMKeyboardWebViewClient(appContext, keyboardType);
      keyboard = InAppKeyboard;
      webViewClient = InAppKeyboardWebViewClient;
    } else if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM && SystemKeyboard == null) {
      SystemKeyboard = new KMKeyboard(appContext, KeyboardType.KEYBOARD_TYPE_SYSTEM);
      SystemKeyboardWebViewClient = new KMKeyboardWebViewClient(appContext, keyboardType);
      keyboard = SystemKeyboard;
      webViewClient = SystemKeyboardWebViewClient;
    }

    if (keyboard == null) {
      return;
    }

    if (!isTestMode()) {
      // Keyboard layout not needed in unit tests. #5125
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      keyboard.setLayoutParams(params);
    }
    keyboard.setVerticalScrollBarEnabled(false);
    keyboard.setHorizontalScrollBarEnabled(false);
    keyboard.setWebViewClient(webViewClient);
    keyboard.addJavascriptInterface(new KMKeyboardJSHandler(appContext, keyboard), "jsInterface");
    keyboard.loadKeyboard();

    if (!isTestMode()) {
      // For apps that don't specify an HTML banner, specify a default phone/tablet HTML banner
      if (getFormFactor() == FormFactor.PHONE) {
        keyboard.setHTMLBanner(KMBLACK_BANNER);
      } else {
        keyboard.setHTMLBanner(KMGRAY_BANNER);
      }
      keyboard.setBanner(KMManager.BannerType.HTML);
      keyboard.showBanner(true);
    }
    setEngineWebViewVersionStatus(appContext, keyboard);
  }

  public static String getLanguagePredictionPreferenceKey(String langID) {
    return langID + predictionPrefSuffix;
  }

  public static String getLanguageCorrectionPreferenceKey(String langID) {
    return langID + correctionPrefSuffix;
  }

  public static String getLanguageAutoCorrectionPreferenceKey(String langID) {
    return langID + autoCorrectionPrefSuffix;
  }

  public static void hideSystemKeyboard() {
    if (SystemKeyboard != null) {
      SystemKeyboard.hideKeyboard();
    }
  }

  public static void showSystemKeyboard() {
    if (SystemKeyboard != null) {
      SystemKeyboard.showKeyboard();
    }
  }

  public static boolean isKeyboardLoaded(KeyboardType type) {
    if (type == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard != null && InAppKeyboardWebViewClient != null) {
        return InAppKeyboardWebViewClient.getKeyboardLoaded();
      }
      return false;
    } else if (type == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard != null && SystemKeyboardWebViewClient != null) {
        return SystemKeyboardWebViewClient.getKeyboardLoaded();
      }
      return false;
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
      InAppKeyboard.onConfigurationChanged(newConfig);
    }
    if (SystemKeyboard != null) {
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      SystemKeyboard.onConfigurationChanged(newConfig);
    }
  }

  /**
   * Query the AndroidManifest file to see if a permission is granted.
   * @param permission - The manifest permission to query
   * @return boolean - true if the manifest permission is granted
    */
  public static boolean hasPermission(Context context, String permission) {
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

  /**
   * Copy HTML banner assets to the app
   * @param context - The context
   * @param path - Folder relative to assets/ containing the banner file.
   * @return boolean - true if assets copied
   */
  public static boolean copyHTMLBannerAssets(Context context, String path) {
    AssetManager assetManager = context.getAssets();
    try {
      File bannerDir = new File(getResourceRoot() + File.separator + path);
      if (!bannerDir.exists()) {
        bannerDir.mkdir();
      }

      String[] bannerFiles = assetManager.list(path);
      for (String bannerFile : bannerFiles) {
        copyAsset(context, bannerFile, path, true);
      }
      return true;
    } catch (Exception e) {
      KMLog.LogException(TAG, "copyHTMLBannerAssets() failed. Error: ", e);
    }
    return false;
  }

  private static void copyAssets(Context context) {
    AssetManager assetManager = context.getAssets();

    try {
      // Copy KMW files
      copyAsset(context, KMFilename_KeyboardHtml, "", true);

      copyAsset(context, KMFilename_JSEngine, "", true);
      copyAsset(context, KMFilename_JSSentry, "", true);
      copyAsset(context, KMFilename_JSSentryInit, "", true);
      copyAsset(context, KMFilename_AndroidHost, "", true);
      copyAsset(context, KMFilename_KmwCss, "", true);
      copyAsset(context, KMFilename_KmwGlobeHintCss, "", true);
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
    return copyAssetWithRename(context, filename, filename, directory, overwrite);
  }

  private static int copyAssetWithRename(Context context, String srcName, String destName, String directory, boolean overwrite) {
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

      File file = new File(dirPath, destName);
      if (!file.exists() || overwrite) {
        InputStream inputStream = assetManager.open(directory + srcName);
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
   * Sets enterMode which specifies how the System keyboard ENTER key is handled
   *
   * @param imeOptions EditorInfo.imeOptions used to determine the action
   * @param inputType  InputType used to determine if the text field is multi-line
   */
  public static void setEnterMode(int imeOptions, int inputType) {
    EnterModeType value = EnterModeType.DEFAULT;
    int imeActions = imeOptions & EditorInfo.IME_MASK_ACTION;
    boolean isMultiLine = (inputType & InputType.TYPE_TEXT_FLAG_MULTI_LINE) != 0;

    switch (imeActions) {
      case EditorInfo.IME_ACTION_GO:
        value = EnterModeType.GO;
        break;

      case EditorInfo.IME_ACTION_SEARCH:
        value = EnterModeType.SEARCH;
        break;

      case EditorInfo.IME_ACTION_SEND:
        value = isMultiLine ?
          EnterModeType.NEWLINE :EnterModeType.SEND;
        break;

      case EditorInfo.IME_ACTION_NEXT:
        value = EnterModeType.NEXT;
        break;

      case EditorInfo.IME_ACTION_DONE:
        value = isMultiLine ?
          EnterModeType.NEWLINE : EnterModeType.DONE;
        break;

      case EditorInfo.IME_ACTION_PREVIOUS:
        value = EnterModeType.PREVIOUS;
        break;

      default:
        value = isMultiLine ?
          EnterModeType.NEWLINE : EnterModeType.DEFAULT;
    }

    enterMode = value;
  }

  /**
   * Get the value of enterMode
   * @return EnterModeType
   */
  public static EnterModeType getEnterMode() {
    return enterMode;
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
   * Store SuggestionType as int in preference
   * @param languageID as String
   * @param suggestType SuggestionType
   */
  public static void setMaySuggest(String languageID, SuggestionType suggestType) {
    SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = prefs.edit();
    editor.putInt(KMManager.getLanguageAutoCorrectionPreferenceKey(languageID), suggestType.toInt());
    editor.commit();
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
   * If the override is true, vibrate when user types on the Keyman keyboard
   * @param override - boolean
   */
  public static void setHapticFeedback(boolean override) {
    mayHaveHapticFeedback = override;
  }

  /**
   * Get the value of mayHaveHapticFeedback. Default is false
   * @return boolean
   */
  public static boolean getHapticFeedback() { return mayHaveHapticFeedback; };

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

    // When entering password field, maySuggest should override to disabled
    SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    int maySuggest = mayPredictOverride ? SuggestionType.SUGGESTIONS_DISABLED.toInt() :
      prefs.getInt(getLanguageAutoCorrectionPreferenceKey(languageID), KMDefault_Suggestion);

    RelativeLayout.LayoutParams params;
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP) && !InAppKeyboard.shouldIgnoreTextChange() && modelFileExists) {
      params = getKeyboardLayoutParams();

      // Do NOT re-layout here; it'll be triggered once the banner loads.
      InAppKeyboard.loadJavascript(KMString.format("enableSuggestions(%s, %d)", model, maySuggest));
    }
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM) && !SystemKeyboard.shouldIgnoreTextChange() && modelFileExists) {
      params = getKeyboardLayoutParams();

      // Do NOT re-layout here; it'll be triggered once the banner loads.
      SystemKeyboard.loadJavascript(KMString.format("enableSuggestions(%s, %d)", model, maySuggest));
    }
    return true;
  }

  public static boolean deregisterLexicalModel(String modelID) {
    // Check if current lexical model needs to be cleared
    if (currentLexicalModel != null && currentLexicalModel.get(KMManager.KMKey_LexicalModelID).equalsIgnoreCase(modelID)) {
      currentLexicalModel = null;
    }

    String url = KMString.format("deregisterModel('%s')", modelID);
    if (InAppKeyboard != null) {
      InAppKeyboard.loadJavascript(url);
    }

    if (SystemKeyboard != null) {
      SystemKeyboard.loadJavascript(url);
    }
    return true;
  }

  /**
   * deleteLexicalModel - Remove lexical model from the installed list
   * and deregister the model with KMW
   * @param context
   * @param position - int position in the models list
   */
  public static void deleteLexicalModel(Context context, int position, boolean silenceNotification) {
    KeyboardPickerActivity.deleteLexicalModel(context, position, silenceNotification);
  }

  /**
   * setBannerOptions - Update KMW whether to generate predictions.
   *                    For now, also display banner
   * @param mayPredict - boolean whether KMW should generate predictions
   * @return boolean - Success
   */
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

  /**
   * Update KeymanWeb banner type
   * @param {KeyboardType} keyboard
   * @param {BannerType} bannerType
   * @return status
   */
  public static boolean setBanner(KeyboardType keyboard, BannerType bannerType) {
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP && InAppKeyboard != null) {
      InAppKeyboard.setBanner(bannerType);
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM && SystemKeyboard != null) {
      SystemKeyboard.setBanner(bannerType);
    } else {
      return false;
    }
    return true;
  }

  /**
   * Set the HTML content to use with the HTML banner
   * @param {KeyboardType} keyboard
   * @param {String} HTMl string
   * @return {boolean}
   */
  public static boolean setHTMLBanner(KeyboardType keyboard, String htmlContent) {
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP && InAppKeyboard != null) {
      InAppKeyboard.setHTMLBanner(htmlContent);
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM && SystemKeyboard != null) {
      SystemKeyboard.setHTMLBanner(htmlContent);
    } else {
      Log.d(TAG, "setHTMLBanner() but keyboard is null");
      return false;
    }
    return true;
  }

  /**
   * Get the HTML content associated with the HTML banner
   * @param {KeyboardType} keyboard
   * @return {String}
   */
  public static String getHTMLBanner(KeyboardType keyboard) {
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP && InAppKeyboard != null) {
      return InAppKeyboard.getHTMLBanner();
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM && SystemKeyboard != null) {
      return SystemKeyboard.getHTMLBanner();
    }
    return "";
  }

  /**
   * showBanner - Update KMW whether to display banner.
   *              For now, always keep displaying banner
   * @param flag - boolean whether KMW should display banner
   * @return boolean - Success
   */
  public static boolean showBanner(boolean flag) {
    if (InAppKeyboard != null) {
      InAppKeyboard.showBanner(flag);
    }

    if (SystemKeyboard != null) {
      SystemKeyboard.showBanner(flag);
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
    if (DependencyUtil.libraryExists(LibraryType.SENTRY) && Sentry.isEnabled() && !(packageID.equalsIgnoreCase(KMManager.KMDefault_PackageID) &&
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

  public static boolean isDefaultKey(String key) {
    return (
      key != null &&
      key.equals(KMString.format("%s_%s", KMDefault_LanguageID, KMDefault_KeyboardID)));
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

    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP)) {
      result1 = InAppKeyboard.setKeyboard(packageID, keyboardID, languageID);
    }

    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM)) {
      result2 = SystemKeyboard.setKeyboard(packageID, keyboardID, languageID);
    }

    return (result1 || result2);
  }

  public static boolean setKeyboard(Keyboard keyboardInfo) {
    boolean result1 = false;
    boolean result2 = false;
    HashMap<String, String> associatedLexicalModel = null;
    String languageID = null;

    if (keyboardInfo != null) {
      languageID = keyboardInfo.getLanguageID();
      associatedLexicalModel = getAssociatedLexicalModel(languageID);
    }

    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP) && keyboardInfo != null) {
      result1 = InAppKeyboard.setKeyboard(keyboardInfo);
      InAppKeyboard.toggleSuggestionBanner(associatedLexicalModel, result1);
    }

    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM) && keyboardInfo != null) {
      result2 = SystemKeyboard.setKeyboard(keyboardInfo);
      SystemKeyboard.toggleSuggestionBanner(associatedLexicalModel, result2);
    }

    registerAssociatedLexicalModel(languageID);

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

    HashMap<String, String> associatedLexicalModel = getAssociatedLexicalModel(languageID);
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP)) {
      result1 = InAppKeyboard.prepareKeyboardSwitch(packageID, keyboardID, languageID,keyboardName);
      InAppKeyboard.toggleSuggestionBanner(associatedLexicalModel, result1);
    }
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM)) {
      result2 = SystemKeyboard.prepareKeyboardSwitch(packageID, keyboardID, languageID,keyboardName);
      SystemKeyboard.toggleSuggestionBanner(associatedLexicalModel, result2);
    }

    registerAssociatedLexicalModel(languageID);

    return (result1 || result2);
  }

  public static boolean setKeyboard(String packageID, String keyboardID, String languageID, String keyboardName, String languageName, String kFont, String kOskFont) {
    boolean result1 = false;
    boolean result2 = false;

    if (InAppKeyboard != null && (InAppKeyboardWebViewClient.getKeyboardLoaded() || isTestMode())) {
      result1 = InAppKeyboard.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);
    }

    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM)) {
      result2 = SystemKeyboard.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);
    }

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

  public static void clearKeyboardCache() {
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP)) {
      InAppKeyboard.clearCache(true);
      InAppKeyboard.loadKeyboard();
    }
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM)) {
      SystemKeyboard.clearCache(true);
      SystemKeyboard.loadKeyboard();
    }
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

  public static void advanceToPreviousInputMethod() {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      if (IMService != null) {
        IMService.switchToPreviousInputMethod();
      }
    } else {
      // This method is added in API level 16 and deprecated in API level 28
      // Reference: https://developer.android.com/reference/android/view/inputmethod/InputMethodManager#switchToLastInputMethod(android.os.IBinder)
      InputMethodManager imm = (InputMethodManager) appContext.getSystemService(Context.INPUT_METHOD_SERVICE);
      imm.switchToLastInputMethod(getToken());
    }
  }

  public static void advanceToNextInputMode() {
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.P) {
      if (IMService != null) {
        IMService.switchToNextInputMethod(false);
      }
    } else {
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

  public static int getOrientation(Context context) {
    Display display;
    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
      // https://developer.android.com/reference/android/content/Context#getDisplay()
      try {
        display = context.getDisplay();
      } catch (UnsupportedOperationException e) {
        // if the method is called on an instance that is not associated with any display.
        return context.getResources().getConfiguration().orientation;
      }
    } else {
      WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
      // Deprecated in API 30
      display = wm.getDefaultDisplay();
    }
    int rotation = display.getRotation();
    if (rotation == Surface.ROTATION_0 || rotation == Surface.ROTATION_180) {
      return Configuration.ORIENTATION_PORTRAIT;
    } else if (rotation == Surface.ROTATION_90 || rotation == Surface.ROTATION_270) {
      return Configuration.ORIENTATION_LANDSCAPE;
    }
    return Configuration.ORIENTATION_UNDEFINED;
  }

  /**
   * Get the longpress delay (in milliseconds) from stored preference. Defaults to 500ms
   * @return int - longpress delay in milliseconds
   */
  public static int getLongpressDelay() {
    SharedPreferences prefs = appContext.getSharedPreferences(
      appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);

    return prefs.getInt(KMKey_LongpressDelay, KMDefault_LongpressDelay);
  }

  /**
   * Set the longpress delay (in milliseconds) as a stored preference.
   * Valid range is 300 ms to 1500 ms. Returns true if the preference is successfully stored.
   * @param longpressDelay - int longpress delay in milliseconds
   * @return boolean
   */
  public static boolean setLongpressDelay(int longpressDelay) {
    if (longpressDelay < KMMinimum_LongpressDelay || longpressDelay > KMMaximum_LongpressDelay) {
      return false;
    }

    SharedPreferences prefs = appContext.getSharedPreferences(
      appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = prefs.edit();
    editor.putInt(KMKey_LongpressDelay, longpressDelay);
    editor.commit();

    return true;
  }

  /**
   * Sends options to the KeymanWeb keyboard.
   * 1. number of milliseconds to trigger a longpress gesture.
   * This method requires a keyboard to be loaded for the value to take effect.
   */
  public static void sendOptionsToKeyboard() {
    int longpressDelay = getLongpressDelay();
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP)) {
      InAppKeyboard.loadJavascript(KMString.format("setLongpressDelay(%d)", longpressDelay));
    }

    if (SystemKeyboard != null) {
      SystemKeyboard.loadJavascript(KMString.format("setLongpressDelay(%d)", longpressDelay));
    }
  }

  public static int getBannerHeight(Context context) {
    int bannerHeight = 0;
    if (InAppKeyboard != null && InAppKeyboard.getBanner() != BannerType.BLANK) {
      bannerHeight = (int) context.getResources().getDimension(R.dimen.banner_height);
    } else if (SystemKeyboard != null && SystemKeyboard.getBanner() != BannerType.BLANK) {
      bannerHeight = (int) context.getResources().getDimension(R.dimen.banner_height);
    }
    return bannerHeight;
  }

  public static int getKeyboardHeight(Context context) {
    int defaultHeight = (int) context.getResources().getDimension(R.dimen.keyboard_height);
    SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);

    int orientation = getOrientation(context);
    if (orientation == Configuration.ORIENTATION_PORTRAIT) {
      return prefs.getInt(KMManager.KMKey_KeyboardHeightPortrait, defaultHeight);
    } else if (orientation == Configuration.ORIENTATION_LANDSCAPE) {
      return prefs.getInt(KMManager.KMKey_KeyboardHeightLandscape, defaultHeight);
    }

    return defaultHeight;
  }

  public static void applyKeyboardHeight(Context context, int height) {
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP)) {
      InAppKeyboard.loadJavascript(KMString.format("setOskHeight('%s')", height));
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      InAppKeyboard.setLayoutParams(params);
    }
    if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM)) {
      SystemKeyboard.loadJavascript(KMString.format("setOskHeight('%s')", height));
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams();
      SystemKeyboard.setLayoutParams(params);
    }
  }

  /**
   * Get the size of the area the window would occupy.
   * API 30+
   * https://developer.android.com/reference/android/view/WindowManager#getCurrentWindowMetrics()
   * @param context
   * @return Point (width, height)
   */
  public static Point getWindowSize(Context context) {
    WindowManager wm = (WindowManager) context.getSystemService(Context.WINDOW_SERVICE);
    if (Build.VERSION.SDK_INT < Build.VERSION_CODES.R) {
      // Deprecated in API 30
      Point size = new Point(0, 0);
      wm.getDefaultDisplay().getSize(size);
      return size;
    }

    WindowMetrics windowMetrics = wm.getCurrentWindowMetrics();
    return new Point(
      windowMetrics.getBounds().width(),
      windowMetrics.getBounds().height());
  }

  public static float getWindowDensity(Context context) {
    DisplayMetrics metrics = context.getResources().getDisplayMetrics();
    Log.d(TAG, "KMManager: metrics.density " + metrics.density);
    return metrics.density;
  }

  protected static void setPersistentShouldShowHelpBubble(boolean flag) {
    SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    SharedPreferences.Editor editor = prefs.edit();
    editor.putBoolean(KMManager.KMKey_ShouldShowHelpBubble, flag);
    editor.commit();
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
    if (kbType == KeyboardType.KEYBOARD_TYPE_UNDEFINED) {
      KMLog.LogError(TAG, String.format("showKeyboardPicker with invalid %s", kbType.toString()));
      return;
    }

    Intent i = new Intent(context, KeyboardPickerActivity.class);
    i.addFlags(Intent.FLAG_ACTIVITY_NEW_DOCUMENT); // Replaces FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET
    i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);     // Required to call startActivity() from outside of an Activity context

    if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      i.addFlags(Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS);
    }

    i.putExtra(KMKey_DisplayKeyboardSwitcher, kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM);
    context.startActivity(i);
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
      if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP) && !InAppKeyboard.shouldIgnoreTextChange()) {
        InAppKeyboard.loadJavascript("setNumericLayer()");
      }
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM) && !SystemKeyboard.shouldIgnoreTextChange()) {
        SystemKeyboard.loadJavascript("setNumericLayer()");
      }
    }
  }

  public static boolean updateText(KeyboardType kbType, String text) {
    boolean result = false;

    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      return InAppKeyboard.updateText(text);
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      return SystemKeyboard.updateText(text);
    }

    return result;
  }

  /**
   * Updates the active range for selected text.
   * @deprecated
   * This method no longer needs the `selStart` and `selEnd` parameters.
   * <p>Use {@link KMManager#updateSelectionRange(KeyboardType)} instead.</p>
   *
   * @param kbType    A value indicating if this request is for the in-app keyboard or the system keyboard
   * @param selStart  (deprecated) the start index for the range
   * @param selEnd  (deprecated) the end index for the selected range
   * @return
   */
  @Deprecated
  public static boolean updateSelectionRange(KeyboardType kbType, int selStart, int selEnd) {
    return updateSelectionRange(kbType);
  }

  /**
   * Performs a synchronization check for the active range for selected text,
   * ensuring it matches the text-editor's current state.
   * @param kbType  A value indicating if this request is for the in-app or system keyboard.
   * @return
   */
  public static boolean updateSelectionRange(KeyboardType kbType) {
    boolean result = false;

    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP) && !InAppKeyboard.shouldIgnoreSelectionChange()) {
        result = InAppKeyboard.updateSelectionRange();
      }

      InAppKeyboard.setShouldIgnoreSelectionChange(false);
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM) && !SystemKeyboard.shouldIgnoreSelectionChange()) {
        result = SystemKeyboard.updateSelectionRange();
      }

      SystemKeyboard.setShouldIgnoreSelectionChange(false);
    }

    return result;
  }

  public static void resetContext(KeyboardType kbType) {
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_INAPP)) {
        InAppKeyboard.loadJavascript("resetContext()");
      }
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (isKeyboardLoaded(KeyboardType.KEYBOARD_TYPE_SYSTEM)) {
        SystemKeyboard.loadJavascript("resetContext()");
      }
    }
    Log.d(TAG, "backspace: resetContext with " + kbType);
  }

  public static int getCurrentKeyboardIndex(Context context) {
    String key = KMKeyboard.currentKeyboard();
    return KeyboardController.getInstance().getKeyboardIndex(key);
  }

  public static Keyboard getCurrentKeyboardInfo(Context context) {
    int index = getCurrentKeyboardIndex(context);
    if(index < 0) {
      // index can be undefined if user installs Keyman (without launching it)
      // and then enables Keyaman as a system keyboard from the Android settings menus.
      // We'll only log if key isn't for fallback keyboard
      String key = KMKeyboard.currentKeyboard();
      if (!isDefaultKey(key)) {
        KMLog.LogError(TAG, "Failed getCurrentKeyboardIndex check for keyboard: " + key);
      }
      return null;
    }
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

  // This API is deprecated in Keyman 16.0 - see #7473
  public static boolean isHelpBubbleEnabled() {
    return false;
  }

  // This API is deprecated in Keyman 16.0 - see #7473
  public static void setHelpBubbleEnabled(boolean newValue) {
    return;
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
   * Return if the lock screen is locked (prevents keyboard picker menu from being displayed)
   * @return boolean
   */
  private static boolean isLocked() {
    KeyguardManager keyguardManager = (KeyguardManager) appContext.getSystemService(Context.KEYGUARD_SERVICE);
    // inKeyguardRestrictedInputMode() deprecated, so check isKeyguardLocked() to determine if screen is locked
    return keyguardManager.isKeyguardLocked();
  }

  /**
   * Handle the globe key action
   * @param globeKeyDown boolean if the globe key state is GLOBE_KEY_STATE_DOWN
   * @param keyboardType KeyboardType KEYBOARD_TYPE_INAPP or KEYBOARD_TYPE_SYSTEM
   */
  public static void handleGlobeKeyAction(Context context, boolean globeKeyDown, KeyboardType keyboardType) {
    if (globeKeyState == GlobeKeyState.GLOBE_KEY_STATE_UP && !globeKeyDown) {
      // No globe key action to process
      return;
    }

    // Update globeKeyState
    if (globeKeyState != GlobeKeyState.GLOBE_KEY_STATE_LONGPRESS) {
      globeKeyState = globeKeyDown ? GlobeKeyState.GLOBE_KEY_STATE_DOWN : GlobeKeyState.GLOBE_KEY_STATE_UP;
    }

    if (KMManager.shouldAllowSetKeyboard()) {
      // inKeyguardRestrictedInputMode() deprecated, so check isKeyguardLocked() to determine if screen is locked
      if (isLocked()) {
        if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM && globeKeyState == GlobeKeyState.GLOBE_KEY_STATE_DOWN) {
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
}
