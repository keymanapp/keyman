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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import android.annotation.SuppressLint;
import android.app.Dialog;
import android.app.KeyguardManager;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
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

import com.google.firebase.analytics.FirebaseAnalytics;
import com.tavultesoft.kmea.KeyboardEventHandler.EventType;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;
import com.tavultesoft.kmea.cloud.CloudDataJsonUtil;
import com.tavultesoft.kmea.cloud.CloudDownloadMgr;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.logic.ResourcesUpdateTool;
import com.tavultesoft.kmea.packages.JSONUtils;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.CharSequenceUtil;
import com.tavultesoft.kmea.util.FileUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public final class KMManager {

  private static final String TAG = "KMManager";

  private static FirebaseAnalytics mFirebaseAnalytics;
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
    GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD,         // Switch to next Keyman keyboard
    GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD, // Advance to next system keyboard
    GLOBE_KEY_ACTION_DO_NOTHING,
  }

  public enum FormFactor {
    PHONE,
    TABLET;
  }

  private static InputMethodService IMService;
  private static boolean debugMode = false;
  private static boolean shouldAllowSetKeyboard = true;
  private static boolean didCopyAssets = false;

  private static GlobeKeyAction inappKbGlobeKeyAction = GlobeKeyAction.GLOBE_KEY_ACTION_SHOW_MENU;
  private static GlobeKeyAction sysKbGlobeKeyAction = GlobeKeyAction.GLOBE_KEY_ACTION_SHOW_MENU;
  // This is used to keep track of the starting system keyboard index while the screen is locked
  private static int sysKbStartingIndexOnLockScreen = -1;

  protected static boolean InAppKeyboardLoaded = false;
  protected static boolean SystemKeyboardLoaded = false;
  protected static boolean InAppKeyboardShouldIgnoreTextChange = false;
  protected static boolean InAppKeyboardShouldIgnoreSelectionChange = false;
  protected static boolean SystemKeyboardShouldIgnoreTextChange = false;
  protected static boolean SystemKeyboardShouldIgnoreSelectionChange = false;
  protected static KMKeyboard InAppKeyboard = null;
  protected static KMKeyboard SystemKeyboard = null;
  protected static HashMap<String, String> currentLexicalModel = null;

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

  // Keyman public keys
  public static final String KMKey_ID = "id";
  public static final String KMKey_Name = "name";
  public static final String KMKey_Version = "version";
  public static final String KMKey_PackageID = "packageId";
  public static final String KMKey_LanguageID = "langId";
  public static final String KMKey_LanguageName = "langName";
  public static final String KMKey_KeyboardCount = "kbCount";
  public static final String KMKey_HelpLink = "helpLink";
  public static final String KMKey_Icon = "icon";
  public static final String KMKey_KeyboardID = "kbId";
  public static final String KMKey_KeyboardName = "kbName";
  public static final String KMKey_KeyboardVersion = "version";
  public static final String KMKey_Font = "font";
  public static final String KMKey_DisplayFont = "displayFont";
  public static final String KMKey_OskFont = "oskFont";
  public static final String KMKey_FontSource = "source";
  public static final String KMKey_FontFiles = "files";
  public static final String KMKey_KeyboardModified = "lastModified";
  public static final String KMKey_KeyboardRTL = "rtl";
  public static final String KMKey_CustomKeyboard = "CustomKeyboard";
  public static final String KMKey_CustomModel = "CustomModel";
  public static final String KMKey_CustomHelpLink = "CustomHelpLink";
  public static final String KMKey_UserKeyboardIndex = "UserKeyboardIndex";
  public static final String KMKey_DisplayKeyboardSwitcher = "DisplayKeyboardSwitcher";
  public static final String KMKey_LexicalModelID = "lmId";
  public static final String KMKey_LexicalModelName = "lmName";
  public static final String KMKey_LexicalModelVersion = "lmVersion";
  public static final String KMKey_LexicalModelPackageFilename = "kmpPackageFilename";

  // Keyman internal keys
  protected static final String KMKey_ShouldShowHelpBubble = "ShouldShowHelpBubble";

  // Default Asset Paths
  // Previous keyboards installed from the cloud  went to /languages/ and /fonts/
  // Keyboards now get installed into /packages/packageID/
  // Keyboards that have an undefined packageID are installed into /cloud/
  public static final String KMDefault_LegacyAssetLanguages = "languages";
  public static final String KMDefault_LegacyAssetFonts = "fonts";
  public static final String KMDefault_UndefinedPackageID = "cloud";
  public static final String KMDefault_AssetPackages = "packages";
  public static final String KMDefault_LexicalModelPackages = "models";

  // Default Keyboard Info
  public static final String KMDefault_KeyboardID = "sil_euro_latin";
  public static final String KMDefault_LanguageID = "en";
  public static final String KMDefault_KeyboardName = "EuroLatin (SIL) Keyboard";
  public static final String KMDefault_LanguageName = "English";
  public static final String KMDefault_KeyboardFont = "{\"family\":\"LatinWeb\",\"source\":[\"DejaVuSans.ttf\"]}";

  // Default Dictionary Info
  public static final String KMDefault_DictionaryPackageID = "nrc.en.mtnt";
  public static final String KMDefault_DictionaryModelID = "nrc.en.mtnt";
  public static final String KMDefault_DictionaryKMP = KMDefault_DictionaryPackageID + FileUtils.MODELPACKAGE;

  // Keyman files
  protected static final String KMFilename_KeyboardHtml = "keyboard.html";
  protected static final String KMFilename_JSEngine = "keyman.js";
  protected static final String KMFilename_JSEngine_Sourcemap = "keyman.js.map";
  protected static final String KMFilename_KmwCss = "kmwosk.css";
  protected static final String KMFilename_Osk_Ttf_Font = "keymanweb-osk.ttf";
  protected static final String KMFilename_Osk_Woff_Font = "keymanweb-osk.woff";
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

  /**
   * Extract app version #.#.# from VERSION_NAME
   * @return String
   */
  public static String getVersion() {
    // Regex needs to match the entire string
    String appVersion = com.tavultesoft.kmea.BuildConfig.VERSION_NAME;
    Pattern pattern = Pattern.compile("^(\\d+\\.\\d+\\.\\d+).*");
    Matcher matcher = pattern.matcher(appVersion);
    if (matcher.matches() && matcher.groupCount() >= 1) {
      appVersion = matcher.group(1);
    }

    return appVersion;
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

    mFirebaseAnalytics = FirebaseAnalytics.getInstance(context);

    if (!didCopyAssets || isTestMode()) {
      copyAssets(appContext);
      migrateOldKeyboardFiles(appContext);
      updateOldKeyboardsList(appContext);
      didCopyAssets = true;
    }

    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      initInAppKeyboard(appContext);
    } else if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      initSystemKeyboard(appContext);
    } else {
      Log.e(TAG, "Cannot initialize: Invalid keyboard type");
    }

    JSONUtils.initialize(new File(getPackagesDir()));

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
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP) {
      InAppKeyboard.executeHardwareKeystroke(code, shift, lstates, eventModifiers);
      return true;
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      SystemKeyboard.executeHardwareKeystroke(code, shift, lstates, eventModifiers);
      return true;
    }

    return false;
  }

  /**
   * Adjust the keyboard dimensions. If the suggestion banner is active, use the
   * combined banner height and keyboard height
   * @param keyboard KeyboardType
   * @return RelativeLayout.LayoutParams
   */
  private static RelativeLayout.LayoutParams getKeyboardLayoutParams(KeyboardType keyboard) {
    int bannerHeight = getBannerHeight(appContext);
    int kbHeight = getKeyboardHeight(appContext);
    RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(
      RelativeLayout.LayoutParams.MATCH_PARENT, bannerHeight + kbHeight);
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP) {
      params.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.TRUE);
    }
   return params;
  }

  private static void initInAppKeyboard(Context appContext) {
    if (InAppKeyboard == null) {
      InAppKeyboard = new KMKeyboard(appContext, KeyboardType.KEYBOARD_TYPE_INAPP);
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_INAPP);
      InAppKeyboard.setLayoutParams(params);
      InAppKeyboard.setVerticalScrollBarEnabled(false);
      InAppKeyboard.setHorizontalScrollBarEnabled(false);
      InAppKeyboard.setWebViewClient(new KMInAppKeyboardWebViewClient(appContext));
      InAppKeyboard.addJavascriptInterface(new KMInAppKeyboardJSHandler(appContext, InAppKeyboard), "jsInterface");
      InAppKeyboard.loadKeyboard();
    }
  }

  private static void initSystemKeyboard(Context appContext) {
    if (SystemKeyboard == null) {
      SystemKeyboard = new KMKeyboard(appContext, KeyboardType.KEYBOARD_TYPE_SYSTEM);
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_SYSTEM);
      SystemKeyboard.setLayoutParams(params);
      SystemKeyboard.setVerticalScrollBarEnabled(false);
      SystemKeyboard.setHorizontalScrollBarEnabled(false);
      SystemKeyboard.setWebViewClient(new KMSystemKeyboardWebViewClient(appContext));
      SystemKeyboard.addJavascriptInterface(new KMSystemKeyboardJSHandler(appContext, SystemKeyboard), "jsInterface");
      SystemKeyboard.loadKeyboard();
    }
  }

  public static void hideSystemKeyboard() {
    if (SystemKeyboard != null) {
      SystemKeyboard.hideKeyboard();
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
    if (!restarting) {
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
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_INAPP);
      InAppKeyboard.setLayoutParams(params);
      InAppKeyboard.onConfigurationChanged(newConfig);
    }
    if (SystemKeyboard != null) {
      RelativeLayout.LayoutParams params = getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_SYSTEM);
      SystemKeyboard.setLayoutParams(params);
      SystemKeyboard.onConfigurationChanged(newConfig);
    }
  }

  public static boolean hasConnection(Context context) {
    ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);

    NetworkInfo wifiNetwork = cm.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
    if (wifiNetwork != null && wifiNetwork.isConnected()) {
      return true;
    }

    NetworkInfo mobileNetwork = cm.getNetworkInfo(ConnectivityManager.TYPE_MOBILE);
    if (mobileNetwork != null && mobileNetwork.isConnected()) {
      return true;
    }

    NetworkInfo activeNetwork = cm.getActiveNetworkInfo();
    if (activeNetwork != null && activeNetwork.isConnected()) {
      return true;
    }

    return false;
  }

  private static void copyAssets(Context context) {
    AssetManager assetManager = context.getAssets();
    try {
      // Copy main files
      copyAsset(context, KMDefault_DictionaryKMP, "", true);
      copyAsset(context, KMFilename_KeyboardHtml, "", true);
      copyAsset(context, KMFilename_JSEngine, "", true);
      if(KMManager.isDebugMode()) {
        copyAsset(context, KMFilename_JSEngine_Sourcemap, "", true);
      }
      copyAsset(context, KMFilename_KmwCss, "", true);
      copyAsset(context, KMFilename_Osk_Ttf_Font, "", true);
      copyAsset(context, KMFilename_Osk_Woff_Font, "", true);

      // Keyboard packages directory
      File packagesDir = new File(getPackagesDir());
      if (!packagesDir.exists()) {
        packagesDir.mkdir();
      }

      // Copy default cloud keyboard
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
    } catch (Exception e) {
      Log.e(TAG, "Failed to copy assets. Error: " + e);
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
      Log.e(TAG, "Failed to copy asset. Error: " + e);
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
   * Assumption: No legacy keyboards exist in /packages/*.js
   * @param context
   */
  private static void migrateOldKeyboardFiles(Context context) {
    String legacyLanguagesPath = getResourceRoot() + KMDefault_LegacyAssetLanguages + File.separator;
    String legacyFontsPath = getResourceRoot() + KMDefault_LegacyAssetFonts + File.separator;
    File legacyLanguagesDir = new File(legacyLanguagesPath);
    File legacyFontsDir = new File(legacyFontsPath);
    if (!legacyLanguagesDir.exists() && !legacyFontsDir.exists()) {
      return;
    }

    File migratedDir = new File(getCloudDir());
    if (!migratedDir.exists()) {
      migratedDir.mkdir();
    }

    try {
      if (legacyLanguagesDir.exists()) {
        FileFilter keyboardFilter = new FileFilter() {
          @Override
          public boolean accept(File pathname) {
            if (pathname.isFile() && FileUtils.hasJavaScriptExtension(pathname.getName())) {
              return true;
            }
            return false;
          }
        };

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
    } catch (IOException e) {
      Log.e(TAG, "Failed to migrate assets. Error: " + e);
    }
  }

  public static void updateOldKeyboardsList(Context context) {
    ArrayList<HashMap<String, String>> kbList = KeyboardPickerActivity.getKeyboardsList(context);
    if (kbList != null && kbList.size() > 0) {
      boolean shouldUpdateList = false;
      boolean shouldClearCache = false;
      HashMap<String, String> kbInfo = kbList.get(0);
      String kbID = kbInfo.get(KMKey_KeyboardID);
      if (kbID.equals("us") || (kbID.equals("european2"))) {
        HashMap<String, String> newKbInfo = new HashMap<String, String>();
        newKbInfo.put(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
        newKbInfo.put(KMManager.KMKey_KeyboardID, KMManager.KMDefault_KeyboardID);
        newKbInfo.put(KMManager.KMKey_LanguageID, KMManager.KMDefault_LanguageID);
        newKbInfo.put(KMManager.KMKey_KeyboardName, KMManager.KMDefault_KeyboardName);
        newKbInfo.put(KMManager.KMKey_LanguageName, KMManager.KMDefault_LanguageName);
        newKbInfo.put(KMManager.KMKey_KeyboardVersion,
          getLatestKeyboardFileVersion(context, KMManager.KMDefault_UndefinedPackageID,
            KMManager.KMDefault_KeyboardID));
        newKbInfo.put(KMManager.KMKey_CustomKeyboard, "N");
        newKbInfo.put(KMManager.KMKey_Font, KMManager.KMDefault_KeyboardFont);
        kbList.set(0, newKbInfo);
        shouldUpdateList = true;
        shouldClearCache = true;
      }

      int index2Remove = -1;
      int kblCount = kbList.size();
      for (int i = 0; i < kblCount; i++) {
        kbInfo = kbList.get(i);

        kbID = kbInfo.get(KMKey_KeyboardID);
        String pkgID = kbInfo.get(KMKey_PackageID);
        if (pkgID == null || pkgID.isEmpty()) {
          pkgID = KMDefault_UndefinedPackageID;
          kbInfo.put(KMManager.KMKey_PackageID, pkgID);
          shouldUpdateList = true;
        }
        String langID = kbInfo.get(KMKey_LanguageID);
        String kbVersion = kbInfo.get(KMManager.KMKey_KeyboardVersion);
        String latestKbVersion = getLatestKeyboardFileVersion(context, pkgID, kbID);
        if ((latestKbVersion != null) && (kbVersion == null || !kbVersion.equals(latestKbVersion))) {
          kbInfo.put(KMManager.KMKey_KeyboardVersion, latestKbVersion);
          kbList.set(i, kbInfo);
          shouldUpdateList = true;
        }

        String isCustom = kbInfo.get(KMManager.KMKey_CustomKeyboard);
        if (isCustom == null || isCustom.equals("U")) {
          String kbKey = String.format("%s_%s", langID, kbID);
          kbInfo.put(KMManager.KMKey_CustomKeyboard, isCustomKeyboard(context, kbKey));
          kbList.set(i, kbInfo);
          shouldUpdateList = true;
        }

        if (kbID.equals(KMManager.KMDefault_KeyboardID) && langID.equals(KMManager.KMDefault_LanguageID)) {
          int defKbIndex = KMManager.getKeyboardIndex(context, KMManager.KMDefault_KeyboardID, KMManager.KMDefault_LanguageID);
          if (defKbIndex == 0 && i > 0)
            index2Remove = i;
        }
      }

      if (index2Remove > 0) {
        kbList.remove(index2Remove);
        SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
        if (index == index2Remove) {
          index = 0;
        } else if (index > index2Remove) {
          index--;
        }
        editor.putInt(KMManager.KMKey_UserKeyboardIndex, index);
        editor.commit();
        shouldUpdateList = true;
      }

      if (shouldUpdateList) {
        KeyboardPickerActivity.updateKeyboardsList(context, kbList);
      }

      if (shouldClearCache) {
        File cache = CloudDataJsonUtil.getKeyboardCacheFile(appContext);
        if (cache.exists()) {
          cache.delete();
        }
      }
    }
  }

  private static String isCustomKeyboard(Context context, String keyboardKey) {
    String isCustom = "U";
    HashMap<String, HashMap<String, String>> keyboardsInfo = LanguageListActivity.getKeyboardsInfo(context);
    if (keyboardsInfo != null) {
      HashMap<String, String> kbInfo = keyboardsInfo.get(keyboardKey);
      if (kbInfo != null) {
        isCustom = "N";
      } else {
        isCustom = "Y";
      }
    }

    return isCustom;
  }

  /**
   * Sets mayPredictOverride true if the InputType field is a hidden password text field
   * (either TYPE_TEXT_VARIATION_PASSWORD or TYPE_TEXT_VARIATION_WEB_PASSWORD
   * but not TYPE_TEXT_VARIATION_VISIBLE_PASSWORD)
   * @param inputType android.text.InputType
   */
  public static void setMayPredictOverride(int inputType) {
    mayPredictOverride =
      ((inputType == (InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_PASSWORD)) ||
       (inputType == (InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_WEB_PASSWORD)));
  }

  /**
   * Get the value of mayPredictOverride
   * @return boolean
   */
  public static boolean getMayPredictOverride() {
    return mayPredictOverride;
  }

  /**
   * Get the font typeface from a fully pathed font name
   * @param context
   * @param fontFilename String - full path to the font file
   * @return Typeface
   */
  public static Typeface getFontTypeface(Context context, String fontFilename) {
    Typeface font = null;

    try {
      if ((fontFilename != null) && FileUtils.hasFontExtension(fontFilename)) {
        File file = new File(fontFilename);
        if (file.exists()) {
          font = Typeface.createFromFile(file);
        }
      }
    } catch (Exception e) {
      Log.e(TAG, "Failed to create Typeface: " + fontFilename);
    }
    return font;
  }

  public static ArrayList<HashMap<String, String>> getKeyboardsList(Context context) {
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
    String path = "file://" + getLexicalModelsDir() + pkgID + File.separator + modelID + ".model.js";

    JSONObject modelObj = new JSONObject();
    JSONArray languageJSONArray = new JSONArray();
    try {
      modelObj.put("id", modelID);
      languageJSONArray.put(languageID);
      modelObj.put("languages", languageJSONArray);
      modelObj.put("path", path);
      modelObj.put("CustomHelpLink", lexicalModelInfo.get(KMKey_CustomHelpLink));
    } catch (JSONException e) {
      Log.e(TAG, "Invalid lexical model to register");
      return false;
    }

    // Escape single quotes, and then convert double quotes to single quotes for javascript call
    String model = String.valueOf(modelObj);
    model = model.replaceAll("\'", "\\\\'"); // Double-escaped-backslash b/c regex.
    model = model.replaceAll("\"", "'");

    // When entering password field, mayPredict should override to false
    SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
    boolean mayPredict = (mayPredictOverride) ? false :
      prefs.getBoolean(LanguageSettingsActivity.getLanguagePredictionPreferenceKey(languageID), true);
    boolean mayCorrect = prefs.getBoolean(LanguageSettingsActivity.getLanguageCorrectionPreferenceKey(languageID), true);

    RelativeLayout.LayoutParams params;
    if (InAppKeyboard != null && InAppKeyboardLoaded && !InAppKeyboardShouldIgnoreTextChange) {
      params = getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_INAPP);
      InAppKeyboard.setLayoutParams(params);
      InAppKeyboard.loadJavascript(String.format("enableSuggestions(%s, %s, %s)", model, mayPredict, mayCorrect));
    }
    if (SystemKeyboard != null && SystemKeyboardLoaded && !SystemKeyboardShouldIgnoreTextChange) {
      params = getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_SYSTEM);
      SystemKeyboard.setLayoutParams(params);
      SystemKeyboard.loadJavascript(String.format("enableSuggestions(%s, %s, %s)", model, mayPredict, mayCorrect));
    }
    return true;
  }

  public static boolean deregisterLexicalModel(String modelID) {
    // Check if current lexical model needs to be cleared
    if (currentLexicalModel != null && currentLexicalModel.get(KMManager.KMKey_LexicalModelID).equalsIgnoreCase(modelID)) {
      currentLexicalModel = null;
    }

    String url = String.format("deregisterModel('%s')", modelID);
    if (InAppKeyboard != null) { // && InAppKeyboardLoaded) {
      InAppKeyboard.loadJavascript(url);
    }

    if (SystemKeyboard != null) { // && SystemKeyboardLoaded) {
      SystemKeyboard.loadJavascript(url);
    }
    return true;
  }

  public static boolean setBannerOptions(boolean mayPredict) {
    String url = String.format("setBannerOptions(%s)", mayPredict);
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
        if (langId.equalsIgnoreCase(lmInfo.get(KMManager.KMKey_LanguageID))) {
          return lmInfo;
        }
      }
    }

    return null;
  }

  public static boolean addKeyboard(Context context, HashMap<String, String> keyboardInfo) {
    // Log Firebase analytic event.
    Bundle params = new Bundle();
    params.putString("packageID", keyboardInfo.get(KMManager.KMKey_PackageID));
    params.putString("keyboardID", keyboardInfo.get(KMManager.KMKey_KeyboardID));
    params.putString("keyboardName", keyboardInfo.get(KMManager.KMKey_KeyboardName));
    params.putString("keyboardVersion", keyboardInfo.get(KMManager.KMKey_KeyboardVersion));

    if(mFirebaseAnalytics != null) {
      mFirebaseAnalytics.logEvent("km_add_keyboard", params);
    }

    return KeyboardPickerActivity.addKeyboard(context, keyboardInfo);
  }

  public static boolean removeKeyboard(Context context, int position) {
    return KeyboardPickerActivity.removeKeyboard(context, position);
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
        InAppKeyboard.setLayoutParams(getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_INAPP));
      if(result2)
        SystemKeyboard.setLayoutParams(getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_SYSTEM));
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
    HashMap<String, String> keyboardInfo = getKeyboardInfo(context, position);
    if (keyboardInfo == null)
      return false;

    String pkgId = keyboardInfo.get(KMManager.KMKey_PackageID);
    String kbId = keyboardInfo.get(KMManager.KMKey_KeyboardID);
    String langId = keyboardInfo.get(KMManager.KMKey_LanguageID);
    String kbName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
    String langName = keyboardInfo.get(KMManager.KMKey_LanguageName);
    String kFont = keyboardInfo.get(KMManager.KMKey_Font);
    String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
    return setKeyboard(pkgId, kbId, langId, kbName, langName, kFont, kOskFont);
  }

  public static void switchToNextKeyboard(Context context) {
    int index = KeyboardPickerActivity.getCurrentKeyboardIndex(context);
    index++;
    HashMap<String, String> kbInfo = KeyboardPickerActivity.getKeyboardInfo(context, index);
    if (kbInfo == null) {
      index = 0;
      kbInfo = KeyboardPickerActivity.getKeyboardInfo(context, index);
    }

    String pkgId = kbInfo.get(KMManager.KMKey_PackageID);
    String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
    String langId = kbInfo.get(KMManager.KMKey_LanguageID);
    String kbName = kbInfo.get(KMManager.KMKey_KeyboardName);
    String langName = kbInfo.get(KMManager.KMKey_LanguageName);
    String kFont = kbInfo.get(KMManager.KMKey_Font);
    String kOskFont = kbInfo.get(KMManager.KMKey_OskFont);
    if (InAppKeyboard != null) {
      InAppKeyboard.setKeyboard(pkgId, kbId, langId, kbName, langName, kFont, kOskFont);
    }

    if (SystemKeyboard != null) {
      SystemKeyboard.setKeyboard(pkgId, kbId, langId, kbName, langName, kFont, kOskFont);
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
      Log.e("KMManager", "Invalid keyboard");
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
    if (packageID == null || keyboardID == null || languageID == null)
      return kbState;

    packageID = packageID.trim();
    keyboardID = keyboardID.trim();
    languageID = languageID.trim();
    if (keyboardID.isEmpty() || languageID.isEmpty())
      return kbState;

    String latestVersion = getLatestKeyboardFileVersion(context, packageID, keyboardID);
    if (latestVersion == null) {
      kbState = KeyboardState.KEYBOARD_STATE_NEEDS_DOWNLOAD;
    } else {
      kbState = KeyboardState.KEYBOARD_STATE_UP_TO_DATE;

      HashMap<String, HashMap<String, String>> keyboardsInfo = LanguageListActivity.getKeyboardsInfo(context);
      if (keyboardsInfo != null) {
        // Check version
        String kbKey = String.format("%s_%s", languageID, keyboardID);
        HashMap<String, String> kbInfo = keyboardsInfo.get(kbKey);
        String kbVersion = "1.0";
        if (kbInfo != null) {
          kbVersion = kbInfo.get(KMManager.KMKey_KeyboardVersion);
        }

        if (kbVersion != null && (FileUtils.compareVersions(kbVersion, latestVersion) == FileUtils.VERSION_GREATER)) {
          kbState = KeyboardState.KEYBOARD_STATE_NEEDS_UPDATE;
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
        String base = String.format("%s-", keyboardID);
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

      try {
        File kmpJSONFile = new File(path);
        if (!kmpJSONFile.exists()) {
          return null;
        }
        JSONParser jsonParser = new JSONParser();
        JSONObject kmpObject = jsonParser.getJSONObjectFromFile(kmpJSONFile);

        return PackageProcessor.getKeyboardVersion(kmpObject, keyboardID);
      } catch (Exception e) {
        return null;
      }
    }

    return kbFileVersion;
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
    return (int) context.getResources().getDimension(R.dimen.keyboard_height);
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
      setKeyboard(KMDefault_UndefinedPackageID, KMDefault_KeyboardID,
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

  public static void showLanguageList(Context context) {
    KeyboardPickerActivity.showLanguageList(context);
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
        InAppKeyboard.loadJavascript(String.format("updateKMText('%s')", kmText));
        result = true;
      }

      InAppKeyboardShouldIgnoreTextChange = false;
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard != null && SystemKeyboardLoaded && !SystemKeyboardShouldIgnoreTextChange) {
        SystemKeyboard.loadJavascript(String.format("updateKMText('%s')", kmText));
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
        InAppKeyboard.loadJavascript(String.format("updateKMSelectionRange(%d,%d)", selStart, selEnd));
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

        SystemKeyboard.loadJavascript(String.format("updateKMSelectionRange(%d,%d)", selStart, selEnd));
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
    return KeyboardPickerActivity.getCurrentKeyboardIndex(context);
  }

  public static HashMap<String, String> getCurrentKeyboardInfo(Context context) {
    return KeyboardPickerActivity.getCurrentKeyboardInfo(context);
  }

  public static int getKeyboardIndex(Context context, String keyboardID, String languageID) {
    int index = -1;

    if (keyboardID != null & languageID != null) {
      String kbKey = String.format("%s_%s", languageID, keyboardID);
      index = KeyboardPickerActivity.getKeyboardIndex(context, kbKey);
    }

    return index;
  }

  public static HashMap<String, String> getKeyboardInfo(Context context, int index) {
    return KeyboardPickerActivity.getKeyboardInfo(context, index);
  }

  public static HashMap<String, String> getLexicalModelInfo(Context context, int index) {
    return KeyboardPickerActivity.getLexicalModelInfo(context, index);
  }

  public static boolean keyboardExists(Context context, String packageID, String keyboardID, String languageID) {
    boolean result = false;

    if (packageID != null && keyboardID != null && languageID != null) {
      String kbKey = String.format("%s_%s", languageID, keyboardID);
      result = KeyboardPickerActivity.containsKeyboard(context, kbKey);
    }

    return result;
  }

  public static boolean lexicalModelExists(Context context, String packageID, String languageID, String modelID) {
    boolean result = false;

    if (packageID != null && languageID != null &&  modelID != null) {
      String lmKey = String.format("%s_%s_%s", packageID, languageID, modelID);
      result = KeyboardPickerActivity.containsLexicalModel(context, lmKey);
    }

    return result;
  }

  public static FormFactor getFormFactor() {
    String device_type = appContext.getResources().getString(R.string.device_type);

    return device_type.equals("AndroidMobile") ? FormFactor.PHONE : FormFactor.TABLET;
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
      String langId = KMManager.KMKey_LanguageID;
      Log.d("KMEA", "pageLoaded: [inapp] " + url);
      if (url.startsWith("file")) { //endsWith(KMFilename_KeyboardHtml)) {
        InAppKeyboardLoaded = true;

        if (!InAppKeyboard.keyboardSet) {
          SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
          int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
          if (index < 0) {
            index = 0;
          }
          HashMap<String, String> keyboardInfo = KMManager.getKeyboardInfo(context, index);
          if (keyboardInfo != null) {
            String pkgId = keyboardInfo.get(KMManager.KMKey_PackageID);
            String kbId = keyboardInfo.get(KMManager.KMKey_KeyboardID);
            langId = keyboardInfo.get(KMManager.KMKey_LanguageID);
            String kbName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
            String langName = keyboardInfo.get(KMManager.KMKey_LanguageName);
            String kFont = keyboardInfo.get(KMManager.KMKey_Font);
            String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
            InAppKeyboard.setKeyboard(pkgId, kbId, langId, kbName, langName, kFont, kOskFont);
          } else {
            InAppKeyboard.setKeyboard(KMDefault_UndefinedPackageID, KMDefault_KeyboardID,
              KMDefault_LanguageID, KMDefault_KeyboardName, KMDefault_LanguageName, KMDefault_KeyboardFont, null);
          }

         registerAssociatedLexicalModel(langId);
        }

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

        KeyboardEventHandler.notifyListeners(KMTextView.kbEventListeners, KeyboardType.KEYBOARD_TYPE_INAPP, EventType.KEYBOARD_LOADED, null);
      }
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
      Log.d("KMEA", "shouldOverrideUrlLoading [inapp]: "+url);
      if(url.indexOf("pageLoaded") >= 0) {
        pageLoaded(view, url);
      } else if (url.indexOf("hideKeyboard") >= 0) {
        if (KMTextView.activeView != null && KMTextView.activeView.getClass() == KMTextView.class) {
          InAppKeyboard.dismissHelpBubble();
          KMTextView textView = (KMTextView) KMTextView.activeView;
          textView.dismissKeyboard();
        }
      } else if (url.indexOf("globeKeyAction") >= 0) {
        InAppKeyboard.dismissHelpBubble();
        if (!InAppKeyboard.isHelpBubbleEnabled) {
          return false;
        }

        SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(KMManager.KMKey_ShouldShowHelpBubble, false);
        editor.commit();

        if (KMManager.shouldAllowSetKeyboard()) {
          if (InAppKeyboard.keyboardPickerEnabled) {
            if (inappKbGlobeKeyAction == GlobeKeyAction.GLOBE_KEY_ACTION_SHOW_MENU) {
              showKeyboardPicker(context, KeyboardType.KEYBOARD_TYPE_INAPP);
            } else if (inappKbGlobeKeyAction == GlobeKeyAction.GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD) {
              switchToNextKeyboard(context);
            }
          } else {
            switchToNextKeyboard(context);
          }
        }
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
        currentBanner = (change.equals("loaded")) ?
          KM_BANNER_STATE_SUGGESTION : KM_BANNER_STATE_BLANK;
        RelativeLayout.LayoutParams params = getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_INAPP);
        InAppKeyboard.setLayoutParams(params);
      } else if (url.indexOf("suggestPopup") >= 0) {
        // URL has actual path to the keyboard.html file as a prefix!  We need to replace
        // just the first intended '#' to get URI-based query param processing.

        // At some point, other parts of the function should be redone to allow use of ? instead
        // of # in our WebView command "queries" entirely.
        String cmd = url.replace("keyboard.html#", "keyboard.html?");
        Uri urlCommand = Uri.parse(cmd);

        double x = Float.parseFloat(urlCommand.getQueryParameter("x"));
        double y = Float.parseFloat(urlCommand.getQueryParameter("y"));
        double width = Float.parseFloat(urlCommand.getQueryParameter("w"));
        double height = Float.parseFloat(urlCommand.getQueryParameter("h"));
        String suggestionJSON = urlCommand.getQueryParameter("suggestion");
        boolean isCustom = Boolean.parseBoolean(urlCommand.getQueryParameter("custom"));

        JSONParser parser = new JSONParser();
        JSONObject obj = parser.getJSONObjectFromURIString(suggestionJSON);

        /*  // For future implementation
        InAppKeyboard.suggestionWindowPos = new double[]{x, y};
        InAppKeyboard.suggestionJSON = suggestionJSON;

        try {
          Log.v("KMEA", "Suggestion display: " + obj.getString("displayAs"));
          Log.v("KMEA", "Suggestion's banner coords: " + x + ", " + y + ", " + width + ", " + height);
          Log.v("KMEA", "Is a <keep> suggestion: " + isCustom); // likely outdated now that tags exist.
        } catch (JSONException e) {
          //e.printStackTrace();
          Log.v("KMEA", "JSON parsing error: " + e.getMessage());
        }
        */
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
      String langId = KMManager.KMKey_LanguageID;
      Log.d("KMEA", "pageLoaded: [system] " + url);
      if (url.startsWith("file:")) {
        SystemKeyboardLoaded = true;
        if (!SystemKeyboard.keyboardSet) {
          SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
          int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
          if (index < 0) {
            index = 0;
          }
          HashMap<String, String> keyboardInfo = KMManager.getKeyboardInfo(context, index);
          if (keyboardInfo != null) {
            String pkgId = keyboardInfo.get(KMManager.KMKey_PackageID);
            String kbId = keyboardInfo.get(KMManager.KMKey_KeyboardID);
            langId = keyboardInfo.get(KMManager.KMKey_LanguageID);
            String kbName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
            String langName = keyboardInfo.get(KMManager.KMKey_LanguageName);
            String kFont = keyboardInfo.get(KMManager.KMKey_Font);
            String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
            SystemKeyboard.setKeyboard(pkgId, kbId, langId, kbName, langName, kFont, kOskFont);
          } else {
            SystemKeyboard.setKeyboard(KMDefault_UndefinedPackageID, KMDefault_KeyboardID,
              KMDefault_LanguageID, KMDefault_KeyboardName, KMDefault_LanguageName, KMDefault_KeyboardFont, null);
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
      }
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
      if(url.indexOf("pageLoaded") >= 0) {
        pageLoaded(view, url);
      } else if (url.indexOf("hideKeyboard") >= 0) {
        SystemKeyboard.dismissHelpBubble();
        IMService.requestHideSelf(0);
      } else if (url.indexOf("globeKeyAction") >= 0) {
        SystemKeyboard.dismissHelpBubble();
        if (!SystemKeyboard.isHelpBubbleEnabled) {
          return false;
        }

        SharedPreferences prefs = appContext.getSharedPreferences(appContext.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putBoolean(KMManager.KMKey_ShouldShowHelpBubble, false);
        editor.commit();

        if (KMManager.shouldAllowSetKeyboard()) {
          if (SystemKeyboard.keyboardPickerEnabled) {
            KeyguardManager keyguardManager = (KeyguardManager) appContext.getSystemService(Context.KEYGUARD_SERVICE);
            GlobeKeyAction action = sysKbGlobeKeyAction;
            if(keyguardManager.inKeyguardRestrictedInputMode()) {
              // Override system keyboard globe key action if screen is locked:
              // 1. Switch to next Keyman keyboard (no menu)
              // 2. When all the Keyman keyboards have been cycled through, advance to the next system keyboard
              if (sysKbStartingIndexOnLockScreen == getCurrentKeyboardIndex(context)) {
                // All the Keyman keyboards have been cycled through
                action = GlobeKeyAction.GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD;
              } else {
                if (sysKbStartingIndexOnLockScreen == -1) {
                  // Initialize the system keyboard starting index while the screen is locked
                  sysKbStartingIndexOnLockScreen = getCurrentKeyboardIndex(context);
                }
                action = GlobeKeyAction.GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD;
              }
            } else {
              // If screen isn't locked, reset the starting index
              sysKbStartingIndexOnLockScreen = -1;
            }

            switch (action) {
              case GLOBE_KEY_ACTION_SHOW_MENU:
                showKeyboardPicker(context, KeyboardType.KEYBOARD_TYPE_SYSTEM);
                break;
              case GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD:
                switchToNextKeyboard(context);
                break;
              case GLOBE_KEY_ACTION_ADVANCE_TO_NEXT_SYSTEM_KEYBOARD:
                advanceToNextInputMode();
                break;
              default:
                // Do nothing
            }
          } else {
            switchToNextKeyboard(context);
          }
        }
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
        currentBanner = (change.equals("loaded")) ? KM_BANNER_STATE_SUGGESTION : KM_BANNER_STATE_BLANK;
        RelativeLayout.LayoutParams params = getKeyboardLayoutParams(KeyboardType.KEYBOARD_TYPE_SYSTEM);
        SystemKeyboard.setLayoutParams(params);
      } else if (url.indexOf("suggestPopup") >= 0) {
        // URL has actual path to the keyboard.html file as a prefix!  We need to replace
        // just the first intended '#' to get URI-based query param processing.

        // At some point, other parts of the function should be redone to allow use of ? instead
        // of # in our WebView command "queries" entirely.
        String cmd = url.replace("keyboard.html#", "keyboard.html?");
        Uri urlCommand = Uri.parse(cmd);

        double x = Float.parseFloat(urlCommand.getQueryParameter("x"));
        double y = Float.parseFloat(urlCommand.getQueryParameter("y"));
        double width = Float.parseFloat(urlCommand.getQueryParameter("w"));
        double height = Float.parseFloat(urlCommand.getQueryParameter("h"));
        String suggestionJSON = urlCommand.getQueryParameter("suggestion");
        boolean isCustom = Boolean.parseBoolean(urlCommand.getQueryParameter("custom"));

        JSONParser parser = new JSONParser();
        JSONObject obj = parser.getJSONObjectFromURIString(suggestionJSON);

        /*  // For future implementation
        SystemKeyboard.suggestionWindowPos = new double[]{x, y};
        SystemKeyboard.suggestionJSON = suggestionJSON;

        try {
          Log.v("KMEA", "Suggestion display: " + obj.getString("displayAs"));
          Log.v("KMEA", "Suggestion's banner coords: " + x + ", " + y + ", " + width + ", " + height);
          Log.v("KMEA", "Is a <keep> suggestion: " + isCustom); // likely outdated now that tags exist.
        } catch (JSONException e) {
          //e.printStackTrace();
          Log.v("KMEA", "JSON parsing error: " + e.getMessage());
        }
        */
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
              if (!s.isEmpty() && s.charAt(0) == '\n') {
                textView.keyDownUp(KeyEvent.KEYCODE_ENTER);
              } else {
                // *** TO DO: Try to find a solution to the bug on API < 17, insert overwrites on next line
                InAppKeyboardShouldIgnoreTextChange = true;
                InAppKeyboardShouldIgnoreSelectionChange = true;
                textView.getText().insert(start, s);
              }
            } else {
              if (!s.isEmpty() && s.charAt(0) == '\n') {
                InAppKeyboardShouldIgnoreTextChange = true;
                InAppKeyboardShouldIgnoreSelectionChange = true;
                textView.getText().replace(start, end, "");
                textView.keyDownUp(KeyEvent.KEYCODE_ENTER);
              } else {
                if (s.isEmpty()) {
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