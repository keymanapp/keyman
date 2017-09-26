/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.ProgressDialog;
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
import android.os.AsyncTask;
import android.os.Handler;
import android.os.Looper;
import android.text.InputType;
import android.util.DisplayMetrics;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.ExtractedText;
import android.view.inputmethod.ExtractedTextRequest;
import android.view.inputmethod.InputConnection;
import android.webkit.JavascriptInterface;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.FrameLayout;
import android.widget.RelativeLayout;

import com.tavultesoft.kmea.KeyboardEventHandler.EventType;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardEventListener;

public final class KMManager {

  private static final String KMEngineVersion = "2.4.3";

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
    GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD,
    GLOBE_KEY_ACTION_DO_NOTHING,
  }

  private static InputMethodService IMService;
  private static boolean debugMode = false;
  private static boolean shouldAllowSetKeyboard = true;
  private static ArrayList<OnKeyboardDownloadEventListener> kbDownloadEventListeners = null;
  private static boolean didCopyAssets = false;
  private static GlobeKeyAction inappKbGlobeKeyAction = GlobeKeyAction.GLOBE_KEY_ACTION_SHOW_MENU;
  private static GlobeKeyAction sysKbGlobeKeyAction = GlobeKeyAction.GLOBE_KEY_ACTION_SHOW_MENU;

  protected static boolean InAppKeyboardLoaded = false;
  protected static boolean SystemKeyboardLoaded = false;
  protected static boolean InAppKeyboardShouldIgnoreTextChange = false;
  protected static boolean InAppKeyboardShouldIgnoreSelectionChange = false;
  protected static boolean SystemKeyboardShouldIgnoreTextChange = false;
  protected static boolean SystemKeyboardShouldIgnoreSelectionChange = false;
  protected static KMKeyboard InAppKeyboard = null;
  protected static KMKeyboard SystemKeyboard = null;

  protected static final String kKeymanApiBaseURL = "https://r.keymanweb.com/api/3.0/";
  private static final String kKeymanApiRemoteURL = "https://r.keymanweb.com/api/2.0/remote?url=";

  // Keyman public keys
  public static final String KMKey_ID = "id";
  public static final String KMKey_Name = "name";
  public static final String KMKey_LanguageID = "langId";
  public static final String KMKey_LanguageName = "langName";
  public static final String KMKey_KeyboardID = "kbId";
  public static final String KMKey_KeyboardName = "kbName";
  public static final String KMKey_KeyboardVersion = "version";
  public static final String KMKey_Keyboard = "keyboard";
  public static final String KMKey_LanguageKeyboards = "keyboards";
  public static final String KMKey_KeyboardFileSize = "fileSize";
  public static final String KMKey_Font = "font";
  public static final String KMKey_OskFont = "oskFont";
  public static final String KMKey_FontFamily = "family";
  public static final String KMKey_FontSource = "source";
  public static final String KMKey_FontFiles = "files";
  public static final String KMKey_Options = "options";
  public static final String KMKey_Language = "language";
  public static final String KMKey_Languages = "languages";
  public static final String KMKey_Filename = "filename";
  public static final String KMKey_KeyboardModified = "lastModified";
  public static final String KMKey_KeyboardRTL = "rtl";
  public static final String KMKey_CustomKeyboard = "CustomKeyboard";
  public static final String KMKey_CustomHelpLink = "CustomHelpLink";
  public static final String KMKey_UserKeyboardIndex = "UserKeyboardIndex";

  // Keyman internal keys
  protected static final String KMKey_KeyboardBaseURI = "keyboardBaseUri";
  protected static final String KMKey_FontBaseURI = "fontBaseUri";
  protected static final String KMKey_ShouldShowHelpBubble = "ShouldShowHelpBubble";

  // Default Keyboard Info
  public static final String KMDefault_KeyboardID = "european2";
  public static final String KMDefault_LanguageID = "eng";
  public static final String KMDefault_KeyboardName = "EuroLatin2 Keyboard";
  public static final String KMDefault_LanguageName = "English";
  public static final String KMDefault_KeyboardFont = "{\"family\":\"LatinWeb\",\"source\":[\"DejaVuSans.ttf\"]}";

  // Keyman files
  protected static final String KMFilename_KeyboardHtml = "keyboard.html";
  protected static final String KMFilename_JSEngine = "keyman.js";
  protected static final String KMFilename_KmwCss = "kmwosk.css";
  protected static final String KMFilename_Osk_Ttf_Font = "keymanweb-osk.ttf";
  protected static final String KMFilename_Osk_Woff_Font = "keymanweb-osk.woff";
  public static final String KMFilename_KeyboardsList = "keyboards_list.dat";

  private static Context appContext;

  public static String getVersion() {
    return KMEngineVersion;
  }

  public static void initialize(Context context, KeyboardType keyboardType) {
    appContext = context.getApplicationContext();

    if (!didCopyAssets) {
      copyAssets(appContext);
      renameOldKeyboardFiles(appContext);
      updateOldKeyboardsList(appContext);
      didCopyAssets = true;
    }

    if (keyboardType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      initInAppKeyboard(appContext);
    } else if (keyboardType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      initSystemKeyboard(appContext);
    } else {
      Log.w("KMManager", "Cannot initialize: Invalid keyboard type");
    }
  }

  public static void setInputMethodService(InputMethodService service) {
    IMService = service;
  }

  public static boolean executeHardwareKeystroke(int code, int shift, int lstates) {
    if (SystemKeyboard != null) {
      return executeHardwareKeystroke(code, shift, KeyboardType.KEYBOARD_TYPE_SYSTEM, lstates);
    } else if (InAppKeyboard != null) {
      return executeHardwareKeystroke(code, shift, KeyboardType.KEYBOARD_TYPE_INAPP, lstates);
    }

    return false;
  }

  public static boolean executeHardwareKeystroke(
    int code, int shift, KeyboardType keyboard, int lstates) {
    if (keyboard == KeyboardType.KEYBOARD_TYPE_INAPP) {
      InAppKeyboard.executeHardwareKeystroke(code, shift, lstates);
      return true;
    } else if (keyboard == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      SystemKeyboard.executeHardwareKeystroke(code, shift, lstates);
      return true;
    }

    return false;
  }

  private static void initInAppKeyboard(Context appContext) {
    if (InAppKeyboard == null) {
      if (isDebugMode())
        Log.d("KMManager", "Initializing In-App Keyboard...");
      int kbHeight = appContext.getResources().getDimensionPixelSize(R.dimen.keyboard_height);
      RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT, kbHeight);
      params.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.TRUE);
      InAppKeyboard = new KMKeyboard(appContext, KeyboardType.KEYBOARD_TYPE_INAPP);
      InAppKeyboard.setLayoutParams(params);
      InAppKeyboard.setVerticalScrollBarEnabled(false);
      InAppKeyboard.setHorizontalScrollBarEnabled(false);
      InAppKeyboard.setWebViewClient(new KMInAppKeyboardWebViewClient(appContext));
      InAppKeyboard.addJavascriptInterface(new KMInAppKeyboardJSHandler(appContext), "jsInterface");
      InAppKeyboard.loadKeyboard();
    }
  }

  private static void initSystemKeyboard(Context appContext) {
    if (SystemKeyboard == null) {
      if (isDebugMode())
        Log.d("KMManager", "Initializing System Keyboard...");
      int kbHeight = appContext.getResources().getDimensionPixelSize(R.dimen.keyboard_height);
      RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT, kbHeight);
      params.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, RelativeLayout.TRUE);
      SystemKeyboard = new KMKeyboard(appContext, KeyboardType.KEYBOARD_TYPE_SYSTEM);
      SystemKeyboard.setLayoutParams(params);
      SystemKeyboard.setVerticalScrollBarEnabled(false);
      SystemKeyboard.setHorizontalScrollBarEnabled(false);
      SystemKeyboard.setWebViewClient(new KMSystemKeyboardWebViewClient(appContext));
      SystemKeyboard.addJavascriptInterface(new KMSystemKeyboardJSHandler(appContext), "jsInterface");
      SystemKeyboard.loadKeyboard();
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
    
    /*
    final RelativeLayout overlayLayout = new RelativeLayout(appContext);
    overlayLayout.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT));
    LayoutInflater inflater = (LayoutInflater) appContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
    RelativeLayout overlayView = (RelativeLayout) inflater.inflate(R.layout.overlay_layout, null, false);
    overlayView.setLayoutParams(SystemKeyboard.getLayoutParams());
    overlayView.setBackgroundColor(Color.argb(192, 0, 0, 0));
    overlayView.setClickable(true);
    overlayLayout.addView(overlayView);
    Button activateButton = (Button) overlayView.findViewById(R.id.button1);
    activateButton.setOnClickListener(new OnClickListener(){
      @Override
      public void onClick(View v) {
        mainLayout.removeView(overlayLayout);
        Toast.makeText(context, "Reactivated", Toast.LENGTH_LONG).show();
      }
    });
    */

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
      InAppKeyboard.onResume();
    }
    if (SystemKeyboard != null) {
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
  }

  public static void onConfigurationChanged(Configuration newConfig) {
    // KMKeyboard
    if (InAppKeyboard != null) {
      InAppKeyboard.onConfigurationChanged(newConfig);
    }
    if (SystemKeyboard != null) {
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
    File file;
    try {
      // Copy main files
      copyAsset(context, KMFilename_KeyboardHtml, "", true);
      copyAsset(context, KMFilename_JSEngine, "", true);
      copyAsset(context, KMFilename_KmwCss, "", true);
      copyAsset(context, KMFilename_Osk_Ttf_Font, "", true);
      copyAsset(context, KMFilename_Osk_Woff_Font, "", true);

      // Copy languages
      file = new File(context.getDir("data", Context.MODE_PRIVATE) + "/languages");
      if (!file.exists())
        file.mkdir();
      String[] languageFiles = assetManager.list("languages");
      for (String filename : languageFiles) {
        copyAsset(context, filename, "languages", true);
      }

      // Copy fonts
      file = new File(context.getDir("data", Context.MODE_PRIVATE) + "/fonts");
      if (!file.exists())
        file.mkdir();
      String[] fontFiles = assetManager.list("fonts");
      for (String filename : fontFiles) {
        copyAsset(context, filename, "fonts", false);
      }
    } catch (Exception e) {
      Log.e("Failed to copy assets", "Error: " + e);
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
        directory = directory + "/";
        dirPath = context.getDir("data", Context.MODE_PRIVATE) + "/" + directory;
      } else {
        dirPath = context.getDir("data", Context.MODE_PRIVATE).toString();
      }

      File file = new File(dirPath, filename);
      if (!file.exists() || overwrite) {
        InputStream inputStream = assetManager.open(directory + filename);
        FileOutputStream outputStream = new FileOutputStream(file);
        copyFile(inputStream, outputStream);

        result = 1;
      } else {
        result = 0;
      }
    } catch (Exception e) {
      Log.e("Failed to copy asset", "Error: " + e);
      result = -1;
    }
    return result;
  }

  private static void copyFile(InputStream inputStream, OutputStream outputStream) throws IOException {
    byte[] buffer = new byte[1024];
    int read;
    while ((read = inputStream.read(buffer)) != -1) {
      outputStream.write(buffer, 0, read);
    }
  }

  private static void renameOldKeyboardFiles(Context context) {
    String path = context.getDir("data", Context.MODE_PRIVATE) + "/languages/";
    File dir = new File(path);
    String[] files = dir.list();
    for (String filename : files) {
      if (filename.lastIndexOf("-") < 0) {
        if (filename.equals("us.js")) {
          File kbFile = new File(path, filename);
          kbFile.delete();
        } else {
          String newFilename = filename.substring(0, filename.lastIndexOf(".js")) + "-1.0.js";
          File kbFile = new File(path, filename);
          kbFile.renameTo(new File(path, newFilename));
        }
      } else {
        String oldFilename = filename.substring(0, filename.lastIndexOf("-")) + ".js";
        File kbFile = new File(path, oldFilename);
        if (kbFile.exists())
          kbFile.delete();
      }
    }
  }

  public static void updateOldKeyboardsList(Context context) {
    ArrayList<HashMap<String, String>> kbList = KeyboardPickerActivity.getKeyboardsList(context);
    if (kbList != null && kbList.size() > 0) {
      boolean shouldUpdateList = false;
      boolean shouldClearCache = false;
      HashMap<String, String> kbInfo = kbList.get(0);
      String kbID = kbInfo.get(KMKey_KeyboardID);
      if (kbID.equals("us")) {
        HashMap<String, String> newKbInfo = new HashMap<String, String>();
        newKbInfo.put(KMManager.KMKey_KeyboardID, KMManager.KMDefault_KeyboardID);
        newKbInfo.put(KMManager.KMKey_LanguageID, KMManager.KMDefault_LanguageID);
        newKbInfo.put(KMManager.KMKey_KeyboardName, KMManager.KMDefault_KeyboardName);
        newKbInfo.put(KMManager.KMKey_LanguageName, KMManager.KMDefault_LanguageName);
        newKbInfo.put(KMManager.KMKey_KeyboardVersion,
          getLatestKeyboardFileVersion(context, KMManager.KMDefault_KeyboardID));
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
        String langID = kbInfo.get(KMKey_LanguageID);
        String kbVersion = kbInfo.get(KMManager.KMKey_KeyboardVersion);
        String latestKbVersion = getLatestKeyboardFileVersion(context, kbID);
        if (kbVersion == null || !kbVersion.equals(latestKbVersion)) {
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
        File cache = LanguageListActivity.getCacheFile(appContext);
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

  public static Typeface getFontTypeface(Context context, String fontFilename) {
    Typeface font = null;

    if (fontFilename != null) {
      if (fontFilename.endsWith(".ttf") || fontFilename.endsWith(".otf")) {
        File file = new File(context.getDir("data", Context.MODE_PRIVATE) + "/fonts/" + fontFilename);
        if (file.exists()) {
          font = Typeface.createFromFile(file);
        } else {
          font = null;
        }
      }
    }

    return font;
  }

  public static ArrayList<HashMap<String, String>> getKeyboardsList(Context context) {
    return KeyboardPickerActivity.getKeyboardsList(context);
  }

  public static final class KMKeyboardDownloader {

    public static void download(final Context context, final int languageIndex, final int keyboardIndex, final boolean showProgressDialog) {
      new AsyncTask<Void, Integer, Integer>() {
        private ProgressDialog progressDialog;
        private String languageID = "";
        private String keyboardID = "";
        private String languageName = "";
        private String keyboardName = "";
        private String kbVersion = "1.0";
        private String isCustom = "N";
        private String font = "";
        private String oskFont = null;

        @Override
        protected void onPreExecute() {
          super.onPreExecute();
          if (showProgressDialog) {
            progressDialog = new ProgressDialog(context);
            progressDialog.setMessage("Downloading keyboard...");
            progressDialog.setCancelable(false);
            if (!((Activity) context).isFinishing()) {
              progressDialog.show();
            } else {
              cancel(true);
              progressDialog = null;
            }
          }
        }

        @Override
        protected Integer doInBackground(Void... voids) {
          int ret = -1;
          ArrayList<String> urls = new ArrayList<String>();

          if (isCancelled())
            return ret;

          try {
            JSONArray languages = LanguageListActivity.languages();
            JSONObject options = LanguageListActivity.options();

            if (languages == null || options == null) {
              throw new Exception("Language list is empty");
            }

            JSONObject language = languages.getJSONObject(languageIndex);
            JSONArray keyboards = language.getJSONArray(KMKey_LanguageKeyboards);
            JSONObject keyboard = keyboards.getJSONObject(keyboardIndex);

            keyboardID = keyboard.getString(KMKey_ID);
            languageID = language.getString(KMKey_ID);
            keyboardName = keyboard.getString(KMKey_Name);
            languageName = language.getString(KMKey_Name);
            kbVersion = keyboard.optString(KMKey_KeyboardVersion, "1.0");
            font = keyboard.optString(KMKey_Font, "");
            oskFont = keyboard.optString(KMKey_OskFont, null);
            String kbFilename = keyboard.optString(KMKey_Filename, "");

            String kbUrl = options.getString(KMKey_KeyboardBaseURI) + kbFilename;
            urls.add(kbUrl);
            JSONObject jsonFont = keyboard.optJSONObject(KMKey_Font);
            JSONObject jsonOskFont = keyboard.optJSONObject(KMKey_OskFont);
            String fontBaseUri = options.getString(KMKey_FontBaseURI);
            ArrayList<String> fontUrls = fontUrls(jsonFont, fontBaseUri, true);
            ArrayList<String> oskFontUrls = fontUrls(jsonOskFont, fontBaseUri, true);
            if (fontUrls != null)
              urls.addAll(fontUrls);
            if (oskFontUrls != null) {
              for (String url : oskFontUrls) {
                if (!urls.contains(url))
                  urls.add(url);
              }
            }

            // Notify listeners: onDownloadStarted
            if (kbDownloadEventListeners != null) {
              HashMap<String, String> keyboardInfo = new HashMap<String, String>();
              keyboardInfo.put(KMKey_KeyboardID, keyboardID);
              keyboardInfo.put(KMKey_LanguageID, languageID);
              keyboardInfo.put(KMKey_KeyboardName, keyboardName);
              keyboardInfo.put(KMKey_LanguageName, languageName);
              keyboardInfo.put(KMKey_KeyboardVersion, kbVersion);
              keyboardInfo.put(KMKey_CustomKeyboard, isCustom);
              keyboardInfo.put(KMKey_Font, font);
              if (oskFont != null)
                keyboardInfo.put(KMKey_OskFont, oskFont);
              KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, EventType.KEYBOARD_DOWNLOAD_STARTED, keyboardInfo, 0);
            }

            ret = 1;
            int result = 0;
            for (String url : urls) {
              String directory = "";
              String filename = "";
              if (url.endsWith(".js")) {
                directory = "languages";
                int start = kbFilename.lastIndexOf("/");
                if (start < 0) {
                  start = 0;
                } else {
                  start++;
                }
                if (!kbFilename.contains("-")) {
                  filename = kbFilename.substring(start, kbFilename.length() - 3) + "-" + kbVersion + ".js";
                } else {
                  filename = kbFilename.substring(start);
                }
              } else {
                directory = "fonts";
                filename = "";
              }

              result = FileDownloader.download(context, url, directory, filename);
              if (result < 0) {
                ret = -1;
                break;
              }
            }
          } catch (Exception e) {
            ret = -1;
            Log.e("Keyboard download", "Error: " + e);
          }

          return ret;
        }

        @Override
        protected void onProgressUpdate(Integer... progress) {
          // Do nothing
        }

        @Override
        protected void onPostExecute(Integer result) {
          if (showProgressDialog) {
            if (progressDialog != null && progressDialog.isShowing()) {
              try {
                progressDialog.dismiss();
                progressDialog = null;
              } catch (Exception e) {
                progressDialog = null;
              }
            }
          }

          // Notify listeners: onDownloadFinished
          if (kbDownloadEventListeners != null) {
            HashMap<String, String> keyboardInfo = new HashMap<String, String>();
            keyboardInfo.put(KMKey_KeyboardID, keyboardID);
            keyboardInfo.put(KMKey_LanguageID, languageID);
            keyboardInfo.put(KMKey_KeyboardName, keyboardName);
            keyboardInfo.put(KMKey_LanguageName, languageName);
            keyboardInfo.put(KMKey_KeyboardVersion, kbVersion);
            keyboardInfo.put(KMKey_CustomKeyboard, isCustom);
            keyboardInfo.put(KMKey_Font, font);
            if (oskFont != null)
              keyboardInfo.put(KMKey_OskFont, oskFont);
            KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, EventType.KEYBOARD_DOWNLOAD_FINISHED, keyboardInfo, result);
          }
        }
      }.execute();
    }

    public static void download(final Context context, final String keyboardID, final String languageID, final boolean showProgressDialog) {
      new AsyncTask<Void, Integer, Integer>() {
        private ProgressDialog progressDialog;
        private String languageName = "";
        private String keyboardName = "";
        private String kbVersion = "1.0";
        private String isCustom = "N";
        private String font = "";
        private String oskFont = "";

        @Override
        protected void onPreExecute() {
          super.onPreExecute();
          if (showProgressDialog) {
            progressDialog = new ProgressDialog(context);
            progressDialog.setMessage("Downloading keyboard...");
            progressDialog.setCancelable(false);
            if (!((Activity) context).isFinishing()) {
              progressDialog.show();
            } else {
              cancel(true);
              progressDialog = null;
            }
          }
        }

        @Override
        protected Integer doInBackground(Void... voids) {
          int ret = -1;
          ArrayList<String> urls = new ArrayList<String>();

          if (isCancelled())
            return ret;

          try {
            String exceptionStr = "Invalid keyboard";
            if (languageID == null || languageID.trim().isEmpty() || keyboardID == null || keyboardID.trim().isEmpty())
              throw new Exception(exceptionStr);

            String deviceType = context.getResources().getString(R.string.device_type);
            if (deviceType.equals("AndroidTablet")) {
              deviceType = "androidtablet";
            } else {
              deviceType = "androidphone";
            }

            String jsonUrl = String.format("%slanguages/%s/%s?device=%s", kKeymanApiBaseURL, languageID, keyboardID, deviceType);
            JSONParser jsonParser = new JSONParser();
            JSONObject kbData = jsonParser.getJSONObjectFromUrl(jsonUrl);

            exceptionStr = "Could not reach Keyman server";
            if (kbData == null) {
              throw new Exception(exceptionStr);
            }

            JSONObject options = kbData.optJSONObject(KMKey_Options);
            JSONObject language = kbData.optJSONObject(KMKey_Language);
            exceptionStr = "The keyboard could not be installed";
            if (options == null || language == null) {
              throw new Exception(exceptionStr);
            }

            JSONArray keyboards = language.getJSONArray(KMKey_LanguageKeyboards);
            String kbBaseUri = options.optString(KMKey_KeyboardBaseURI, "");
            String fontBaseUri = options.optString(KMKey_FontBaseURI, "");

            if (keyboards == null || kbBaseUri.isEmpty())
              throw new Exception(exceptionStr);

            JSONObject keyboard = keyboards.getJSONObject(0);
            if (keyboard == null)
              throw new Exception(exceptionStr);

            languageName = language.optString(KMKey_Name, "");
            keyboardName = keyboard.optString(KMKey_Name, "");
            kbVersion = keyboard.optString(KMKey_KeyboardVersion, "1.0");
            font = keyboard.optString(KMKey_Font, "");
            oskFont = keyboard.optString(KMKey_OskFont, null);
            String kbFilename = keyboard.optString(KMKey_Filename, "");

            if (keyboardName.isEmpty() || languageName.isEmpty() || kbFilename.isEmpty())
              throw new Exception(exceptionStr);

            String kbUrl = kbBaseUri + kbFilename;
            urls.add(kbUrl);
            JSONObject jsonFont = keyboard.optJSONObject(KMKey_Font);
            JSONObject jsonOskFont = keyboard.optJSONObject(KMKey_OskFont);
            ArrayList<String> fontUrls = fontUrls(jsonFont, fontBaseUri, true);
            ArrayList<String> oskFontUrls = fontUrls(jsonOskFont, fontBaseUri, true);
            if (fontUrls != null)
              urls.addAll(fontUrls);
            if (oskFontUrls != null) {
              for (String url : oskFontUrls) {
                if (!urls.contains(url))
                  urls.add(url);
              }
            }

            // Notify listeners: onDownloadStarted
            if (kbDownloadEventListeners != null) {
              HashMap<String, String> keyboardInfo = new HashMap<String, String>();
              keyboardInfo.put(KMKey_KeyboardID, keyboardID);
              keyboardInfo.put(KMKey_LanguageID, languageID);
              keyboardInfo.put(KMKey_KeyboardName, keyboardName);
              keyboardInfo.put(KMKey_LanguageName, languageName);
              keyboardInfo.put(KMKey_KeyboardVersion, kbVersion);
              keyboardInfo.put(KMKey_CustomKeyboard, isCustom);
              keyboardInfo.put(KMKey_Font, font);
              if (oskFont != null)
                keyboardInfo.put(KMKey_OskFont, oskFont);
              KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, EventType.KEYBOARD_DOWNLOAD_STARTED, keyboardInfo, 0);
            }

            ret = 1;
            int result = 0;
            for (String url : urls) {
              String directory = "";
              String filename = "";
              if (url.endsWith(".js")) {
                directory = "languages";
                int start = kbFilename.lastIndexOf("/");
                if (start < 0) {
                  start = 0;
                } else {
                  start++;
                }
                if (!kbFilename.contains("-")) {
                  filename = kbFilename.substring(start, kbFilename.length() - 3) + "-" + kbVersion + ".js";
                } else {
                  filename = kbFilename.substring(start);
                }
              } else {
                directory = "fonts";
                filename = "";
              }

              result = FileDownloader.download(context, url, directory, filename);
              if (result < 0) {
                ret = -1;
                break;
              }
            }
          } catch (Exception e) {
            ret = -1;
            Log.e("Keyboard download", "Error: " + e);
          }

          return ret;
        }

        @Override
        protected void onProgressUpdate(Integer... progress) {
          // Do nothing
        }

        @Override
        protected void onPostExecute(Integer result) {
          if (showProgressDialog) {
            if (progressDialog != null && progressDialog.isShowing()) {
              try {
                progressDialog.dismiss();
                progressDialog = null;
              } catch (Exception e) {
                progressDialog = null;
              }
            }
          }

          // Notify listeners: onDownloadFinished
          if (kbDownloadEventListeners != null) {
            HashMap<String, String> keyboardInfo = new HashMap<String, String>();
            keyboardInfo.put(KMKey_KeyboardID, keyboardID);
            keyboardInfo.put(KMKey_LanguageID, languageID);
            keyboardInfo.put(KMKey_KeyboardName, keyboardName);
            keyboardInfo.put(KMKey_LanguageName, languageName);
            keyboardInfo.put(KMKey_KeyboardVersion, kbVersion);
            keyboardInfo.put(KMKey_CustomKeyboard, isCustom);
            keyboardInfo.put(KMKey_Font, font);
            if (oskFont != null)
              keyboardInfo.put(KMKey_OskFont, oskFont);
            KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, EventType.KEYBOARD_DOWNLOAD_FINISHED, keyboardInfo, result);
          }
        }
      }.execute();
    }

    private static ArrayList<String> fontUrls(JSONObject jsonFont, String baseUri, boolean isOskFont) {
      if (jsonFont == null)
        return null;

      ArrayList<String> urls = new ArrayList<String>();
      JSONArray fontSource = jsonFont.optJSONArray(KMKey_FontSource);
      if (fontSource != null) {
        int fcCount = fontSource.length();
        for (int i = 0; i < fcCount; i++) {
          String fontSourceString;
          try {
            fontSourceString = fontSource.getString(i);
            if (fontSourceString.endsWith(".ttf") || fontSourceString.endsWith(".otf")) {
              urls.add(baseUri + fontSourceString);
            } else if (isOskFont && (fontSourceString.endsWith(".svg") || fontSourceString.endsWith(".woff"))) {
              urls.add(baseUri + fontSourceString);
            } else if (isOskFont && fontSourceString.contains(".svg#")) {
              String fontFilename = fontSourceString.substring(0, fontSourceString.indexOf(".svg#") + 5);
              urls.add(baseUri + fontFilename);
            }
          } catch (JSONException e) {
            return null;
          }
        }
      } else {
        String fontSourceString;
        try {
          fontSourceString = jsonFont.getString(KMKey_FontSource);
          if (fontSourceString.endsWith(".ttf") || fontSourceString.endsWith(".otf")) {
            urls.add(baseUri + fontSourceString);
          } else if (isOskFont && (fontSourceString.endsWith(".svg") || fontSourceString.endsWith(".woff"))) {
            urls.add(baseUri + fontSourceString);
          } else if (isOskFont && fontSourceString.contains(".svg#")) {
            String fontFilename = fontSourceString.substring(0, fontSourceString.indexOf(".svg#") + 5);
            urls.add(baseUri + fontFilename);
          }
        } catch (JSONException e) {
          return null;
        }
      }

      return urls;
    }
  }

  public static final class KMCustomKeyboardDownloader {

    public static void download(final Context context, final String jsonUrl, final boolean isDirect, final boolean showProgressDialog) {
      new AsyncTask<Void, Integer, Integer>() {
        private ProgressDialog progressDialog;
        private String keyboardID = "";
        private String languageID = "";
        private String keyboardName = "";
        private String languageName = "";
        private String kbVersion = "1.0";
        private String isCustom = "Y";
        private String font = "";
        private String oskFont = "";

        @Override
        protected void onPreExecute() {
          super.onPreExecute();
          if (showProgressDialog) {
            progressDialog = new ProgressDialog(context);
            progressDialog.setMessage("Downloading keyboard...");
            progressDialog.setCancelable(false);
            if (!((Activity) context).isFinishing()) {
              progressDialog.show();
            } else {
              cancel(true);
              progressDialog = null;
            }
          }
        }

        @Override
        protected Integer doInBackground(Void... voids) {
          int ret = -1;
          ArrayList<String> urls = new ArrayList<String>();

          if (isCancelled())
            return ret;

          try {
            JSONParser jsonParser = new JSONParser();
            JSONObject customKb = null;
            if (isDirect) {
              customKb = jsonParser.getJSONObjectFromUrl(jsonUrl);
            } else {
              String deviceType = context.getResources().getString(R.string.device_type);
              if (deviceType.equals("AndroidTablet")) {
                deviceType = "androidtablet";
              } else {
                deviceType = "androidphone";
              }

              String encodedUrl = URLEncoder.encode(jsonUrl, "utf-8");
              String remoteUrl = String.format("%s%s&device=%s", kKeymanApiRemoteURL, encodedUrl, deviceType);
              customKb = jsonParser.getJSONObjectFromUrl(remoteUrl);
            }

            String exceptionStr = "Failed to fetch JSON object from the URL";
            if (customKb == null) {
              throw new Exception(exceptionStr);
            }

            JSONObject options = customKb.optJSONObject(KMKey_Options);
            exceptionStr = "The keyboard could not be installed";
            if (options == null) {
              throw new Exception(exceptionStr);
            }

            String kbBaseUri = options.optString(KMKey_KeyboardBaseURI, "");
            String fontBaseUri = options.optString(KMKey_FontBaseURI, "");

            JSONObject keyboard = customKb.getJSONObject(KMKey_Keyboard);

            if (keyboard == null || kbBaseUri.isEmpty()) {
              throw new Exception(exceptionStr);
            }

            JSONArray languages = keyboard.optJSONArray(KMKey_Languages);

            keyboardID = keyboard.optString(KMKey_ID, "");
            keyboardName = keyboard.optString(KMKey_Name, "");
            kbVersion = keyboard.optString(KMKey_KeyboardVersion, "1.0");
            font = keyboard.optString(KMKey_Font, "").replace("\"" + KMManager.KMKey_Filename + "\"", "\"" + KMManager.KMKey_FontSource + "\"");
            oskFont = keyboard.optString(KMKey_OskFont, "").replace("\"" + KMManager.KMKey_Filename + "\"", "\"" + KMManager.KMKey_FontSource + "\"");
            if (oskFont.isEmpty()) {
              oskFont = null;
            }
            String kbFilename = keyboard.optString(KMKey_Filename, "");

            if (languages == null || keyboardID.isEmpty() || keyboardName.isEmpty() || kbFilename.isEmpty()) {
              throw new Exception(exceptionStr);
            }

            String kbUrl = kbBaseUri + kbFilename;
            urls.add(kbUrl);
            JSONObject jsonFont = keyboard.optJSONObject(KMKey_Font);
            JSONObject jsonOskFont = keyboard.optJSONObject(KMKey_OskFont);
            ArrayList<String> fontUrls = fontUrls(jsonFont, fontBaseUri, true);
            ArrayList<String> oskFontUrls = fontUrls(jsonOskFont, fontBaseUri, true);
            if (fontUrls != null) {
              urls.addAll(fontUrls);
            }
            if (oskFontUrls != null) {
              for (String url : oskFontUrls) {
                if (!urls.contains(url)) {
                  urls.add(url);
                }
              }
            }

            languageID = "";
            languageName = "";
            int langCount = languages.length();
            for (int i = 0; i < langCount; i++) {
              languageID += languages.getJSONObject(i).getString(KMKey_ID);
              languageName += languages.getJSONObject(i).getString(KMKey_Name);
              if (i < langCount - 1) {
                languageID += ";";
                languageName += ";";
              }
            }

            // Notify listeners: onDownloadStarted
            if (kbDownloadEventListeners != null) {
              HashMap<String, String> keyboardInfo = new HashMap<String, String>();
              keyboardInfo.put(KMKey_KeyboardID, keyboardID);
              keyboardInfo.put(KMKey_LanguageID, languageID);
              keyboardInfo.put(KMKey_KeyboardName, keyboardName);
              keyboardInfo.put(KMKey_LanguageName, languageName);
              keyboardInfo.put(KMKey_KeyboardVersion, kbVersion);
              keyboardInfo.put(KMKey_CustomKeyboard, isCustom);
              keyboardInfo.put(KMKey_Font, font);
              if (oskFont != null) {
                keyboardInfo.put(KMKey_OskFont, oskFont);
              }
              KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, EventType.KEYBOARD_DOWNLOAD_STARTED, keyboardInfo, 0);
            }

            ret = 1;
            int result = 0;
            for (String url : urls) {
              String directory = "";
              String filename = "";
              if (url.endsWith(".js")) {
                directory = "languages";
                int start = kbFilename.lastIndexOf("/");
                if (start < 0) {
                  start = 0;
                } else {
                  start++;
                }
                if (!kbFilename.contains("-")) {
                  filename = kbFilename.substring(start, kbFilename.length() - 3) + "-" + kbVersion + ".js";
                } else {
                  filename = kbFilename.substring(start);
                }
              } else {
                directory = "fonts";
                filename = "";
              }

              result = FileDownloader.download(context, url, directory, filename);
              if (result < 0) {
                ret = -1;
                break;
              }
            }
          } catch (Exception e) {
            ret = -1;
            Log.e("Keyboard download", "Error: " + e);
            e.printStackTrace();
          }

          return ret;
        }

        @Override
        protected void onProgressUpdate(Integer... progress) {
          // Do nothing
        }

        @Override
        protected void onPostExecute(Integer result) {
          if (showProgressDialog) {
            if (progressDialog != null && progressDialog.isShowing()) {
              try {
                progressDialog.dismiss();
                progressDialog = null;
              } catch (Exception e) {
                progressDialog = null;
              }
            }
          }

          // Notify listeners: onDownloadFinished
          if (kbDownloadEventListeners != null) {
            HashMap<String, String> keyboardInfo = new HashMap<String, String>();
            keyboardInfo.put(KMKey_KeyboardID, keyboardID);
            keyboardInfo.put(KMKey_LanguageID, languageID);
            keyboardInfo.put(KMKey_KeyboardName, keyboardName);
            keyboardInfo.put(KMKey_LanguageName, languageName);
            keyboardInfo.put(KMKey_KeyboardVersion, kbVersion);
            keyboardInfo.put(KMKey_CustomKeyboard, isCustom);
            keyboardInfo.put(KMKey_Font, font);
            if (oskFont != null)
              keyboardInfo.put(KMKey_OskFont, oskFont);
            KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, EventType.KEYBOARD_DOWNLOAD_FINISHED, keyboardInfo, result);
          }

          if (result > 0) {
            KMManager.getActiveKeyboard().loadKeyboard();
          }
        }
      }.execute();
    }

    private static ArrayList<String> fontUrls(JSONObject jsonFont, String baseUri, boolean isOskFont) {
      if (jsonFont == null)
        return null;

      ArrayList<String> urls = new ArrayList<String>();
      JSONArray fontSource = jsonFont.optJSONArray(KMKey_FontSource);
      if (fontSource == null)
        fontSource = jsonFont.optJSONArray(KMKey_Filename); // Font filename is deprecated

      if (fontSource != null) {
        int fcCount = fontSource.length();
        for (int i = 0; i < fcCount; i++) {
          String fontSourceString;
          try {
            fontSourceString = fontSource.getString(i);
            if (fontSourceString.endsWith(".ttf") || fontSourceString.endsWith(".otf")) {
              urls.add(baseUri + fontSourceString);
            } else if (isOskFont && (fontSourceString.endsWith(".svg") || fontSourceString.endsWith(".woff"))) {
              urls.add(baseUri + fontSourceString);
            } else if (isOskFont && fontSourceString.contains(".svg#")) {
              String fontFilename = fontSourceString.substring(0, fontSourceString.indexOf(".svg#") + 5);
              urls.add(baseUri + fontFilename);
            }
          } catch (JSONException e) {
            return null;
          }
        }
      } else {
        String fontSourceString;
        try {
          fontSourceString = jsonFont.optString(KMKey_FontSource, null);
          if (fontSourceString == null)
            fontSourceString = jsonFont.getString(KMKey_Filename); // Font filename is deprecated

          if (fontSourceString.endsWith(".ttf") || fontSourceString.endsWith(".otf")) {
            urls.add(baseUri + fontSourceString);
          } else if (isOskFont && (fontSourceString.endsWith(".svg") || fontSourceString.endsWith(".woff"))) {
            urls.add(baseUri + fontSourceString);
          } else if (isOskFont && fontSourceString.contains(".svg#")) {
            String fontFilename = fontSourceString.substring(0, fontSourceString.indexOf(".svg#") + 5);
            urls.add(baseUri + fontFilename);
          }
        } catch (JSONException e) {
          return null;
        }
      }

      return urls;
    }
  }

  private static final class FileDownloader {

    public static int download(Context context, String urlStr, String directory, String filename) {
      int ret = -1;
      HttpURLConnection urlConnection = null;
      String fileName = "";
      String tmpFileName = "";
      File tmpFile = null;
      File file = null;

      try {
        if (directory == null)
          directory = "";

        directory = directory.trim();

        String dirPath;
        if (directory.length() != 0) {
          directory = directory + "/";
          dirPath = context.getDir("data", Context.MODE_PRIVATE) + "/" + directory;
        } else {
          dirPath = context.getDir("data", Context.MODE_PRIVATE).toString();
        }

        URL url = new URL(urlStr);
        filename = filename.trim();
        if (filename == null || filename.isEmpty()) {
          fileName = url.getFile().substring(url.getFile().lastIndexOf('/') + 1);
          if (fileName.lastIndexOf(".js") > 0 && !fileName.contains("-")) {
            fileName = fileName.substring(0, filename.lastIndexOf(".js")) + "-1.0.js";
          }
        } else {
          fileName = filename;
        }
        tmpFileName = String.format("%s.tmp", fileName);
        file = new File(dirPath, fileName);
        tmpFile = new File(dirPath, tmpFileName);

        urlConnection = (HttpURLConnection) url.openConnection();
        urlConnection.setRequestProperty("Cache-Control", "no-cache");
        urlConnection.setConnectTimeout(10000);
        urlConnection.setReadTimeout(10000);
        InputStream binStream = new BufferedInputStream(urlConnection.getInputStream(), 4096);
        byte[] buff = new byte[4096];

        FileOutputStream fos = new FileOutputStream(tmpFile);

        int len;
        while ((len = binStream.read(buff)) != -1) {
          fos.write(buff, 0, len);
        }

        fos.flush();
        fos.close();
        binStream.close();

        ret = 1;
      } catch (Exception e) {
        ret = -1;
        Log.e("FD: Download failed!", "Error: " + e);
      } finally {
        if (ret > 0) {
          if (tmpFile.exists()) {
            if (file.exists()) {
              file.delete();
            }
            if (!tmpFile.renameTo(file)) {
              ret = -1;
            } else if (isDebugMode()) {
              Log.d("FD: Download finished", "Filename = " + file.toString());
            }
          } else {
            ret = -1;
          }
        } else {
          if (file.exists()) {
            file.delete();
          }
          if (tmpFile.exists()) {
            tmpFile.delete();
          }
          if (isDebugMode()) {
            Log.d("FD: Could not download", "Filename = " + file.toString());
          }
        }

        if (urlConnection != null) {
          urlConnection.disconnect();
        }
      }

      return ret;
    }
  }

  public static boolean addKeyboard(Context context, HashMap<String, String> keyboardInfo) {
    return KeyboardPickerActivity.addKeyboard(context, keyboardInfo);
  }

  public static boolean removeKeyboard(Context context, int position) {
    return KeyboardPickerActivity.removeKeyboard(context, position);
  }

  public static boolean setKeyboard(String keyboardID, String languageID) {
    boolean result1 = false;
    boolean result2 = false;

    if (InAppKeyboard != null && InAppKeyboardLoaded)
      result1 = InAppKeyboard.setKeyboard(keyboardID, languageID);

    if (SystemKeyboard != null && SystemKeyboardLoaded)
      result2 = SystemKeyboard.setKeyboard(keyboardID, languageID);

    return (result1 || result2);
  }

  public static boolean setKeyboard(String keyboardID, String languageID, String keyboardName, String languageName, String kFont, String kOskFont) {
    boolean result1 = false;
    boolean result2 = false;

    if (InAppKeyboard != null && InAppKeyboardLoaded)
      result1 = InAppKeyboard.setKeyboard(keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);

    if (SystemKeyboard != null && SystemKeyboardLoaded)
      result2 = SystemKeyboard.setKeyboard(keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);

    return (result1 || result2);
  }

  public static boolean setKeyboard(Context context, int position) {
    HashMap<String, String> keyboardInfo = KeyboardPickerActivity.getKeyboardInfo(context, position);
    if (keyboardInfo == null)
      return false;

    String kbId = keyboardInfo.get(KMManager.KMKey_KeyboardID);
    String langId = keyboardInfo.get(KMManager.KMKey_LanguageID);
    String kbName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
    String langName = keyboardInfo.get(KMManager.KMKey_LanguageName);
    String kFont = keyboardInfo.get(KMManager.KMKey_Font);
    String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
    return setKeyboard(kbId, langId, kbName, langName, kFont, kOskFont);
  }

  public static void switchToNextKeyboard(Context context) {
    int index = KeyboardPickerActivity.getCurrentKeyboardIndex(context);
    index++;
    HashMap<String, String> kbInfo = KeyboardPickerActivity.getKeyboardInfo(context, index);
    if (kbInfo == null) {
      index = 0;
      kbInfo = KeyboardPickerActivity.getKeyboardInfo(context, index);
    }

    String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
    String langId = kbInfo.get(KMManager.KMKey_LanguageID);
    String kbName = kbInfo.get(KMManager.KMKey_KeyboardName);
    String langName = kbInfo.get(KMManager.KMKey_LanguageName);
    String kFont = kbInfo.get(KMManager.KMKey_Font);
    String kOskFont = kbInfo.get(KMManager.KMKey_OskFont);
    KMManager.getActiveKeyboard().setKeyboard(kbId, langId, kbName, langName, kFont, kOskFont);
  }

  public static KMKeyboard getActiveKeyboard() {
    Log.d("KMM", "InApp: " + String.valueOf(InAppKeyboard != null) + " SystemKeyboard: " + String.valueOf(SystemKeyboard != null));
    Log.d("KMM", "InAppKeyboardLoaded: " + String.valueOf(InAppKeyboardLoaded) + " SystemKeyboardLoaded: " + String.valueOf(SystemKeyboardLoaded));
    if (InAppKeyboard != null && InAppKeyboardLoaded) {
      return InAppKeyboard;
    } else if (SystemKeyboard != null && SystemKeyboardLoaded) {
      return SystemKeyboard;
    } else {
      Log.w("KMManager", "Cannot determine active keyboard");
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

  public static KeyboardState getKeyboardState(Context context, String keyboardID, String languageID) {
    KeyboardState kbState = KeyboardState.KEYBOARD_STATE_UNDEFINED;
    if (keyboardID == null || languageID == null)
      return kbState;

    keyboardID = keyboardID.trim();
    languageID = languageID.trim();
    if (keyboardID.isEmpty() || languageID.isEmpty())
      return kbState;

    String latestVersion = getLatestKeyboardFileVersion(context, keyboardID);
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
        if (kbInfo != null)
          kbVersion = kbInfo.get(KMManager.KMKey_KeyboardVersion);

        try {
          if (kbVersion != null && compareVersions(kbVersion, latestVersion) > 0)
            kbState = KeyboardState.KEYBOARD_STATE_NEEDS_UPDATE;
        } catch (Exception e) {
          Log.e("getKeyboardState", "Error: " + e);
        }
      }
    }

    return kbState;
  }

  public static String getLatestKeyboardFileVersion(Context context, String keyboardID) {
    String kbFileVersion = null;
    String path = context.getDir("data", Context.MODE_PRIVATE) + "/languages/";
    File dir = new File(path);
    String[] files = dir.list();
    if (files == null)
      return kbFileVersion;

    for (String file : files) {
      if (!file.endsWith(".js"))
        continue;

      String base = String.format("%s-", keyboardID);
      int index = file.indexOf(base);
      if (index == 0) {
        int firstIndex = base.length();
        int lastIndex = file.lastIndexOf(".js");
        String v = file.substring(firstIndex, lastIndex);
        if (kbFileVersion != null) {
          if (compareVersions(v, kbFileVersion) > 0) {
            kbFileVersion = v;
          }
        } else if (compareVersions(v, v) == 0) {
          kbFileVersion = v;
        }
      }
    }

    return kbFileVersion;
  }

  public static int compareVersions(String v1, String v2) {
    // returns;
    // -2 if v1 or v2 is invalid
    // 0 if v1 = v2
    // -1 if v1 < v2
    // 1 if v1 > v2

    if (v1 == null || v2 == null) {
      return -2;
    }

    if (v1.isEmpty() || v2.isEmpty()) {
      return -2;
    }

    String[] v1Values = v1.split("\\.");
    String[] v2Values = v2.split("\\.");

    int len = (v1Values.length >= v2Values.length ? v1Values.length : v2Values.length);
    for (int i = 0; i < len; i++) {
      String vStr1 = "0";
      if (i < v1Values.length) {
        vStr1 = v1Values[i];
      }

      String vStr2 = "0";
      if (i < v2Values.length) {
        vStr2 = v2Values[i];
      }

      Integer vInt1 = parseInteger(vStr1);
      Integer vInt2 = parseInteger(vStr2);
      int iV1, iV2, iV1_, iV2_;

      if (vInt1 != null) {
        iV1 = vInt1.intValue();
        iV1_ = 0;
      } else {
        iV1 = 0;
        iV1_ = 0;
      }

      if (vInt2 != null) {
        iV2 = vInt2.intValue();
        iV2_ = 0;
      } else {
        iV2 = 0;
        iV2_ = 0;
      }

      if (vInt1 == null) {
        if (i != (v1Values.length - 1)) {
          return -2;
        }

        if (vStr1.toLowerCase().endsWith("b")) {
          Integer vInt1_ = parseInteger(vStr1.substring(0, vStr1.length() - 1));
          if (vInt1_ == null) {
            return -2;
          }

          iV1 = vInt1_.intValue();
          iV1_ = -100;
        } else if (vStr1.toLowerCase().endsWith("a")) {
          Integer vInt1_ = parseInteger(vStr1.substring(0, vStr1.length() - 1));
          if (vInt1_ == null) {
            return -2;
          }

          iV1 = vInt1_.intValue();
          iV1_ = -200;
        } else {
          return -2;
        }
      }

      if (vInt2 == null) {
        if (i != (v2Values.length - 1)) {
          return -2;
        }

        if (vStr2.toLowerCase().endsWith("b")) {
          Integer vInt2_ = parseInteger(vStr2.substring(0, vStr2.length() - 1));
          if (vInt2_ == null) {
            return -2;
          }

          iV2 = vInt2_.intValue();
          iV2_ = -100;
        } else if (vStr2.toLowerCase().endsWith("a")) {
          Integer vInt2_ = parseInteger(vStr2.substring(0, vStr2.length() - 1));
          if (vInt2_ == null) {
            return -2;
          }

          iV2 = vInt2_.intValue();
          iV2_ = -200;
        } else {
          return -2;
        }
      }

      if (iV1 == iV2) {
        if (iV1_ == iV2_) {
          continue;
        }
        if (iV1_ < iV2_) {
          return -1;
        }
        if (iV1_ > iV2_) {
          return 1;
        }
      } else if (iV1 < iV2) {
        return -1;
      } else if (iV1 > iV2) {
        return 1;
      }
    }

    return 0;
  }

  private static Integer parseInteger(String s) {
    Integer retVal = null;
    try {
      int i = Integer.parseInt(s);
      retVal = new Integer(i);
    } catch (Exception e) {
      retVal = null;
    }

    return retVal;
  }

  public static void addKeyboardEventListener(OnKeyboardEventListener listener) {
    KMTextView.addOnKeyboardEventListener(listener);
    KMKeyboard.addOnKeyboardEventListener(listener);
  }

  public static void removeKeyboardEventListener(OnKeyboardEventListener listener) {
    KMTextView.removeOnKeyboardEventListener(listener);
    KMKeyboard.removeOnKeyboardEventListener(listener);
  }

  public static void addKeyboardDownloadEventListener(OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners == null) {
      kbDownloadEventListeners = new ArrayList<OnKeyboardDownloadEventListener>();
    }

    if (listener != null && !kbDownloadEventListeners.contains(listener)) {
      kbDownloadEventListeners.add(listener);
    }
  }

  public static void removeKeyboardDownloadEventListener(OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners != null) {
      kbDownloadEventListeners.remove(listener);
    }
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

  public static void setShouldAllowSetKeyboard(boolean value) {
    shouldAllowSetKeyboard = value;
    if (shouldAllowSetKeyboard == false) {
      setKeyboard(KMDefault_KeyboardID, KMDefault_LanguageID, KMDefault_KeyboardName, KMDefault_LanguageName, KMDefault_KeyboardFont, null);
    }
  }

  public static boolean shouldAllowSetKeyboard() {
    return shouldAllowSetKeyboard;
  }

  public static void showKeyboardPicker(Context context, KeyboardType kbType) {
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      Intent i = new Intent(context, KeyboardPickerActivity.class);
      i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
      context.startActivity(i);
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      Intent i = new Intent(context, KeyboardPickerActivity.class);
      i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_WHEN_TASK_RESET);
      i.addFlags(Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS);
      i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK);
      i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
      context.startActivity(i);
    }
  }

  public static void setKeyboardPickerFont(Typeface typeface) {
    KeyboardPickerActivity.listFont = typeface;
  }

  public static void showLanguageList(Context context) {
    KeyboardPickerActivity.showLanguageList(context);
  }

  public static boolean updateText(KeyboardType kbType, String text) {
    boolean result = false;
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard != null && InAppKeyboardLoaded && !InAppKeyboardShouldIgnoreTextChange) {
        String kmText = "";
        if (text != null) {
          kmText = text.toString().replace("\\", "\\u005C").replace("'", "\\u0027").replace("\n", "\\n");
        }
        InAppKeyboard.loadUrl(String.format("javascript:updateKMText('%s')", kmText));
        result = true;
      }

      InAppKeyboardShouldIgnoreTextChange = false;
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard != null && SystemKeyboardLoaded && !SystemKeyboardShouldIgnoreTextChange) {
        String kmText = "";
        if (text != null) {
          kmText = text.toString().replace("\\", "\\u005C").replace("'", "\\u0027").replace("\n", "\\n");
        }
        SystemKeyboard.loadUrl(String.format("javascript:updateKMText('%s')", kmText));
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
        InAppKeyboard.loadUrl(String.format("javascript:updateKMSelectionRange(%d,%d)", selStart, selEnd));
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

        SystemKeyboard.loadUrl(String.format("javascript:updateKMSelectionRange(%d,%d)", selStart, selEnd));
        result = true;
      }

      SystemKeyboardShouldIgnoreSelectionChange = false;
    }

    return result;
  }

  public static void resetContext(KeyboardType kbType) {
    if (kbType == KeyboardType.KEYBOARD_TYPE_INAPP) {
      if (InAppKeyboard != null && InAppKeyboardLoaded) {
        InAppKeyboard.loadUrl("javascript:resetContext()");
      }
    } else if (kbType == KeyboardType.KEYBOARD_TYPE_SYSTEM) {
      if (SystemKeyboard != null && SystemKeyboardLoaded) {
        SystemKeyboard.loadUrl("javascript:resetContext()");
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

  public static boolean keyboardExists(Context context, String keyboardID, String languageID) {
    boolean result = false;

    if (keyboardID != null & languageID != null) {
      String kbKey = String.format("%s_%s", languageID, keyboardID);
      result = KeyboardPickerActivity.containsKeyboard(context, kbKey);
    }

    return result;
  }

  public static boolean isHelpBubbleEnabled() {
    return KMManager.getActiveKeyboard().isHelpBubbleEnabled;
  }

  public static void setHelpBubbleEnabled(boolean newValue) {
    KMManager.getActiveKeyboard().isHelpBubbleEnabled = newValue;
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
      if (url.endsWith(KMFilename_KeyboardHtml)) {
        InAppKeyboardLoaded = false;
      }
    }

    @Override
    public void onPageFinished(WebView view, String url) {
      if (url.endsWith(KMFilename_KeyboardHtml)) {
        InAppKeyboardLoaded = true;
        if (isDebugMode()) {
          Log.d("KMManager", "In-App Keyboard loaded.");
        }

        if (!InAppKeyboard.keyboardSet) {
          SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
          int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
          if (index < 0) {
            index = 0;
          }
          HashMap<String, String> keyboardInfo = KMManager.getKeyboardInfo(context, index);
          if (keyboardInfo != null) {
            String kbId = keyboardInfo.get(KMManager.KMKey_KeyboardID);
            String langId = keyboardInfo.get(KMManager.KMKey_LanguageID);
            String kbName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
            String langName = keyboardInfo.get(KMManager.KMKey_LanguageName);
            String kFont = keyboardInfo.get(KMManager.KMKey_Font);
            String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
            InAppKeyboard.setKeyboard(kbId, langId, kbName, langName, kFont, kOskFont);
          } else {
            InAppKeyboard.setKeyboard(KMDefault_KeyboardID, KMDefault_LanguageID, KMDefault_KeyboardName, KMDefault_LanguageName, KMDefault_KeyboardFont, null);
          }
        }

        Handler handler = new Handler();
        handler.postDelayed(new Runnable() {
          @Override
          public void run() {
            SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
            if (prefs.getBoolean(KMManager.KMKey_ShouldShowHelpBubble, true)) {
              InAppKeyboard.loadUrl("javascript:showHelpBubble()");
            }
          }
        }, 2000);

        KeyboardEventHandler.notifyListeners(KMTextView.kbEventListeners, KeyboardType.KEYBOARD_TYPE_INAPP, EventType.KEYBOARD_LOADED, null);
      }

      shouldOverrideUrlLoading(view, url);
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
      if (url.indexOf("hideKeyboard") >= 0) {
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
        String text = "";
        String[] values = t.split("\\,");
        int length = values.length;
        for (int i = 0; i < length; i++) {
          if (values[i].startsWith("0x")) {
            int c = Integer.parseInt(values[i].substring(2), 16);
            text += String.valueOf((char) c);
          }
        }

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
          InAppKeyboard.specialOskFont = "keymanweb-osk.ttf";
        }

        String keys = url.substring(start, end);

        String[] keyList = keys.split("\\;");
        int klCount = keyList.length;
        InAppKeyboard.subKeysList = new ArrayList<HashMap<String, String>>();
        for (int i = 0; i < klCount; i++) {
          String[] values = keyList[i].split("\\:");
          String keyId = "";
          String keyText = "";
          if (values.length == 2) {
            keyId = values[0];
            keyText = values[1];
          } else if (values.length == 1) {
            keyId = values[0];
            keyText = values[0];
            int index = keyText.indexOf("-");
            if (index >= 0) {
              keyText = keyText.substring(index + 1);
            }
          }

          HashMap<String, String> hashMap = new HashMap<String, String>();
          hashMap.put("keyId", keyId);
          hashMap.put("keyText", keyText);
          InAppKeyboard.subKeysList.add(hashMap);
        }
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
      if (url.endsWith(KMFilename_KeyboardHtml)) {
        SystemKeyboardLoaded = false;
      }
    }

    @Override
    public void onPageFinished(WebView view, String url) {
      if (url.endsWith(KMFilename_KeyboardHtml)) {
        SystemKeyboardLoaded = true;
        if (isDebugMode()) {
          Log.d("KMManager", "System Keyboard loaded.");
        }

        if (!SystemKeyboard.keyboardSet) {
          SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
          int index = prefs.getInt(KMManager.KMKey_UserKeyboardIndex, 0);
          if (index < 0) {
            index = 0;
          }
          HashMap<String, String> keyboardInfo = KMManager.getKeyboardInfo(context, index);
          if (keyboardInfo != null) {
            String kbId = keyboardInfo.get(KMManager.KMKey_KeyboardID);
            String langId = keyboardInfo.get(KMManager.KMKey_LanguageID);
            String kbName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
            String langName = keyboardInfo.get(KMManager.KMKey_LanguageName);
            String kFont = keyboardInfo.get(KMManager.KMKey_Font);
            String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);
            SystemKeyboard.setKeyboard(kbId, langId, kbName, langName, kFont, kOskFont);
          } else {
            SystemKeyboard.setKeyboard(KMDefault_KeyboardID, KMDefault_LanguageID, KMDefault_KeyboardName, KMDefault_LanguageName, KMDefault_KeyboardFont, null);
          }
        }

        Handler handler = new Handler();
        handler.postDelayed(new Runnable() {
          @Override
          public void run() {
            SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
            if (prefs.getBoolean(KMManager.KMKey_ShouldShowHelpBubble, true)) {
              SystemKeyboard.loadUrl("javascript:showHelpBubble()");
            }
          }
        }, 2000);

        KeyboardEventHandler.notifyListeners(KMTextView.kbEventListeners, KeyboardType.KEYBOARD_TYPE_SYSTEM, EventType.KEYBOARD_LOADED, null);
      }

      shouldOverrideUrlLoading(view, url);
    }

    @Override
    public boolean shouldOverrideUrlLoading(WebView view, String url) {
      if (url.indexOf("hideKeyboard") >= 0) {
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
            if (sysKbGlobeKeyAction == GlobeKeyAction.GLOBE_KEY_ACTION_SHOW_MENU) {
              showKeyboardPicker(context, KeyboardType.KEYBOARD_TYPE_SYSTEM);
            } else if (sysKbGlobeKeyAction == GlobeKeyAction.GLOBE_KEY_ACTION_SWITCH_TO_NEXT_KEYBOARD) {
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
          SystemKeyboard.showHelpBubble(context, fx, fy);
        }
      } else if (url.indexOf("showKeyPreview") >= 0) {
        String deviceType = context.getResources().getString(R.string.device_type);
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
        String text = "";
        String[] values = t.split("\\,");
        int length = values.length;
        for (int i = 0; i < length; i++) {
          if (values[i].startsWith("0x")) {
            int c = Integer.parseInt(values[i].substring(2), 16);
            text += String.valueOf((char) c);
          }
        }

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
          SystemKeyboard.specialOskFont = "keymanweb-osk.ttf";
        }

        String keys = url.substring(start, end);

        String[] keyList = keys.split("\\;");
        int klCount = keyList.length;
        SystemKeyboard.subKeysList = new ArrayList<HashMap<String, String>>();
        for (int i = 0; i < klCount; i++) {
          String[] values = keyList[i].split("\\:");
          String keyId = "";
          String keyText = "";
          if (values.length == 2) {
            keyId = values[0];
            keyText = values[1];
          } else if (values.length == 1) {
            keyId = values[0];
            keyText = values[0];
            int index = keyText.indexOf("-");
            if (index >= 0)
              keyText = keyText.substring(index + 1);
          }

          HashMap<String, String> hashMap = new HashMap<String, String>();
          hashMap.put("keyId", keyId);
          hashMap.put("keyText", keyText);
          SystemKeyboard.subKeysList.add(hashMap);
        }
      }

      return false;
    }
  }

  private static final class KMInAppKeyboardJSHandler {
    private Context context;

    KMInAppKeyboardJSHandler(Context context) {
      this.context = context;
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public String getDeviceType() {
      return context.getResources().getString(R.string.device_type);
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public int getKeyboardHeight() {
      int kbHeight = context.getResources().getDimensionPixelSize(R.dimen.keyboard_height);
      kbHeight -= kbHeight % 20;
      return kbHeight;
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public int getKeyboardWidth() {
      DisplayMetrics dms = context.getResources().getDisplayMetrics();
      int kbWidth = (int) (dms.widthPixels / dms.density);
      return kbWidth;
    }

    @JavascriptInterface
    public void setIsChiral(boolean isChiral) {
      InAppKeyboard.setChirality(isChiral);
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public void insertText(final int dn, final String s) {
      Handler mainLoop = new Handler(Looper.getMainLooper());
      mainLoop.post(new Runnable() {
        public void run() {
          if (InAppKeyboard.subKeysWindow != null || KMTextView.activeView == null || KMTextView.activeView.getClass() != KMTextView.class) {
            if (KMTextView.activeView == null)
              Log.w("IAK: JS Handler", "insertText failed: activeView is null");
            return;
          }

          InAppKeyboard.dismissHelpBubble();

          KMTextView textView = (KMTextView) KMTextView.activeView;
          textView.beginBatchEdit();

          int start = textView.getSelectionStart();
          int end = textView.getSelectionEnd();
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

  private static final class KMSystemKeyboardJSHandler {
    private Context context;

    KMSystemKeyboardJSHandler(Context context) {
      this.context = context;
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public String getDeviceType() {
      return context.getResources().getString(R.string.device_type);
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public int getKeyboardHeight() {
      int kbHeight = context.getResources().getDimensionPixelSize(R.dimen.keyboard_height);
      kbHeight -= kbHeight % 20;
      return kbHeight;
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public int getKeyboardWidth() {
      DisplayMetrics dms = context.getResources().getDisplayMetrics();
      int kbWidth = (int) (dms.widthPixels / dms.density);
      return kbWidth;
    }

    @JavascriptInterface
    public void setIsChiral(boolean isChiral) {
      SystemKeyboard.setChirality(isChiral);
    }

    // This annotation is required in Jelly Bean and later:
    @JavascriptInterface
    public void insertText(final int dn, final String s) {
      Handler mainLoop = new Handler(Looper.getMainLooper());
      mainLoop.post(new Runnable() {
        public void run() {
          if (SystemKeyboard.subKeysWindow != null) {
            return;
          }

          InputConnection ic = IMService.getCurrentInputConnection();
          if (ic == null) {
            Log.w("SWK: JS Handler", "insertText failed: InputConnection is null");
            return;
          }

          SystemKeyboard.dismissHelpBubble();

          ic.beginBatchEdit();

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

          for (int i = 0; i < dn; i++) {
            CharSequence chars = ic.getTextBeforeCursor(1, 0);
            if (chars != null && chars.length() > 0) {
              char c = chars.charAt(0);
              SystemKeyboardShouldIgnoreSelectionChange = true;
              if (Character.isLowSurrogate(c)) {
                ic.deleteSurroundingText(2, 0);
              } else {
                ic.deleteSurroundingText(1, 0);
              }
            }
          }

          if (s.length() > 0) {
            SystemKeyboardShouldIgnoreSelectionChange = true;
            ic.commitText(s, s.length());
          }

          ic.endBatchEdit();
        }
      });
    }

    private void keyDownUp(int keyEventCode) {
      IMService.getCurrentInputConnection().sendKeyEvent(new KeyEvent(KeyEvent.ACTION_DOWN, keyEventCode));
      IMService.getCurrentInputConnection().sendKeyEvent(new KeyEvent(KeyEvent.ACTION_UP, keyEventCode));
    }
  }
}