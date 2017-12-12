package com.tavultesoft.kmea;

import android.app.Activity;
import android.app.DialogFragment;
import android.app.ProgressDialog;
import android.content.Context;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;

import com.tavultesoft.kmea.util.FileDownloader;

import static com.tavultesoft.kmea.KMManager.KMDefault_AssetPackages;

public class KMKeyboardDownloaderActivity extends Activity {
  // Keys for cloud keyboard
  public static final String ARG_PKG_ID = "KMKeyboardActivity.pkgID";
  public static final String ARG_KB_ID = "KMKeyboardActivity.kbID";
  public static final String ARG_LANG_ID = "KMKeyboardActivity.langID";
  public static final String ARG_KB_NAME = "KMKeyboardActivity.kbName";
  public static final String ARG_LANG_NAME = "KMKeyboardActivity.langName";
  public static final String ARG_IS_CUSTOM = "KMKeyboardActivity.isCustom";

  // Keys for custom keyboard
  public static final String ARG_KEYBOARD = "KMKeyboardActivity.keyboard";
  public static final String ARG_LANGUAGE = "KMKeyboardActivity.language";
  public static final String ARG_IS_DIRECT = "KMKeyboardActivity.isDirect";
  public static final String ARG_URL = "KMKeyboardActivity.url";
  public static final String ARG_JSON_URL = "KMKeyboardActivity.jsonUrl";

  public static final String kKeymanApiBaseURL = "https://r.keymanweb.com/api/3.0/";
  public static final String kKeymanApiRemoteURL = "https://r.keymanweb.com/api/2.0/remote?url=";
  public static final String KMKey_KeyboardBaseURI = "keyboardBaseUri";
  public static final String KMKey_FontBaseURI = "fontBaseUri";
  public static final String KMKey_Direct = "direct";

  private static String pkgID;
  private static String kbID;
  private static String langID;
  private static String kbName;
  private static String langName;
  private static Boolean isCustom;

  private static String customKeyboard;
  private static String customLanguage;
  private static Boolean isDirect;
  private static String url;
  private static String jsonUrl;

  private static ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> kbDownloadEventListeners = null;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    Bundle bundle = getIntent().getExtras();
    if (bundle != null) {
      pkgID = bundle.getString(ARG_PKG_ID);
      kbID = bundle.getString(ARG_KB_ID);
      langID = bundle.getString(ARG_LANG_ID);
      kbName = bundle.getString(ARG_KB_NAME);
      langName = bundle.getString(ARG_LANG_NAME);
      isCustom = bundle.getBoolean(ARG_IS_CUSTOM);

      // URL parameters for custom keyboard (if they exist)
      customKeyboard = bundle.getString(ARG_KEYBOARD);
      customLanguage = bundle.getString(ARG_LANGUAGE);
      isDirect = bundle.getBoolean(ARG_IS_DIRECT);
      url = bundle.getString(ARG_URL);
      jsonUrl = bundle.getString(ARG_JSON_URL);
    } else {
      return;
    }

    Bundle args = new Bundle();
    String title = "";
    if (url != null) {
      int index = url.lastIndexOf("/") + 1;
      String jsonFilename = "unknown";
      if (index >= 0 && index <= url.length()) {
        jsonFilename = url.substring(index);
      }

      title = "Custom Keyboard: " + jsonFilename;
    } else if (customKeyboard != null && customLanguage != null &&
        !customKeyboard.trim().isEmpty() && !customLanguage.trim().isEmpty()) {
      int kbIndex = KMManager.getKeyboardIndex(getApplicationContext(), customKeyboard, customLanguage);

      if (kbIndex >= 0) {
        KMManager.setKeyboard(getApplicationContext(), kbIndex);
        // No interaction needed
        return; // false
      }

      title = customLanguage + "_" + customKeyboard;
  } else {
    // Download keyboard from cloud server
    title = langName + ": " + kbName;
  }

  DialogFragment dialog = new ConfirmDialogFragment();
    args.putString(ConfirmDialogFragment.ARG_TITLE, title);
    dialog.setArguments(args);
    dialog.show(

  getFragmentManager(), "dialog");
}

  /**
   * Async task to download a Keyman keyboard from either Keyman cloud server or custom url
   * @param context
   * @param showProgressDialog
   */
    public static void download(final Context context, final boolean showProgressDialog) {

    new AsyncTask<Void, Integer, Integer>() {
      private ProgressDialog progressDialog;
      private String languageName = "";
      private String keyboardName = "";
      private String kbVersion = "1.0";
      private String kbIsCustom = isCustom ? "Y" : "N";
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
          if (pkgID == null || pkgID.trim().isEmpty() ||
            langID == null || langID.trim().isEmpty() ||
            kbID == null || kbID.trim().isEmpty())
            throw new Exception(exceptionStr);

          String deviceType = context.getResources().getString(R.string.device_type);
          if (deviceType.equals("AndroidTablet")) {
            deviceType = "androidtablet";
          } else {
            deviceType = "androidphone";
          }

          // Formerly from KMManager.KMCustomKeyboardDownloader
          JSONParser jsonParser = new JSONParser();
          JSONObject kbData = null;
          String remoteUrl = "";
          if (isCustom) {
            if (isDirect) {
              remoteUrl = jsonUrl;
            } else {
              String encodedUrl = URLEncoder.encode(jsonUrl, "utf-8");
              remoteUrl = String.format("%s%s&device=%s", kKeymanApiRemoteURL, encodedUrl, deviceType);
            }
          } else {
            remoteUrl = String.format("%slanguages/%s/%s?device=%s", kKeymanApiBaseURL, langID, kbID, deviceType);
          }
          kbData = jsonParser.getJSONObjectFromUrl(remoteUrl);

          exceptionStr = "Could not reach Keyman server";
          if (kbData == null) {
            throw new Exception(exceptionStr);
          }

          JSONObject options = kbData.optJSONObject(KMManager.KMKey_Options);
          JSONObject language = kbData.optJSONObject(KMManager.KMKey_Language);
          exceptionStr = "The keyboard could not be installed";
          if (options == null || language == null) {
            throw new Exception(exceptionStr);
          }

          JSONArray keyboards = language.getJSONArray(KMManager.KMKey_LanguageKeyboards);
          String kbBaseUri = options.optString(KMKey_KeyboardBaseURI, "");
          String fontBaseUri = options.optString(KMKey_FontBaseURI, "");

          if (keyboards == null || kbBaseUri.isEmpty())
            throw new Exception(exceptionStr);

          JSONObject keyboard = keyboards.getJSONObject(0);
          if (keyboard == null)
            throw new Exception(exceptionStr);

          languageName = language.optString(KMManager.KMKey_Name, "");
          keyboardName = keyboard.optString(KMManager.KMKey_Name, "");
          kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
          font = keyboard.optString(KMManager.KMKey_Font, "");
          oskFont = keyboard.optString(KMManager.KMKey_OskFont, null);
          String kbFilename = keyboard.optString(KMManager.KMKey_Filename, "");

          if (keyboardName.isEmpty() || languageName.isEmpty() || kbFilename.isEmpty())
            throw new Exception(exceptionStr);

          String kbUrl = kbBaseUri + kbFilename;
          urls.add(kbUrl);
          JSONObject jsonFont = keyboard.optJSONObject(KMManager.KMKey_Font);
          JSONObject jsonOskFont = keyboard.optJSONObject(KMManager.KMKey_OskFont);
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

          notifyListeners(KeyboardEventHandler.EventType.KEYBOARD_DOWNLOAD_STARTED, 0);

          ret = 1;
          int result = 0;
          for (String url : urls) {
            String directory = KMDefault_AssetPackages + File.separator + pkgID;
            File dir = new File(directory);
            String filename = "";
            if (url.endsWith(".js")) {

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

        notifyListeners(KeyboardEventHandler.EventType.KEYBOARD_DOWNLOAD_FINISHED, result);
        super.onPostExecute(result);
      }

      /**
       * Notify listeners when an event happens
       * @param eventType
       * @param result
       */
      protected void notifyListeners(KeyboardEventHandler.EventType eventType, int result) {
        if (kbDownloadEventListeners != null) {
          HashMap<String, String> keyboardInfo = new HashMap<String, String>();
          keyboardInfo.put(KMManager.KMKey_PackageID, pkgID);
          keyboardInfo.put(KMManager.KMKey_KeyboardID, kbID);
          keyboardInfo.put(KMManager.KMKey_LanguageID, langID);
          keyboardInfo.put(KMManager.KMKey_KeyboardName, keyboardName);
          keyboardInfo.put(KMManager.KMKey_LanguageName, languageName);
          keyboardInfo.put(KMManager.KMKey_KeyboardVersion, kbVersion);
          keyboardInfo.put(KMManager.KMKey_CustomKeyboard, kbIsCustom);
          keyboardInfo.put(KMManager.KMKey_Font, font);
          if (oskFont != null)
            keyboardInfo.put(KMManager.KMKey_OskFont, oskFont);
          KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, eventType, keyboardInfo, result);
        }
      }
    }.execute();
  }

  public static boolean isCustom(String u) {
    boolean ret = false;
    if (u != null && !u.contains(KMKeyboardDownloaderActivity.kKeymanApiBaseURL) &&
      !u.contains(KMKeyboardDownloaderActivity.kKeymanApiRemoteURL)) {
      isCustom = true;
    }
    return ret;
  }

  private static ArrayList<String> fontUrls(JSONObject jsonFont, String baseUri, boolean isOskFont) {
    if (jsonFont == null)
      return null;

    ArrayList<String> urls = new ArrayList<String>();
    JSONArray fontSource = jsonFont.optJSONArray(KMManager.KMKey_FontSource);
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
        fontSourceString = jsonFont.getString(KMManager.KMKey_FontSource);
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

  public static void addKeyboardDownloadEventListener(KeyboardEventHandler.OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners == null) {
      kbDownloadEventListeners = new ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener>();
    }

    if (listener != null && !kbDownloadEventListeners.contains(listener)) {
      kbDownloadEventListeners.add(listener);
    }
  }

  public static void removeKeyboardDownloadEventListener(KeyboardEventHandler.OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners != null) {
      kbDownloadEventListeners.remove(listener);
    }
  }
}
