package com.tavultesoft.kmea;

import android.app.DialogFragment;
import android.content.Context;
import android.os.Bundle;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.tavultesoft.kmea.data.CloudApiTypes;
import com.tavultesoft.kmea.data.CloudDataJsonUtil;
import com.tavultesoft.kmea.data.CloudDownloadMgr;
import com.tavultesoft.kmea.data.CloudKeyboardDataDownloadCallback;
import com.tavultesoft.kmea.data.CloudKeyboardMetaDataDownloadCallback;
import com.tavultesoft.kmea.data.CloudLexicalPackageDownloadCallback;

import java.util.ArrayList;
import java.util.List;

import static com.tavultesoft.kmea.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DOWNLOAD_KEYBOARD;
import static com.tavultesoft.kmea.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DOWNLOAD_MODEL;

public class KMKeyboardDownloaderActivity extends AppCompatActivity {
  // Bundle Keys

  // Cloud
  public static final String ARG_PKG_ID = "KMKeyboardActivity.pkgID";
  public static final String ARG_KB_ID = "KMKeyboardActivity.kbID";
  public static final String ARG_LANG_ID = "KMKeyboardActivity.langID";
  public static final String ARG_KB_NAME = "KMKeyboardActivity.kbName";
  public static final String ARG_LANG_NAME = "KMKeyboardActivity.langName";
  public static final String ARG_IS_CUSTOM = "KMKeyboardActivity.isCustom";
  public static final String ARG_MODEL_ID = "KMKeyboardActivity.modelID";
  public static final String ARG_MODEL_NAME = "KMKeyboardActivity.modelName";
  public static final String ARG_MODEL_URL = "KMKeyboardActivity.modelURL";
  public static final String ARG_MODEL_CUSTOM_HELP_LINK = "KMKeyboardActivity.customHelpLink";

  // custom keyboard
  public static final String ARG_KEYBOARD = "KMKeyboardActivity.keyboard";
  public static final String ARG_LANGUAGE = "KMKeyboardActivity.language";
  public static final String ARG_URL = "KMKeyboardActivity.url";
  public static final String ARG_FILENAME = "KMKeyboardActivity.filename";

  public static final String kKeymanApiBaseURL = "https://api.keyman.com/cloud/4.0/languages";
  public static final String kKeymanApiModelURL = "https://api.keyman.com/model";
  public static final String kKeymanApiRemoteURL = "https://r.keymanweb.com/api/2.0/remote?url=";

  private static final String TAG = "KMKbdDownloaderActivity"; // TAG needs to be less than 28 chars

  // Keyman public keys
  public static final String KMKey_URL = "url";
  public static final String KMKey_Keyboard = "keyboard";
  public static final String KMKey_LanguageKeyboards = "keyboards";
  public static final String KMKey_Options = "options";
  public static final String KMKey_Language = "language";
  public static final String KMKey_Languages = "languages";
  public static final String KMKey_Filename = "filename";

  // Keyman internal keys
  public static final String KMKey_KeyboardBaseURI = "keyboardBaseUri";
  public static final String KMKey_FontBaseURI = "fontBaseUri";

  //TODO: use keyboard model class, should not be static
  private static String pkgID;
  private static String kbID;
  private static String langID;
  private static String modelID;
  private static String modelName;
  private static String kbName;
  private static String langName;
  private static Boolean isCustom;
  private static Boolean downloadOnlyLexicalModel;

  private static String customKeyboard;
  private static String customLanguage;
  private static Boolean isDirect;
  private static String url;
  private static String filename;

  private static ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> kbDownloadEventListeners = null;
  
  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);

    Bundle bundle = getIntent().getExtras();
    if (bundle == null)
      return;

    pkgID = bundle.getString(ARG_PKG_ID);
    if (pkgID == null || pkgID.isEmpty()) {
      pkgID = KMManager.KMDefault_UndefinedPackageID;
    }
    langID = bundle.getString(ARG_LANG_ID);
    langName = bundle.getString(ARG_LANG_NAME);

    downloadOnlyLexicalModel = bundle.containsKey(ARG_MODEL_URL) &&
      bundle.getString(ARG_MODEL_URL) != null &&
      !bundle.getString(ARG_MODEL_URL).isEmpty();

    if (downloadOnlyLexicalModel) {
      modelID = bundle.getString(ARG_MODEL_ID);
      modelName = bundle.getString(ARG_MODEL_NAME);
      isCustom = false;
      url = bundle.getString(ARG_MODEL_URL);
    } else {

      kbID = bundle.getString(ARG_KB_ID);
      kbName = bundle.getString(ARG_KB_NAME);
      isCustom = bundle.getBoolean(ARG_IS_CUSTOM);

      // URL parameters for custom keyboard (if they exist)
      customKeyboard = bundle.getString(ARG_KEYBOARD);
      customLanguage = bundle.getString(ARG_LANGUAGE);
      url = bundle.getString(ARG_URL);
      filename = bundle.getString(ARG_FILENAME);
      if (filename == null || filename.isEmpty()) {
        filename = "unknown";
      }
    }

    String title = "";
    if (url != null) {
      title = String.format("%s: %s", getString(R.string.custom_keyboard), filename);
    } else if (customKeyboard != null && customLanguage != null &&
        !customKeyboard.trim().isEmpty() && !customLanguage.trim().isEmpty()) {
      int kbIndex = KMManager.getKeyboardIndex(getApplicationContext(), customKeyboard, customLanguage);

      if (kbIndex >= 0) {
        KMManager.setKeyboard(getApplicationContext(), kbIndex);
        // No interaction needed
        return; // false
      }

      title = String.format("%s_%s", customLanguage, customKeyboard);
    } else {
      // Download keyboard from cloud server
      title = String.format("%s: %s", langName, kbName);
    }

    DialogFragment dialog;
    if (downloadOnlyLexicalModel) {
      title = String.format("%s: %s", langName, modelName);
      dialog = ConfirmDialogFragment.newInstance(
        DIALOG_TYPE_DOWNLOAD_MODEL, title, getString(R.string.confirm_download_model));
    } else {
      dialog = ConfirmDialogFragment.newInstance(
        DIALOG_TYPE_DOWNLOAD_KEYBOARD, title, getString(R.string.confirm_download_keyboard));
    }

    dialog.show(getFragmentManager(), "dialog");
  }

  /**
   * Download in keyboards and lexical model using download manager.
   * @param context the context
   * @param aDownloadOnlyLexicalModel is lexical model download
   */
  public static void downloadUsingDownloadManager(final Context context,
                              final boolean aDownloadOnlyLexicalModel)
  {
    if(aDownloadOnlyLexicalModel)
    {
      downloadLexicalModelUsingDownloadManager(context);
    }
    else
    {
      downloadKeyboardUsingDownloadManager(context);
    }
  }

  /**
   * prepare and execute keyboard download using downloadmanager.
   * @param context the context
   */
  private static void downloadKeyboardUsingDownloadManager(Context context)
  {
    if (pkgID == null || pkgID.trim().isEmpty() ||
      (!isCustom && (langID == null || langID.trim().isEmpty() || kbID == null || kbID.trim().isEmpty()))) {
      throw new IllegalStateException("Invalid keyboard");
    }

    List<CloudApiTypes.CloudApiParam> cloudQueries = getPrepareCloudQueriesForKeyboardDownload(context);

    String _downloadid= CloudKeyboardMetaDataDownloadCallback.createDownloadId(langID , kbID);

    if(  CloudDownloadMgr.getInstance().alreadyDownloadingData(_downloadid)
       ||  CloudDownloadMgr.getInstance().alreadyDownloadingData(
              CloudKeyboardDataDownloadCallback.createDownloadId(kbID)))
    {
      Toast.makeText(context,
        context.getString(R.string.keyboard_download_is_running_in_background),
        Toast.LENGTH_SHORT).show();
    }
    else
    {
      CloudKeyboardMetaDataDownloadCallback _callback = new CloudKeyboardMetaDataDownloadCallback();
      _callback.setDownloadEventListeners(kbDownloadEventListeners);

      Toast.makeText(context,
        context.getString(R.string.keyboard_download_start_in_background),
        Toast.LENGTH_SHORT).show();

      CloudDownloadMgr.getInstance().executeAsDownload(
        context, _downloadid, null, _callback,
        cloudQueries.toArray(new CloudApiTypes.CloudApiParam[0]));
    }

    ((AppCompatActivity) context).finish();
  }

  /**
   * Prepare the cloud queries for keyboard metadata download.
   * @param context the context
   * @return the result
   */
  private static List<CloudApiTypes.CloudApiParam> getPrepareCloudQueriesForKeyboardDownload(Context context)
  {
    List<CloudApiTypes.CloudApiParam> cloudQueries = new ArrayList<>();

    String deviceType = CloudDataJsonUtil.getDeviceTypeForCloudQuery(context);

    if (isCustom) {
      //TODO: will end up in an exception during download???
      cloudQueries.add(new CloudApiTypes.CloudApiParam(
        CloudApiTypes.ApiTarget.KeyboardData, url));
    }
    else
    {
      // Keyman cloud
      String _remoteUrl = String.format("%s/%s/%s?version=%s&device=%s&languageidtype=bcp47",
        kKeymanApiBaseURL, langID, kbID, BuildConfig.VERSION_NAME, deviceType);
      cloudQueries.add(
        new CloudApiTypes.CloudApiParam(
          CloudApiTypes.ApiTarget.Keyboard, _remoteUrl)
          .setType(CloudApiTypes.JSONType.Object)
          .setAdditionalProperty(CloudKeyboardMetaDataDownloadCallback.PARAM_IS_CUSTOM,isCustom)
          .setAdditionalProperty(CloudKeyboardMetaDataDownloadCallback.PARAM_LANG_ID,langID)
           .setAdditionalProperty(CloudKeyboardMetaDataDownloadCallback.PARAM_KB_ID,kbID));

      String _remoteLexicalModelUrl = String.format("%s?q=bcp47:%s", kKeymanApiModelURL, langID);
      cloudQueries.add(new CloudApiTypes.CloudApiParam(
        CloudApiTypes.ApiTarget.KeyboardLexicalModels, _remoteLexicalModelUrl)
        .setType(CloudApiTypes.JSONType.Array));
    }
    return cloudQueries;
  }

  /**
   * prepare and execute lexical model download using downloadmanager.
   * @param context the context
   */
  private static void downloadLexicalModelUsingDownloadManager(Context context) {


    String _downloadid= CloudLexicalPackageDownloadCallback.createDownloadId(modelID);

    if(  CloudDownloadMgr.getInstance().alreadyDownloadingData(_downloadid))
    {
      Toast.makeText(context,
        context.getString(R.string.dictionary_download_is_running_in_background),
        Toast.LENGTH_SHORT).show();
    }
    else
    {
      CloudLexicalPackageDownloadCallback _callback = new CloudLexicalPackageDownloadCallback();
      _callback.setDownloadEventListeners(kbDownloadEventListeners);

      CloudApiTypes.CloudApiParam _param = new CloudApiTypes.CloudApiParam(
        CloudApiTypes.ApiTarget.LexicalModelPackage, url);

      Toast.makeText(context,
        context.getString(R.string.dictionary_download_start_in_background),
        Toast.LENGTH_SHORT).show();

      CloudDownloadMgr.getInstance().executeAsDownload(
        context, _downloadid, null, _callback, _param);
    }

    ((AppCompatActivity) context).finish();
  }


  public static boolean isCustom(String u) {
    boolean ret = false;
    if (u != null && !u.contains(KMKeyboardDownloaderActivity.kKeymanApiBaseURL) &&
      !u.contains(KMKeyboardDownloaderActivity.kKeymanApiRemoteURL)) {
      ret = true;
    }
    return ret;
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
