package com.tavultesoft.kmea;

import android.os.Build;
import androidx.appcompat.app.AppCompatActivity;
import android.app.DialogFragment;
import android.app.ProgressDialog;
import android.content.Context;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.widget.Toast;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.tavultesoft.kmea.data.CloudApiTypes;
import com.tavultesoft.kmea.data.CloudDataJsonUtil;
import com.tavultesoft.kmea.data.CloudDownloadMgr;
import com.tavultesoft.kmea.data.CloudKeyboardDataDownloadCallback;
import com.tavultesoft.kmea.data.CloudKeyboardMetaDataDownloadCallback;
import com.tavultesoft.kmea.data.CloudLexicalPackageDownloadCallback;
import com.tavultesoft.kmea.packages.LexicalModelPackageProcessor;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.FileUtils;

import static com.tavultesoft.kmea.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DOWNLOAD_KEYBOARD;
import static com.tavultesoft.kmea.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DOWNLOAD_MODEL;
import static com.tavultesoft.kmea.KMManager.KMDefault_UndefinedPackageID;

public class KMKeyboardDownloaderActivity extends AppCompatActivity {
  // Bundle Keys
  // Cloud
  public static boolean USE_DOWNLOAD_MANAGER = true;

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
   * Used by the <code>download</code> method to handle asynchronous downloading and evaluation of
   * packages and keyboards.
   */
  @Deprecated
  static class DownloadTask extends AsyncTask<Void, Integer, DownloadTask.Result> {
    static class Result {
      public final Integer kbdResult;
      public final List<Map<String, String>> installedLexicalModels;

      public Result(Integer i) {
        this(i, null);
      }

      public Result(Integer i, List<Map<String, String>> installedLexicalModels) {
        this.kbdResult = i;
        this.installedLexicalModels = installedLexicalModels;
      }
    }

    private ProgressDialog progressDialog;
    private String kbVersion = "1.0";
    private String kbIsCustom = isCustom ? "Y" : "N";
    private String font = "";
    private String oskFont = "";

    private Context context;
    private boolean showProgressDialog;
    private boolean downloadOnlyLexicalModel;

    public DownloadTask(Context context, boolean showProgressDialog) {
      this.context = context;
      this.showProgressDialog = showProgressDialog;
      this.downloadOnlyLexicalModel = false;
    }

    public DownloadTask(Context context, boolean showProgressDialog, boolean downloadOnlyLexicalModel) {
      this.context = context;
      this.showProgressDialog = showProgressDialog;
      this.downloadOnlyLexicalModel = downloadOnlyLexicalModel;
    }

    @Override
    protected void onPreExecute() {
      super.onPreExecute();
      if (showProgressDialog) {
        progressDialog = new ProgressDialog(context);
        if (!downloadOnlyLexicalModel) {
          progressDialog.setMessage(context.getString(R.string.downloading_keyboard));
        } else {
          progressDialog.setMessage(context.getString(R.string.downloading_model));
        }
        progressDialog.setCancelable(false);
        if (!((AppCompatActivity) context).isFinishing()) {
          progressDialog.show();
        } else {
          cancel(true);
          progressDialog = null;
        }
      }
    }

    @Override
    protected Result doInBackground(Void... voids) {
      int ret = -1;

      if (isCancelled())
        return new Result(-1);

      try {

        if (downloadOnlyLexicalModel) {
          return downloadKMPLexicalModel();
        }

        String exceptionStr = "Invalid keyboard";
        if (pkgID == null || pkgID.trim().isEmpty() ||
          (!isCustom && (langID == null || langID.trim().isEmpty() || kbID == null || kbID.trim().isEmpty()))) {
          throw new Exception(exceptionStr);
        }

        String deviceType = context.getString(R.string.device_type);
        if (deviceType.equals("AndroidTablet")) {
          deviceType = "androidtablet";
        } else {
          deviceType = "androidphone";
        }

        String remoteUrl = "",remoteLexicalModelUrl = "";
        if (isCustom) {
          //TODO: Doesn't work, because this case will end up in an exception during download???
          // See downloadNonKMPKeyboard
          remoteUrl = url;
        } else {
          // Keyman cloud
          remoteUrl = String.format("%s/%s/%s?version=%s&device=%s&languageidtype=bcp47",
            kKeymanApiBaseURL, langID, kbID, BuildConfig.VERSION_NAME, deviceType);
          remoteLexicalModelUrl = String.format("%s?q=bcp47:%s", kKeymanApiModelURL, langID);

        }

        return downloadNonKMPKeyboard(remoteUrl, remoteLexicalModelUrl);
      } catch (Exception e) {
        ret = -1;
        Log.e(TAG, "Error: " + e, e);
      }

      return new Result(ret);
    }

    @Override
    protected void onProgressUpdate(Integer... progress) {
      // Do nothing
    }

    @Override
    protected void onPostExecute(Result result) {
      try {
        if (progressDialog != null && progressDialog.isShowing()) {
          progressDialog.dismiss();
          progressDialog = null;
        }
      } catch (Exception e) {
        progressDialog = null;
      }

      ((AppCompatActivity) context).finish();

      if (!downloadOnlyLexicalModel) {
        notifyListeners(KeyboardEventHandler.EventType.KEYBOARD_DOWNLOAD_FINISHED, result.kbdResult);
      }

      if(result.installedLexicalModels != null) {
        notifyLexicalModelInstallListeners(KeyboardEventHandler.EventType.LEXICAL_MODEL_INSTALLED,
            result.installedLexicalModels, 1);
      }
    }

    /**
     * Download a KMP Keyman lexical model from Keyman cloud
     * @return A Result object with a success code and any successfully downloaded lexical models.
     * @throws Exception
     */
    protected Result downloadKMPLexicalModel() throws Exception {
      int result = -1;
      File resourceRoot =  new File(context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
      LexicalModelPackageProcessor kmpProcessor = new LexicalModelPackageProcessor(resourceRoot);
      List<Map<String, String>> installedLexicalModels = null;

      if (downloadOnlyLexicalModel) {
        String destination = (context.getCacheDir() + File.separator).toString();
        filename = FileUtils.getFilename(url);

        result = FileUtils.download(context, url, destination, filename);
        if (result > 0 && FileUtils.hasKeymanPackageExtension(url)) {
          // Extract the kmp. Validate it contains only lexical models, and then process the lexical model package
          File kmpFile = new File(context.getCacheDir(), filename);
          String pkgTarget = kmpProcessor.getPackageTarget(kmpFile);
          if (pkgTarget.equals(PackageProcessor.PP_TARGET_LEXICAL_MODELS)) {
            File unzipPath = kmpProcessor.unzipKMP(kmpFile);
            installedLexicalModels = kmpProcessor.processKMP(kmpFile, unzipPath, PackageProcessor.PP_LEXICAL_MODELS_KEY);
          }
        }
      }

      // We'll notify any listeners of the successful download in onPostExecute.
      return new Result(result, installedLexicalModels);
    }

    /**
     * Download a non-KMP Keyman keyboard from Keyman cloud via JSON.
     * If an associated lexical model keyboard package is available, download the first one.
     * @param remoteUrl String
     * @param remoteLexicalModelUrl String API query for lexical model associated with the language ID
     * @return ret int -1 for fail; >0 for success; 2 for keyboard downloading but not font
     * @throws Exception
     */
    protected Result downloadNonKMPKeyboard(String remoteUrl, String remoteLexicalModelUrl) throws Exception {
      int ret = -1;
      JSONParser jsonParser = new JSONParser();
      JSONObject kbData = jsonParser.getJSONObjectFromUrl(remoteUrl);
      String exceptionStr = "Could not reach server";
      if (kbData == null) {
        throw new Exception(exceptionStr);
      }

      if (isCustom) {
        exceptionStr = "Cannot download custom non-KMP keyboard";
        throw new Exception(exceptionStr);
      }

      exceptionStr = "JSON file does not contain a valid \"options\" object";
      JSONObject options = kbData.optJSONObject(KMKey_Options);
      if (options == null) {
        throw new Exception(exceptionStr);
      }
      String kbBaseUri = options.optString(KMKey_KeyboardBaseURI, "");
      if (kbBaseUri.isEmpty()) {
        throw new Exception(exceptionStr);
      }

      String fontBaseUri = options.optString(KMKey_FontBaseURI, "");

      // Keyman cloud keyboard distribution via JSON
      JSONObject language = kbData.optJSONObject(KMKey_Language);
      if (language == null) {
        throw new Exception(exceptionStr);
      }

      langName = language.optString(KMManager.KMKey_Name, "");

      JSONArray keyboards = language.getJSONArray(KMKey_LanguageKeyboards);

      JSONObject keyboard = CloudDataJsonUtil.findMatchingKeyBoardByID(keyboards,kbID);

      kbID = keyboard.getString(KMManager.KMKey_ID);
      pkgID = keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);

      kbName = keyboard.optString(KMManager.KMKey_Name, "");
      kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
      String kbFilename = keyboard.optString(KMKey_Filename, "");
      if (kbName.isEmpty() || langName.isEmpty() || kbFilename.isEmpty())
        throw new Exception(exceptionStr);

      String kbUrl = kbBaseUri + kbFilename;
      ArrayList<String> urls = new ArrayList<String>();
      urls.add(kbUrl);

      JSONObject jsonFont = keyboard.optJSONObject(KMManager.KMKey_Font);
      JSONObject jsonOskFont = keyboard.optJSONObject(KMManager.KMKey_OskFont);

      if (jsonFont != null) {
        CloudDataJsonUtil.updateFontSourceToTTFFont(jsonFont);
      }
      if (jsonOskFont != null) {
        CloudDataJsonUtil.updateFontSourceToTTFFont(jsonOskFont);
      }
      ArrayList<String> fontUrls = CloudDataJsonUtil.fontUrls(jsonFont, fontBaseUri, true);
      ArrayList<String> oskFontUrls = CloudDataJsonUtil.fontUrls(jsonOskFont, fontBaseUri, true);
      if (fontUrls != null)
        urls.addAll(fontUrls);
      if (oskFontUrls != null) {
        for (String url : oskFontUrls) {
          if (!urls.contains(url))
            urls.add(url);
        }
      }

      font = keyboard.optString(KMManager.KMKey_Font);
      oskFont = keyboard.optString(KMManager.KMKey_OskFont);

      // Also download first associated lexical model if it exists
      JSONArray lmData = jsonParser.getJSONObjectFromUrl(remoteLexicalModelUrl, JSONArray.class);
      if (lmData != null && lmData.length() > 0) {
        try {
          JSONObject modelInfo = lmData.getJSONObject(0);
          if (modelInfo.has("packageFilename")) {
            // TODO: Confirm if user wants to overwrite exisiting model version
            urls.add(modelInfo.getString("packageFilename"));
          }
        } catch (JSONException e) {
          Log.e(TAG, "Error parsing lexical model from api.keyman.com. " + e);
        }
      }

      notifyListeners(KeyboardEventHandler.EventType.KEYBOARD_DOWNLOAD_STARTED, 0);

      String destination = context.getDir("data", Context.MODE_PRIVATE).toString() +
        File.separator + KMDefault_UndefinedPackageID + File.separator + File.separator;

      ret = 1;
      int result = 0;
      File resourceRoot =  new File(context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator);
      LexicalModelPackageProcessor kmpProcessor = new LexicalModelPackageProcessor(resourceRoot);
      List<Map<String, String>> installedLexicalModels = new ArrayList<>();
      for (String url : urls) {
        String filename = "";
        if (FileUtils.hasJavaScriptExtension(url)) {

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
        } else if (FileUtils.hasKeymanPackageExtension(url)) {
          // Save the kmp file in the app cache
          destination = (context.getCacheDir() + File.separator).toString();
          filename = FileUtils.getFilename(url);
        }

        result = FileUtils.download(context, url, destination, filename);
        if (result > 0 && FileUtils.hasKeymanPackageExtension(url)) {
          // Extract the kmp. Validate it contains only lexical models, and then process the lexical model package
          File kmpFile = new File(context.getCacheDir(), filename);
          String pkgTarget = kmpProcessor.getPackageTarget(kmpFile);
          if (pkgTarget.equals(PackageProcessor.PP_TARGET_LEXICAL_MODELS)) {
            File unzipPath = kmpProcessor.unzipKMP(kmpFile);
            // Assumption:  only one lexical model KMP will occur per download.
            installedLexicalModels = kmpProcessor.processKMP(kmpFile, unzipPath, PackageProcessor.PP_LEXICAL_MODELS_KEY);
          }
        }

        if (result < 0) {
          if (FileUtils.hasFontExtension(url)) {
            // Propagate warning about font failing to download
            ret = 2;
          } else {
            ret = -1;
            break;
          }
        }
      }

      if (installedLexicalModels.size() != 0) {
        // Let the postExecute method signal the listeners - it's triggered on the main thread.
        return new Result(ret, installedLexicalModels);
      }

      return new Result(ret);
    }

    /**
     * Notify listeners when an event happens
     * @param eventType
     * @param result
     */
    protected void notifyListeners(KeyboardEventHandler.EventType eventType, int result) {
      if (kbDownloadEventListeners != null) {
        HashMap<String, String> keyboardInfo =
          CloudDataJsonUtil.createKeyBoardInfoMap(pkgID,langID,langName,kbID,kbName,kbVersion,kbIsCustom,font,oskFont);

        KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, eventType, keyboardInfo, result);
      }
    }

    /**
     * Notify listeners when a lexical model is installed
     * @param eventType
     * @param models
     * @param result
     */
    protected void notifyLexicalModelInstallListeners(KeyboardEventHandler.EventType eventType,
      List<Map<String, String>> models, int result) {
      if (kbDownloadEventListeners != null) {
        KeyboardEventHandler.notifyListeners(kbDownloadEventListeners, eventType, models, result);
      }
    }
  }

  /**
   * Async task to download a Keyman keyboard from either Keyman cloud server or custom url
   * @param context
   * @param showProgressDialog
   */
  public static void download(final Context context, final boolean showProgressDialog) {
    if(USE_DOWNLOAD_MANAGER)
      downloadKeyboardUsingDownloadManager(context);
    else
      new DownloadTask(context, showProgressDialog).execute();
  }

  public static void download(final Context context, final boolean showProgressDialog,
                              final boolean aDownloadOnlyLexicalModel) {
    if(USE_DOWNLOAD_MANAGER)
      downloadUsingDownloadManager(context,aDownloadOnlyLexicalModel);
    else
      new DownloadTask(context, showProgressDialog, aDownloadOnlyLexicalModel).execute();
  }

  /**
   * Download in keyboards and lexical model using download manager.
   * @param context the context
   * @param aDownloadOnlyLexicalModel is lexical model download
   */
  private static void downloadUsingDownloadManager(final Context context,
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
        CloudApiTypes.ApiTarget.KeyBoardLexicalModels, _remoteLexicalModelUrl)
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
