package com.keyman.engine;

import android.app.DialogFragment;
import android.content.Context;
import android.os.Bundle;
import android.widget.Toast;

import androidx.appcompat.app.AppCompatActivity;

import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.CloudDataJsonUtil;
import com.keyman.engine.cloud.CloudDownloadMgr;
import com.keyman.engine.cloud.DownloadManagerDisabledException;
import com.keyman.engine.cloud.impl.CloudKeyboardPackageDownloadCallback;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.cloud.impl.CloudLexicalPackageDownloadCallback;
import com.keyman.engine.util.KMLog;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import static com.keyman.engine.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DOWNLOAD_KEYBOARD;
import static com.keyman.engine.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DOWNLOAD_MODEL;

public class KMKeyboardDownloaderActivity extends BaseActivity {
  // Bundle Keys

  // Cloud
  public static final String ARG_PKG_ID = "KMKeyboardActivity.pkgID";
  public static final String ARG_KB_ID = "KMKeyboardActivity.kbID";
  public static final String ARG_LANG_ID = "KMKeyboardActivity.langID";
  public static final String ARG_KB_NAME = "KMKeyboardActivity.kbName";
  public static final String ARG_LANG_NAME = "KMKeyboardActivity.langName";
  public static final String ARG_MODEL_ID = "KMKeyboardActivity.modelID";
  public static final String ARG_MODEL_NAME = "KMKeyboardActivity.modelName";
  public static final String ARG_CUSTOM_HELP_LINK = "KMKeyboardActivity.customHelpLink";
  public static final String ARG_KMP_LINK = "KMKeyboardActivity.kmpLink";

  private static final String TAG = "KMKbdDownloaderActivity"; // TAG needs to be less than 28 chars

  // Keyman public keys
  public static final String KMKey_BCP47 = "bcp47";
  public static final String KMKey_Filename = "filename";
  public static final String KMKey_Keyboard = "keyboard";
  public static final String KMKey_LanguageKeyboards = "keyboards";
  public static final String KMKey_Language = "language";
  public static final String KMKey_Languages = "languages";
  public static final String KMKey_Options = "options";
  public static final String KMKey_Platform = "platform";
  public static final String KMKey_Tier = "tier";
  public static final String KMKey_URL = "url";

  // Keyman internal keys
  public static final String KMKey_KeyboardBaseURI = "keyboardBaseUri";
  public static final String KMKey_FontBaseURI = "fontBaseUri";

  //TODO: use keyboard model class
  private String pkgID;
  private String kbID;
  private String langID;
  private String modelID;
  private String modelName;
  private String kbName;
  private String langName;
  private Boolean downloadOnlyLexicalModel;

  private String customKeyboard;
  private String customLanguage;
  private Boolean isDirect;
  private String url;
  private String filename;

  //TODO: move to keyboard manager class
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

    modelID = bundle.getString(ARG_MODEL_ID);
    modelName = bundle.getString(ARG_MODEL_NAME);
    kbID = bundle.getString(ARG_KB_ID);
    kbName = bundle.getString(ARG_KB_NAME);
    url = bundle.getString(ARG_KMP_LINK);

    downloadOnlyLexicalModel = (modelID != null && !modelID.isEmpty());

    // Download keyboard from cloud server
    String title = String.format("%s: %s", langName, kbName);

    DialogFragment dialog;
    if (downloadOnlyLexicalModel) {
      title = String.format("%s: %s", langName, modelName);
      dialog = ConfirmDialogFragment.newInstanceForLexicalModel(
        DIALOG_TYPE_DOWNLOAD_MODEL, title, getString(R.string.confirm_download_model),
        langID, modelID,
        prepareCloudApiParamsForLexicalModelDownload());
    } else {
      dialog = ConfirmDialogFragment.newInstanceForKeyboard(
        DIALOG_TYPE_DOWNLOAD_KEYBOARD, title, getString(R.string.confirm_download_keyboard),
        langID, kbID,
        prepareCloudApiParamsForKeyboardDownload());
    }

    dialog.show(getFragmentManager(), "dialog");
  }

  private ArrayList<CloudApiTypes.CloudApiParam> prepareCloudApiParamsForKeyboardPackageDownload() {
    ArrayList<CloudApiTypes.CloudApiParam> _params = new ArrayList<>();
    _params.add(new CloudApiTypes.CloudApiParam(
      CloudApiTypes.ApiTarget.KeyboardPackage, url));
    return _params;
  }

  /**
   * prepare cloud api params for lexical models download.
   * @return the result
   */
  private ArrayList<CloudApiTypes.CloudApiParam> prepareCloudApiParamsForLexicalModelDownload() {
    ArrayList<CloudApiTypes.CloudApiParam> _params = new ArrayList<>();
    _params.add(new CloudApiTypes.CloudApiParam(
      CloudApiTypes.ApiTarget.LexicalModelPackage, url));
    return _params;
  }

  /**
   * prepare and execute keyboard download using downloadmanager.
   * @param context the context
   * @param aLangId the language id
   * @param aKbId the keyman keyboard id
   * @param aPreparedCloudApiParams the prepared api params
   */
  public static void downloadKeyboard(Context context,
                                       String aLangId, String aKbId,
                                       List<CloudApiTypes.CloudApiParam> aPreparedCloudApiParams)
  {
    String _downloadid= CloudKeyboardPackageDownloadCallback.createDownloadId(aLangId , aKbId);

    if(  CloudDownloadMgr.getInstance().alreadyDownloadingData(_downloadid)
       ||  CloudDownloadMgr.getInstance().alreadyDownloadingData(
      CloudKeyboardPackageDownloadCallback.createDownloadId(aLangId, aKbId)))
    {
      Toast.makeText(context,
        context.getString(R.string.keyboard_download_is_running_in_background),
        Toast.LENGTH_SHORT).show();
    }
    else
    {
      CloudKeyboardPackageDownloadCallback _callback = new CloudKeyboardPackageDownloadCallback();
      _callback.setLanguageID(aLangId);

      Toast.makeText(context,
        context.getString(R.string.keyboard_download_start_in_background),
        Toast.LENGTH_SHORT).show();

      Toast errorToast = Toast.makeText(context,
        context.getString(R.string.update_check_unavailable),
        Toast.LENGTH_SHORT);

      try {
        CloudDownloadMgr.getInstance().executeAsDownload(
          context, _downloadid, null, _callback,
          aPreparedCloudApiParams.toArray(new CloudApiTypes.CloudApiParam[0]));
      } catch (DownloadManagerDisabledException e) {
        errorToast.show();
      } catch (Exception e) {
        errorToast.show();
        KMLog.LogException(TAG, "Unexpected exception occurred during download attempt", e);
      }
    }

    ((AppCompatActivity) context).finish();
  }

  /**
   * Prepare the cloud api params for keyboard metadata download.
   * @return the result
   */
  private ArrayList<CloudApiTypes.CloudApiParam> prepareCloudApiParamsForKeyboardDownload()
  {
    if (pkgID == null || pkgID.trim().isEmpty() ||
        langID == null || langID.trim().isEmpty() ||
        kbID == null || kbID.trim().isEmpty()) {
      throw new IllegalStateException("Invalid keyboard");
    }

    ArrayList<CloudApiTypes.CloudApiParam> cloudQueries = new ArrayList<>();
    cloudQueries.add(
      new CloudApiTypes.CloudApiParam(
        CloudApiTypes.ApiTarget.KeyboardPackage, url));

    return cloudQueries;
  }

  /**
   * prepare and execute lexical model download using downloadmanager.
   * @param context the context
   * @param aModelId the lexical model id
   * @param aPreparedCloudApiParams the prepared api params
   */
  public static void downloadLexicalModel(Context context,
                                          String aModelId,
                                          List<CloudApiTypes.CloudApiParam> aPreparedCloudApiParams) {


    String _downloadid= CloudLexicalPackageDownloadCallback.createDownloadId(aModelId);

    if(  CloudDownloadMgr.getInstance().alreadyDownloadingData(_downloadid))
    {
      Toast.makeText(context,
        context.getString(R.string.dictionary_download_is_running_in_background),
        Toast.LENGTH_SHORT).show();
    }
    else
    {
      CloudLexicalPackageDownloadCallback _callback = new CloudLexicalPackageDownloadCallback();

      Toast.makeText(context,
        context.getString(R.string.dictionary_download_start_in_background),
        Toast.LENGTH_SHORT).show();

      Toast errorToast = Toast.makeText(context,
        context.getString(R.string.update_check_unavailable),
        Toast.LENGTH_SHORT);

      try {
        CloudDownloadMgr.getInstance().executeAsDownload(
          context, _downloadid, null, _callback,
          aPreparedCloudApiParams.toArray(new CloudApiTypes.CloudApiParam[0]));
      } catch (DownloadManagerDisabledException e) {
        errorToast.show();
      } catch (Exception e) {
        errorToast.show();
        KMLog.LogException(TAG, "Unexpected exception occurred during download attempt", e);
      }
    }

    ((AppCompatActivity) context).finish();
  }

  public static void addKeyboardDownloadEventListener(KeyboardEventHandler.OnKeyboardDownloadEventListener listener) {
    if (kbDownloadEventListeners == null) {
      kbDownloadEventListeners = new ArrayList<>();
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

  public static ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> getKbDownloadEventListeners() {
    return kbDownloadEventListeners;
  }
}
