package com.tavultesoft.kmea.cloud.impl;

import android.content.Context;
import android.os.Bundle;
import android.widget.Toast;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.cloud.CloudApiTypes;
import com.tavultesoft.kmea.cloud.CloudDataJsonUtil;
import com.tavultesoft.kmea.cloud.CloudDownloadMgr;
import com.tavultesoft.kmea.cloud.ICloudDownloadCallback;
import com.tavultesoft.kmea.data.KeyboardController;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.KMLog;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * Start the keyboard download when keyboard metadata is downloaded.
 */
public class CloudKeyboardMetaDataDownloadCallback implements ICloudDownloadCallback<Void,
  List<CloudKeyboardMetaDataDownloadCallback.MetaDataResult>>
{
  /**
   * the metadata result and all necessary downloads which should be started.
   */
  static class MetaDataResult
  {
    CloudApiTypes.CloudApiReturns returnjson;
    CloudApiTypes.CloudApiParam params;
    HashMap<String,String> keyboardInfo;
    String additionalDownloadid;
    List<CloudApiTypes.CloudApiParam> additionalDownloads;
  }

  private static final String TAG = "CloudKeyboardMetaDldCb";

  /**
   * Additional Cloud API parameter: language id.
   */
  public static final String PARAM_LANG_ID = "lang_id";
  /**
   * Additional Cloud API parameter: keyboard id.
   */
  public static final String PARAM_KB_ID = "kb_id";



  @Override
  public void initializeContext(Context context) {

  }

  @Override
  public List<CloudKeyboardMetaDataDownloadCallback.MetaDataResult> extractCloudResultFromDownloadSet(
    CloudApiTypes.CloudDownloadSet<Void, List<CloudKeyboardMetaDataDownloadCallback.MetaDataResult>> aDownload) {

    List<CloudKeyboardMetaDataDownloadCallback.MetaDataResult> _result = new ArrayList<>(aDownload.getSingleDownloads().size());

    for (CloudApiTypes.SingleCloudDownload _d : aDownload.getSingleDownloads()) {

      CloudApiTypes.CloudApiReturns _json_result = CloudDataJsonUtil.retrieveJsonFromDownload(_d);

      if (_json_result!=null) {

        CloudKeyboardMetaDataDownloadCallback.MetaDataResult _data = new CloudKeyboardMetaDataDownloadCallback.MetaDataResult();
        _data.returnjson = _json_result;
        _data.params = _d.getCloudParams();
        _result.add(_data);  // Null if offline.
      }
    }
    return _result;
  }



  @Override
  public void applyCloudDownloadToModel(Context aContext, Void aModel, List<CloudKeyboardMetaDataDownloadCallback.MetaDataResult> aCloudResult)
  {
    if(aCloudResult.isEmpty()) {
      String msg = aContext.getString(R.string.catalog_unavailable);
      Toast.makeText(aContext, msg, Toast.LENGTH_SHORT).show();
      //throw new IllegalStateException("Could not reach server");
      return;
    }

    processCloudResults(aContext, aCloudResult);

    startDownloads(aContext, aCloudResult);
  }

  /**
   * Start the keyboard data and lexical model download.
   * @param aContext the context
   * @param aMetaDataResult the meta data result
   */
  private void startDownloads(Context aContext, List<MetaDataResult> aMetaDataResult) {
    for(MetaDataResult _r:aMetaDataResult)
    {
      if(_r.additionalDownloads!=null)
      {
        if(_r.returnjson.target== CloudApiTypes.ApiTarget.Keyboard)
        {
          CloudKeyboardDataDownloadCallback _callback = new CloudKeyboardDataDownloadCallback();
          _callback.setKeyboardInfo(_r.keyboardInfo);

          if(  CloudDownloadMgr.getInstance().alreadyDownloadingData(_r.additionalDownloadid))
          {
            continue;
          }

          KeyboardEventHandler.notifyListeners(
            KMKeyboardDownloaderActivity.getKbDownloadEventListeners(),
            KeyboardEventHandler.EventType.KEYBOARD_DOWNLOAD_STARTED, _r.keyboardInfo, 0);

          CloudDownloadMgr.getInstance().executeAsDownload(aContext, _r.additionalDownloadid, null, _callback,
            _r.additionalDownloads.toArray(new CloudApiTypes.CloudApiParam[0]));
        }
        else if(_r.returnjson.target== CloudApiTypes.ApiTarget.KeyboardLexicalModels) {

          if(  CloudDownloadMgr.getInstance().alreadyDownloadingData(_r.additionalDownloadid))
          {
            Toast.makeText(aContext,
              aContext.getString(R.string.dictionary_download_is_running_in_background),
              Toast.LENGTH_SHORT).show();
            continue;
          }
          CloudLexicalPackageDownloadCallback _callback = new CloudLexicalPackageDownloadCallback();

          Toast.makeText(aContext,
            aContext.getString(R.string.dictionary_download_start_in_background),
            Toast.LENGTH_SHORT).show();

          CloudDownloadMgr.getInstance().executeAsDownload(aContext,
            _r.additionalDownloadid, null, _callback,
            _r.additionalDownloads.toArray(new CloudApiTypes.CloudApiParam[0]));
        }
      }
    }
  }

  /**
   * process the meta data result and prepare the additional downloads.
   * @param aContext the context
   * @param aMetaDataResult the meta data results
   */
  private void processCloudResults(Context aContext, List<MetaDataResult> aMetaDataResult) {
    for(MetaDataResult _r:aMetaDataResult) {
      if (_r.returnjson.target== CloudApiTypes.ApiTarget.Keyboard) {
        //handleKeyboardMetaData(_r);
      }
      if(_r.returnjson.target== CloudApiTypes.ApiTarget.KeyboardLexicalModels) {
        JSONArray lmData = _r.returnjson.jsonArray;
        if (lmData != null && lmData.length() > 0) {
          try {
            JSONObject modelInfo = lmData.getJSONObject(0);

            if (modelInfo.has("packageFilename") && modelInfo.has("id")) {
              String _modelID = modelInfo.getString("id");
              ArrayList<CloudApiTypes.CloudApiParam> urls = new ArrayList<>();
              urls.add(new CloudApiTypes.CloudApiParam(
                CloudApiTypes.ApiTarget.LexicalModelPackage,
                modelInfo.getString("packageFilename")));
              _r.additionalDownloadid = CloudLexicalPackageDownloadCallback.createDownloadId(_modelID);
              _r.additionalDownloads= urls;
            }
          } catch (JSONException e) {
            KMLog.LogException(TAG, "Error parsing lexical model from api.keyman.com. ", e);
          }
        }
      }
      if (_r.returnjson.target == CloudApiTypes.ApiTarget.PackageVersion) {
        JSONObject pkgData = _r.returnjson.jsonObject;

        if (pkgData != null) {
          List<Bundle> updateBundles = new ArrayList<>();
          CloudDataJsonUtil.processKeyboardPackageUpdateJSON(aContext, pkgData, updateBundles);
          CloudDataJsonUtil.processLexicalModelPackageUpdateJSON(aContext, pkgData, updateBundles);
        }
      }
    }
  }

  /**
   * handle meta data result.
   * @param theKbData the data
   */
  private void handleKeyboardMetaData(MetaDataResult theKbData)
  {
    String _key_id = theKbData.params.getAdditionalProperty(PARAM_KB_ID,String.class);
    String _lang_id = theKbData.params.getAdditionalProperty(PARAM_LANG_ID,String.class);

    JSONObject _kb_data = theKbData.returnjson.jsonObject;

    try {
      JSONObject options = _kb_data.optJSONObject(
        KMKeyboardDownloaderActivity.KMKey_Options);
      if (options == null) {
        throw new IllegalStateException("JSON file does not contain a valid \"options\" object");
      }


      // Keyman cloud _keyboard distribution via JSON
      JSONObject language = _kb_data.optJSONObject(
        KMKeyboardDownloaderActivity.KMKey_Language);
      if (language == null) {
        throw new IllegalStateException("JSON file does not contain a valid \"language\" object");
      }

      String _langName = language.optString(KMManager.KMKey_Name, "");

      JSONArray keyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);

      JSONObject _keyboard = CloudDataJsonUtil.findMatchingKeyboardByID(keyboards, _key_id);

      _key_id = _keyboard.getString(KMManager.KMKey_ID);
      String _pkgID = _keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);

      String _kbName = _keyboard.optString(KMManager.KMKey_Name, "");
      String _kbVersion = _keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");

      if (_kbName.isEmpty() || _langName.isEmpty())
        throw new IllegalStateException("JSON file does not contain a valid base values for _keyboard object");

      ArrayList<CloudApiTypes.CloudApiParam> urls = new ArrayList<>();

      urls.add(prepareKeyboardPackageDownload(options, _keyboard));

      JSONObject jsonFont = _keyboard.optJSONObject(KMManager.KMKey_Font);
      JSONObject jsonOskFont = _keyboard.optJSONObject(KMManager.KMKey_OskFont);

      if (jsonFont != null) {
        CloudDataJsonUtil.updateFontSourceToTTFFont(jsonFont);
      }
      if (jsonOskFont != null) {
        CloudDataJsonUtil.updateFontSourceToTTFFont(jsonOskFont);
      }
      String fontBaseUri = options.optString(KMKeyboardDownloaderActivity.KMKey_FontBaseURI, "");

      ArrayList<String> fontUrls = CloudDataJsonUtil.fontUrls(jsonFont, fontBaseUri, false);
      ArrayList<String> oskFontUrls = CloudDataJsonUtil.fontUrls(jsonOskFont, fontBaseUri, true);
      if (fontUrls != null) {
        for (String url : fontUrls) {
          urls.add(new CloudApiTypes.CloudApiParam(CloudApiTypes.ApiTarget.KeyboardData, url)
            .setAdditionalProperty(
              CloudKeyboardDataDownloadCallback.PARAM_PACKAGE, _pkgID));
        }
      }
      if (oskFontUrls != null) {
        for (String url : oskFontUrls) {
          if (fontUrls==null || ! fontUrls.contains(url)) {
            urls.add(new CloudApiTypes.CloudApiParam(CloudApiTypes.ApiTarget.KeyboardData, url)
              .setAdditionalProperty(
                CloudKeyboardDataDownloadCallback.PARAM_PACKAGE, _pkgID));
          }

        }
      }

      String _font = _keyboard.optString(KMManager.KMKey_Font);
      String _oskFont = _keyboard.optString(KMManager.KMKey_OskFont);
      String _customHelpLink = _keyboard.optString(KMManager.KMKey_CustomHelpLink, null);


      theKbData.additionalDownloadid = CloudKeyboardDataDownloadCallback.createDownloadId(_key_id);
      theKbData.keyboardInfo = CloudDataJsonUtil
        .createKeyboardInfoMap(
          _pkgID, _lang_id, _langName, _key_id, _kbName, _kbVersion, _font, _oskFont, _customHelpLink);
      theKbData.additionalDownloads = urls;
    }
    catch(JSONException e) {
      KMLog.LogException(TAG, "", e);
    }
  }

  /**
   * prepare keyboard package download.
   * @param aOptions the options from json
   * @param aKeyboard the keyboard
   * @return the result
   */
  private CloudApiTypes.CloudApiParam prepareKeyboardPackageDownload(JSONObject aOptions, JSONObject aKeyboard)
  {
    String kbBaseUri = aOptions.optString(KMKeyboardDownloaderActivity.KMKey_KeyboardBaseURI, "");
    if (kbBaseUri.isEmpty()) {
      throw new IllegalStateException("JSON file does not contain a valid \"keyboardBaseUri\" object");
    }

    String _kbVersion = aKeyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
    String _kbFilename = aKeyboard.optString(KMKeyboardDownloaderActivity.KMKey_Filename, "");

    if (_kbFilename.isEmpty())
      throw new IllegalStateException("JSON file does not contain a valid \"filename\" object");

    String _js_filename = _kbFilename;
    if (FileUtils.hasJavaScriptExtension(_kbFilename)) {

      int start = _kbFilename.lastIndexOf("/");
      if (start < 0) {
        start = 0;
      } else {
        start++;
      }
      if (!_kbFilename.contains("-")) {
        _js_filename = _kbFilename.substring(start, _kbFilename.length() - 3) + "-" + _kbVersion + ".js";
      } else {
        _js_filename = _kbFilename.substring(start);
      }
    }

    String _pkgID = aKeyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
    String kbUrl = kbBaseUri + _kbFilename;

    return new CloudApiTypes.CloudApiParam(CloudApiTypes.ApiTarget.KeyboardData, kbUrl)
      .setAdditionalProperty(
        CloudKeyboardDataDownloadCallback.PARAM_PACKAGE, _pkgID)
      .setAdditionalProperty(
        CloudKeyboardDataDownloadCallback.PARAM_DESTINATION_FILE_NAME, _js_filename);
  }

  /**
   * create a download id for the keyboard metadata.
   * @param  aLanguageId the language id
   * @param aKeyboardId the keyboard id
   * @return the result
   */
  public static String createDownloadId(String aLanguageId, String aKeyboardId)
  {
    return "metadata_" + aLanguageId + "_" + aKeyboardId;
  }

}
