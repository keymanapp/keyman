package com.tavultesoft.kmea.data;

import android.content.Context;
import android.util.Log;
import android.widget.Toast;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardEventHandler;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.util.FileUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class CloudKeyboardMetaDataDownloadCallback implements ICloudDownloadCallback<Void,
  List<CloudKeyboardMetaDataDownloadCallback.MetaDataResult>>
{
  public static class MetaDataResult
  {
    CloudApiTypes.CloudApiReturns returnjson;
    CloudApiTypes.CloudApiParam params;
    HashMap<String,String> keyboardInfo;
    String keyboardId;
    List<CloudApiTypes.CloudApiParam> additionalDownloads;
  }

  private static final String TAG = "CloudKeyboardMetaDldCb";

  private static ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> downloadEventListeners = new ArrayList<>();


  public static final String PARAM_IS_CUSTOM = "is_custom";
  public static final String PARAM_LANG_ID = "lang_id";
  public static final String PARAM_KB_ID = "kb_id";


  public void setDownloadEventListeners(ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> aDownloadEventListeners) {
    downloadEventListeners.clear();
    downloadEventListeners.addAll(aDownloadEventListeners);
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

    processCloudResults(aCloudResult);

    for(MetaDataResult _r:aCloudResult)
    {
      if(_r.additionalDownloads!=null)
      {
       ;



        if(_r.returnjson.target== CloudApiTypes.ApiTarget.Keyboard)
        {
          CloudKeyboardDataDownloadCallback _callback = new CloudKeyboardDataDownloadCallback();
          _callback.setDownloadEventListeners(downloadEventListeners);
          _callback.setKeyboardInfo(_r.keyboardInfo);
          _callback.initializeContext(aContext);

          KeyboardEventHandler.notifyListeners(downloadEventListeners,
            KeyboardEventHandler.EventType.KEYBOARD_DOWNLOAD_STARTED, _r.keyboardInfo, 0);

          CloudDownloadMgr.getInstance().executeAsDownload(aContext,
            "keyboarddata_" + _r.keyboardId, null, _callback,
            _r.additionalDownloads.toArray(new CloudApiTypes.CloudApiParam[0]));
        }
        else if(_r.returnjson.target== CloudApiTypes.ApiTarget.KeyBoardLexicalModels) {
          CloudLexicalPackageDownloadCallback _callback = new CloudLexicalPackageDownloadCallback();
          _callback.setDownloadEventListeners(downloadEventListeners);
          _callback.initializeContext(aContext);

          CloudDownloadMgr.getInstance().executeAsDownload(aContext,
            "lexicalpackage_" + _r.keyboardId, null, _callback,
            _r.additionalDownloads.toArray(new CloudApiTypes.CloudApiParam[0]));
        }
        else
          continue;



      }
    }
  }

  private void processCloudResults(List<MetaDataResult> aCloudResult) {
    for(MetaDataResult _r:aCloudResult)
    {
      if(_r.returnjson.target== CloudApiTypes.ApiTarget.Keyboard)
      {
        handleKeyBoardMetaData(_r);
      }
      if(_r.returnjson.target== CloudApiTypes.ApiTarget.KeyBoardLexicalModels)
      {
        JSONArray lmData = _r.returnjson.jsonArray;
        if (lmData != null && lmData.length() > 0) {
          try
          {
            JSONObject modelInfo = lmData.getJSONObject(0);
            if (modelInfo.has("packageFilename"))
            {
              ArrayList<CloudApiTypes.CloudApiParam> urls = new ArrayList<>();
              urls.add(new CloudApiTypes.CloudApiParam(
                CloudApiTypes.ApiTarget.KeyBoardLexicalModels,
                modelInfo.getString("packageFilename")));
              _r.additionalDownloads= urls;
            }
          } catch (JSONException e) {
            Log.e(TAG, "Error parsing lexical model from api.keyman.com. " + e);
          }
        }
      }
    }
  }

  private void handleKeyBoardMetaData(MetaDataResult theKbData)
  {
    if (theKbData.params.getAdditionalProperty(PARAM_IS_CUSTOM,Boolean.class)) {
      throw new IllegalStateException("Cannot download custom non-KMP keyboard");
    }

    String _key_id = theKbData.params.getAdditionalProperty(PARAM_KB_ID,String.class);
    String _lang_id = theKbData.params.getAdditionalProperty(PARAM_LANG_ID,String.class);

    String _kbIsCustom =
      theKbData.params.getAdditionalProperty(PARAM_IS_CUSTOM,Boolean.class) ? "Y" : "N";
    JSONObject _kb_data = theKbData.returnjson.jsonObject;

    try {
      JSONObject options = _kb_data.optJSONObject(
        KMKeyboardDownloaderActivity.KMKey_Options);
      if (options == null) {
        throw new IllegalStateException("JSON file does not contain a valid \"options\" object");
      }
      String kbBaseUri = options.optString(KMKeyboardDownloaderActivity.KMKey_KeyboardBaseURI, "");
      if (kbBaseUri.isEmpty()) {
        throw new IllegalStateException("JSON file does not contain a valid \"keyboardBaseUri\" object");
      }

      String fontBaseUri = options.optString(KMKeyboardDownloaderActivity.KMKey_FontBaseURI, "");

      // Keyman cloud keyboard distribution via JSON
      JSONObject language = _kb_data.optJSONObject(
        KMKeyboardDownloaderActivity.KMKey_Language);
      if (language == null) {
        throw new IllegalStateException("JSON file does not contain a valid \"language\" object");
      }

      String _langName = language.optString(KMManager.KMKey_Name, "");

      JSONArray keyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);

      JSONObject keyboard = CloudDataJsonUtil.findMatchingKeyBoardByID(keyboards, _key_id);

      _key_id = keyboard.getString(KMManager.KMKey_ID);
      String _pkgID = keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);

      String _kbName = keyboard.optString(KMManager.KMKey_Name, "");
      String _kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
      String _kbFilename = keyboard.optString(KMKeyboardDownloaderActivity.KMKey_Filename, "");
      if (_kbName.isEmpty() || _langName.isEmpty() || _kbFilename.isEmpty())
        throw new IllegalStateException("JSON file does not contain a valid base values for keyboard object");

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
      String kbUrl = kbBaseUri + _kbFilename;
      ArrayList<CloudApiTypes.CloudApiParam> urls = new ArrayList<>();

      urls.add(
        new CloudApiTypes.CloudApiParam(CloudApiTypes.ApiTarget.KeyboardData, kbUrl)
          .setAdditionalProperty(
            CloudKeyboardDataDownloadCallback.PARAM_DESTINATION_FILE_NAME, _js_filename));

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
      if (fontUrls != null) {
        for (String url : fontUrls) {
          urls.add(new CloudApiTypes.CloudApiParam(CloudApiTypes.ApiTarget.KeyboardData, url));
        }
      }
      if (oskFontUrls != null) {
        for (String url : oskFontUrls) {
          if (!urls.contains(url))
            urls.add(new CloudApiTypes.CloudApiParam(CloudApiTypes.ApiTarget.KeyboardData, url));
          ;
        }
      }

      String _font = keyboard.optString(KMManager.KMKey_Font);
      String _oskFont = keyboard.optString(KMManager.KMKey_OskFont);

      theKbData.keyboardId = _key_id;
      theKbData.keyboardInfo = CloudDataJsonUtil
        .createKeyBoardInfoMap(
          _pkgID, _lang_id, _langName, _key_id, _kbName, _kbVersion, _kbIsCustom, _font, _oskFont);
      theKbData.additionalDownloads = urls;
    }
    catch(JSONException _e)
    {
      Log.e(TAG,_e.getLocalizedMessage(),_e);
    }
  }


}
