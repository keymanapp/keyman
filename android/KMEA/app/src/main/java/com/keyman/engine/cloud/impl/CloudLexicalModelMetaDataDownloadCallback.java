/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.keyman.engine.cloud.impl;

import android.content.Context;
import android.os.Bundle;
import android.widget.Toast;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.R;
import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.CloudDataJsonUtil;
import com.keyman.engine.cloud.CloudDownloadMgr;
import com.keyman.engine.cloud.DownloadManagerDisabledException;
import com.keyman.engine.cloud.ICloudDownloadCallback;
import com.keyman.engine.util.KMLog;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

/**
 * Start the lexical model download when lexical model metadata is downloaded.
 */
public class CloudLexicalModelMetaDataDownloadCallback implements ICloudDownloadCallback<Void,
  List<CloudLexicalModelMetaDataDownloadCallback.MetaDataResult>>
{
  /**
   * the metadata result and all necessary downloads which should be started
   */
  static class MetaDataResult
  {
    CloudApiTypes.CloudApiReturns returnjson;
    CloudApiTypes.CloudApiParam params;
    //HashMap<String,String> keyboardInfo;
    String additionalDownloadid;
    List<CloudApiTypes.CloudApiParam> additionalDownloads;
  }

  private static final String TAG = "CloudLMMetaDldCb";

  /**
   * Additional Cloud API parameter: language id.
   */
  public static final String PARAM_LANG_ID = "lang_id";
  /**
   * Additional Cloud API parameter: lexical model id.
   */
  public static final String PARAM_LM_ID = "lm_id";

  @Override
  public void initializeContext(Context context) {

  }

  @Override
  public List<CloudLexicalModelMetaDataDownloadCallback.MetaDataResult> extractCloudResultFromDownloadSet(
      Context aContext,
      CloudApiTypes.CloudDownloadSet<Void, List<CloudLexicalModelMetaDataDownloadCallback.MetaDataResult>> aDownload) {

    List<CloudLexicalModelMetaDataDownloadCallback.MetaDataResult> _result = new ArrayList<>(aDownload.getSingleDownloads().size());

    for (CloudApiTypes.SingleCloudDownload _d : aDownload.getSingleDownloads()) {

      CloudApiTypes.CloudApiReturns _json_result = CloudDataJsonUtil.retrieveJsonFromDownload(aContext, _d);

      if (_json_result != null) {

        CloudLexicalModelMetaDataDownloadCallback.MetaDataResult _data = new CloudLexicalModelMetaDataDownloadCallback.MetaDataResult();
        _data.returnjson = _json_result;
        _data.params = _d.getCloudParams();
        _result.add(_data); // Null if offline.
      }
    }
    return _result;
  }

  @Override
  public void applyCloudDownloadToModel(Context aContext, Void aModel, List<CloudLexicalModelMetaDataDownloadCallback.MetaDataResult> aCloudResult) {
    if (aCloudResult.isEmpty()) {
      BaseActivity.makeToast(aContext, R.string.catalog_unavailable, Toast.LENGTH_SHORT);
      KMLog.LogError(TAG, "Could not reach server");
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
        if(_r.returnjson.target== CloudApiTypes.ApiTarget.KeyboardLexicalModels) {
          if(  CloudDownloadMgr.getInstance().alreadyDownloadingData(_r.additionalDownloadid))
          {
            BaseActivity.makeToast(aContext, R.string.dictionary_download_is_running_in_background, Toast.LENGTH_SHORT);
            continue;
          }
          CloudLexicalPackageDownloadCallback _callback = new CloudLexicalPackageDownloadCallback();

          BaseActivity.makeToast(aContext, R.string.dictionary_download_start_in_background, Toast.LENGTH_SHORT);

          try {
            CloudDownloadMgr.getInstance().executeAsDownload(aContext,
              _r.additionalDownloadid, null, _callback,
              _r.additionalDownloads.toArray(new CloudApiTypes.CloudApiParam[0]));
          } catch (DownloadManagerDisabledException e) {
            Toast.makeText(aContext,
              aContext.getString(R.string.update_check_unavailable),
              Toast.LENGTH_SHORT).show();
          }
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
    for (MetaDataResult _r : aMetaDataResult) {
      if (_r.returnjson.target == CloudApiTypes.ApiTarget.Keyboard) {
        //handleKeyboardMetaData(_r);
      }
      if (_r.returnjson.target == CloudApiTypes.ApiTarget.KeyboardLexicalModels) {
        processCloudResultForModel(aContext, _r);
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

  private void processCloudResultForModel(Context aContext, MetaDataResult _r) {
    JSONArray lmData = _r.returnjson.jsonArray;
    if (lmData == null || lmData.length() == 0) {
      // Not an error if api.keyman.com returns empty array of associated lexical models
      return;
    }

    try {
      ArrayList<CloudApiTypes.CloudApiParam> urls = new ArrayList<>();
      for(int i=0; i< lmData.length(); i++) {
        JSONObject modelInfo = lmData.getJSONObject(i);
        if (!modelInfo.has("packageFilename") || !modelInfo.has("id")) {
          KMLog.LogError(TAG, "Error in lexical model metadata from api.keyman.com - missing metadata");
          continue;
        }

        String _modelID = modelInfo.getString("id");
        urls.add(new CloudApiTypes.CloudApiParam(
          CloudApiTypes.ApiTarget.LexicalModelPackage,
          modelInfo.getString("packageFilename")));
        _r.additionalDownloadid = CloudLexicalPackageDownloadCallback.createDownloadId(_modelID);
        _r.additionalDownloads = urls;
      }
    } catch (JSONException e) {
      KMLog.LogException(TAG, "Error in lexical model metadata from api.keyman.com. ", e);
    }
  }

  /**
   * create a download id for the lexical model metadata.
   * @param  aLanguageId the language id
   * @return the result
   */
  public static String createDownloadId(String aLanguageId)
  {
    return "metadata_" + aLanguageId;
  }
}
