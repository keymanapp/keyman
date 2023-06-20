package com.keyman.engine.cloud.impl;

import android.content.Context;
import android.os.Bundle;
import android.util.Log;
import android.widget.Toast;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.R;
import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.CloudDataJsonUtil;
import com.keyman.engine.cloud.ICloudDownloadCallback;
import com.keyman.engine.data.CloudRepository;
import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.data.LanguageResource;
import com.keyman.engine.data.LexicalModel;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.VersionUtils;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

/**
 * Callback for cloud catalogue download.
 * Is used for download with progress and download with Clientdownloadmanager.
 */
public class CloudCatalogDownloadCallback implements ICloudDownloadCallback<Dataset, CloudCatalogDownloadReturns> {

  private static final String TAG = "CloudCatalogDownloadCb";

  /**
   * Forces update process for all installed keyboard bundles.
   * Only for testing, should be false for merging
   */
  private static final boolean DEBUG_SIMULATE_UPDATES = false;

  private final Context context;

  // These keyboard query callback parameters actually aren't used at present.
  private final Runnable querySuccess;
  private final Runnable failure;
  private final CloudRepository.UpdateHandler updateHandler;


  public CloudCatalogDownloadCallback(Context context, CloudRepository.UpdateHandler updateHandler, Runnable success, Runnable failure)
  {
    this.context = context;


    Runnable dummy = new Runnable() {
      public void run() {
        // Do nothing.
      }
    };
    this.querySuccess = success != null ? success : dummy;
    this.failure = failure != null ? failure : dummy;

    this.updateHandler = updateHandler != null ? updateHandler : new CloudRepository.UpdateHandler() {
      @Override
      public void onUpdateDetection(List<Bundle> updateBundles) {
        // Do nothing.
        return;
      }
    };
  }
  private Bundle updateCheck(LanguageResource cloudResource, LanguageResource existingMatch) {
    // For local and PR test builds, invalidate cache to make keyboard updates easier
    if (DEBUG_SIMULATE_UPDATES || VersionUtils.isLocalOrTestBuild()) {
      return cloudResource.buildDownloadBundle();
    }

    if (compareVersions(cloudResource, existingMatch) == FileUtils.VERSION_GREATER) {
      return cloudResource.buildDownloadBundle();
    } else {
      return null;
    }
  }

  private int compareVersions(LanguageResource addition, LanguageResource original) {
    // Get version from newly-downloaded keyboard.
    String addVersion = addition.getVersion();
    String origVersion = original.getVersion();

    int result = FileUtils.compareVersions(addVersion, origVersion);

    if (DEBUG_SIMULATE_UPDATES && result == FileUtils.VERSION_EQUAL) {
      // Ensures that we preserve the cloud-based version that provides download URLs.
      return FileUtils.VERSION_GREATER;
    } else {
      return result;
    }
  }

  public void handleDownloadError()
  {
    failure.run();
  }

  void saveDataToCache(CloudCatalogDownloadReturns jsonTuple)
  {
    // First things first - we've successfully downloaded from the Cloud.  Cache that stuff!
    if (jsonTuple.packagesJSON != null) {
      FileUtils.saveList(CloudDataJsonUtil.getResourcesCacheFile(context), jsonTuple.packagesJSON);
    }
  }


    private JSONArray ensureInit(Context aContext,Dataset aDataSet, JSONArray json) {
    if (json == null && aDataSet.isEmpty()) {
      BaseActivity.makeToast(context, R.string.cannot_connect, Toast.LENGTH_SHORT);
      handleDownloadError();
      return null;
    }

    return (json != null) ? json : new JSONArray();
  }

   private JSONObject ensureInit(Context aContext,Dataset aDataSet, JSONObject json) {
    if (json == null && aDataSet.isEmpty()) {
      BaseActivity.makeToast(context, R.string.cannot_connect, Toast.LENGTH_SHORT);
      handleDownloadError();
      return null;
    }

    return (json != null) ? json : new JSONObject();
  }

  protected void ensureInitCloudReturn(Context aContext, Dataset aDataSet, CloudCatalogDownloadReturns jsonTuple)
  {
    jsonTuple.packagesJSON = ensureInit(aContext, aDataSet, jsonTuple.packagesJSON);
  }

  public void processCloudReturns(Dataset aDataSet, CloudCatalogDownloadReturns jsonTuple, boolean executeCallbacks) {
    // Only empty if no queries returned data - we're offline.
    if (jsonTuple.isEmpty()) {
      this.failure.run(); // Signal failure to download to our failure callback.
      CloudRepository.shared.updateFinished();
      return;
    }

    final boolean fromKMP = false;

    Dataset installedData = KeyboardPickerActivity.getInstalledDataset(context);
    final List<Bundle> updateBundles = new ArrayList<>();

    // We're about to do a big batch of edits.
    aDataSet.setNotifyOnChange(false);

    // The actual update check
    CloudDataJsonUtil.processKeyboardPackageUpdateJSON(context, jsonTuple.packagesJSON, updateBundles);

    // Only add installed kmp keyboards
    aDataSet.keyboards.clear();
    aDataSet.keyboards.addAll(KeyboardController.getInstance().get());

    if (updateBundles.size() > 0 && !(DEBUG_SIMULATE_UPDATES && !executeCallbacks)) {
      // Time for updates!
      Log.v(TAG, "Performing keyboard and model updates for " + updateBundles.size() + " resources.");

      updateHandler.onUpdateDetection(updateBundles);
    }

    // And finish.
    aDataSet.notifyDataSetChanged(); // Edits are done - signal that.

    if (executeCallbacks) {
      querySuccess.run();
    }
  }

  @Override
  public void initializeContext(Context context) {

  }

  @Override
  public void applyCloudDownloadToModel(Context aContext, Dataset aDataSet, CloudCatalogDownloadReturns aCloudResult)
  {
    saveDataToCache(aCloudResult);

    ensureInitCloudReturn(aContext,aDataSet,aCloudResult);

    processCloudReturns(aDataSet, aCloudResult,true);

    CloudRepository.shared.updateFinished();
    aDataSet.notifyDataSetChanged();
  }

  @Override
  public CloudCatalogDownloadReturns extractCloudResultFromDownloadSet(
    Context aContext, CloudApiTypes.CloudDownloadSet<Dataset, CloudCatalogDownloadReturns> aDownload)
  {
    List<CloudApiTypes.CloudApiReturns> retrievedJSON = new ArrayList<>(aDownload.getSingleDownloads().size());

    for (CloudApiTypes.SingleCloudDownload _d : aDownload.getSingleDownloads()) {

      CloudApiTypes.CloudApiReturns _json_result = CloudDataJsonUtil.retrieveJsonFromDownload(aContext, _d);

      if (_json_result!=null)
        retrievedJSON.add(_json_result);  // Null if offline.
    }

    return new CloudCatalogDownloadReturns(retrievedJSON);
  }
}
