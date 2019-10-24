package com.tavultesoft.kmea.data;

import android.content.Context;
import android.os.Bundle;
import android.util.Log;
import android.widget.Toast;

import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.util.FileUtils;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;

public class CloudApiDownloadCallback {

  private static final boolean DEBUG_SIMULATE_UPDATES = false;

  private final Context context;

  // These keyboard query callback parameters actually aren't used at present.
  private final Runnable querySuccess;
  private final Runnable failure;
  private final CloudRepository.UpdateHandler updateHandler;

  //

  private final Dataset dataset;

  public CloudApiDownloadCallback(Context context, Dataset dataset, CloudRepository.UpdateHandler updateHandler, Runnable success, Runnable failure)
  {
    this.context = context;

    this.dataset = dataset;

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
  public Bundle updateCheck(LanguageResource cloudResource, LanguageResource existingMatch) {
    if (DEBUG_SIMULATE_UPDATES) {
      return cloudResource.buildDownloadBundle();
    }

    if (compareVersions(cloudResource, existingMatch) == FileUtils.VERSION_GREATER) {
      return cloudResource.buildDownloadBundle();
    } else {
      return null;
    }
  }

  public int compareVersions(LanguageResource addition, LanguageResource original) {
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

  void saveDataToCache(CloudApiTypes.CloudDownloadReturns jsonTuple)
  {
    // First things first - we've successfully downloaded from the Cloud.  Cache that stuff!
    if (jsonTuple.keyboardJSON != null) {
      CloudDataJsonUtil.saveJSONObjectToCache(CloudRepository.getKeyboardCacheFile(context), jsonTuple.keyboardJSON);
    }
    if (jsonTuple.lexicalModelJSON != null) {
      CloudDataJsonUtil.saveJSONArrayToCache(CloudRepository.getLexicalModelCacheFile(context), jsonTuple.lexicalModelJSON);
    }
  }


    protected JSONArray ensureInit(Context aContext,JSONArray json) {
    if (json == null && dataset.isEmpty()) {
      Toast.makeText(context, "Failed to access Keyman server!", Toast.LENGTH_SHORT).show();
      handleDownloadError();
      return null;
    }

    return (json != null) ? json : new JSONArray();
  }

    protected JSONObject ensureInit(Context aContext,JSONObject json) {
    if (json == null && dataset.isEmpty()) {
      Toast.makeText(context, "Failed to access Keyman server!", Toast.LENGTH_SHORT).show();
      handleDownloadError();
      return null;
    }

    return (json != null) ? json : new JSONObject();
  }

  void ensureInitCloudReturn(Context aContext,CloudApiTypes.CloudDownloadReturns jsonTuple)
  {
    jsonTuple.keyboardJSON = ensureInit(aContext,jsonTuple.keyboardJSON);
    jsonTuple.lexicalModelJSON = ensureInit(aContext,jsonTuple.lexicalModelJSON);
  }

  public void processCloudReturns(CloudApiTypes.CloudDownloadReturns jsonTuple, boolean executeCallbacks) {
    // Only empty if no queries returned data - we're offline.
    if (jsonTuple.isEmpty()) {
      if (this.updateHandler == null) {
        String msg = context.getString(R.string.catalog_unavailable);
        Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
      }
      this.failure.run(); // Signal failure to download to our failure callback.
      return;
    }

    List<Keyboard> keyboardsArrayList = CloudDataJsonUtil.processKeyboardJSON(jsonTuple.keyboardJSON, false);
    List<LexicalModel> lexicalModelsArrayList = CloudDataJsonUtil.processLexicalModelJSON(jsonTuple.lexicalModelJSON);

    Dataset installedData = KeyboardPickerActivity.getInstalledDataset(context);
    final List<Bundle> updateBundles = new ArrayList<>();

    // We're about to do a big batch of edits.
    this.dataset.setNotifyOnChange(false);

    // Filter out any duplicates from KMP keyboards, properly merging the lists.
    for (int i = 0; i < keyboardsArrayList.size(); i++) {
      Keyboard keyboard = keyboardsArrayList.get(i);

      // Check for duplicates / possible updates.
      Keyboard match = dataset.keyboards.findMatch(keyboard);

      if (match != null) {
        if (compareVersions(keyboard, match) == FileUtils.VERSION_GREATER) {
          dataset.keyboards.remove(match);
        } else {
          keyboardsArrayList.remove(keyboard);
          i--; // Decrement our index to reflect the removal.
        }
      } // else no match == no special handling.
    }

    // Add cloud-returned keyboard info to the CloudRepository's KeyboardsAdapter.
    dataset.keyboards.addAll(keyboardsArrayList);

    // The actual update check.
    for (int i = 0; i < installedData.keyboards.getCount(); i++) {
      Keyboard keyboard = installedData.keyboards.getItem(i);

      // Check for duplicates / possible updates.
      Keyboard match = dataset.keyboards.findMatch(keyboard);

      if (match != null) {
        Bundle bundle = updateCheck(match, keyboard);
        if (bundle != null) {
          updateBundles.add(bundle);
        }
      } // else no match == no special handling.
    }

    // Filter out any duplicates from already-installed models, properly merging the lists.
    for (int i = 0; i < lexicalModelsArrayList.size(); i++) {
      LexicalModel model = lexicalModelsArrayList.get(i);

      // Check for duplicates / possible updates.
      LexicalModel match = dataset.lexicalModels.findMatch(model);

      if (match != null) {
        if (compareVersions(model, match) == FileUtils.VERSION_GREATER) {
          dataset.lexicalModels.remove(match);
        } else {
          lexicalModelsArrayList.remove(model);
          i--; // Decrement our index to reflect the removal.
        }
      } // else no match == no special handling.
    }

    // Add the cloud-returned lexical model info to the CloudRepository's lexical models adapter.
    dataset.lexicalModels.addAll(lexicalModelsArrayList);

    // Do the actual update checks.
    for (int i = 0; i < installedData.lexicalModels.getCount(); i++) {
      LexicalModel model = installedData.lexicalModels.getItem(i);

      // Check for duplicates / possible updates.
      LexicalModel match = dataset.lexicalModels.findMatch(model);

      if (match != null) {
        Bundle bundle = updateCheck(match, model);
        if (bundle != null) {
          updateBundles.add(bundle);
        }
      } // else no match == no special handling.
    }

    if (updateBundles.size() > 0) {
      // Time for updates!
      Log.v(CloudRepository.TAG, "Performing keyboard and model updates for " + updateBundles.size() + " resources.");

      updateHandler.onUpdateDetection(updateBundles);
    }

    // And finish.
    this.dataset.notifyDataSetChanged(); // Edits are done - signal that.

    if (executeCallbacks) {
      querySuccess.run();
    }
  }
}
