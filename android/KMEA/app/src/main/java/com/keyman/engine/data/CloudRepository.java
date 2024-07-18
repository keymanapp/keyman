package com.keyman.engine.data;

import android.content.Context;
import android.os.Bundle;
import android.widget.Toast;

import androidx.annotation.NonNull;

import com.keyman.engine.BaseActivity;
import com.keyman.engine.BuildConfig;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.R;
import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.cloud.impl.CloudCatalogDownloadCallback;
import com.keyman.engine.cloud.impl.CloudCatalogDownloadReturns;
import com.keyman.engine.cloud.CloudDataJsonUtil;
import com.keyman.engine.cloud.CloudDownloadMgr;
import com.keyman.engine.packages.JSONUtils;
import com.keyman.engine.util.BCP47;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.VersionUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

public class CloudRepository {
  static public final CloudRepository shared = new CloudRepository();
  private static final String TAG = "CloudRepository";

  public static final String DOWNLOAD_IDENTIFIER_CATALOGUE = "catalogue";

  public static final String API_PRODUCTION_HOST = "api.keyman.com";
  public static final String API_STAGING_HOST = "api.keyman.com"; // #7227 disabling: "api.keyman-staging.com";

  public static final String API_MODEL_LANGUAGE_FORMATSTR = "https://%s/model?q=bcp47:%s";
  public static final String API_PACKAGE_VERSION_FORMATSTR = "https://%s/package-version?platform=android%s%s";

  private Dataset memCachedDataset;
  private Calendar lastLoad; // To be used for Dataset caching.
  private boolean invalidateLexicalCache = false;

  // DEBUG:  Never allow these to be `true` in production.
  private static final boolean DEBUG_DISABLE_CACHE = false;

  private boolean updateIsRunning = false;



  private CloudRepository() {
    // Tracks the time of the most recent cache.  We start at null to indicate that we haven't
    // tried to read the cache yet, as we don't yet have a Context instance to reference.
    lastLoad = null;
  }

  public interface UpdateHandler {
    void onUpdateDetection(List<Bundle> updateBundles);
  }

  /**
   * Get the api.keyman.com host (production vs staging) based on the tier
   * @return String of api.keyman.com host
   */
  public static String getHost() {
    switch (KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME)) {
      case ALPHA:
      case BETA:
        return API_STAGING_HOST;
      default:
        return API_PRODUCTION_HOST;
    }
  }

  /**
   * Get the validity for cached resources (lexical model cache and package-version)
   * @param context the main activity of the application
   * @return boolean of the cache validity
   */
  public boolean getCacheValidity(@NonNull Context context) {
    boolean loadLexicalModelsFromCache = this.shouldUseCache(context, CloudDataJsonUtil.getLexicalModelCacheFile(context));
    boolean loadResourcesFromCache = this.shouldUseCache(context, CloudDataJsonUtil.getResourcesCacheFile(context));

    boolean cacheValid = loadLexicalModelsFromCache && loadResourcesFromCache;

    return cacheValid;
  }

  public boolean hasCache(Context context) {
    if(DEBUG_DISABLE_CACHE) {
      return false;
    }

    if(shouldUseMemCache(context)) {
      return true;
    } else {
      return getCacheValidity(context);
    }
  }

  private boolean shouldUseMemCache(Context context) {
    if(DEBUG_DISABLE_CACHE) {
      return false;
    }

    boolean hasConnection = KMManager.hasConnection(context);

    if (memCachedDataset != null) {
      Calendar lastModified = Calendar.getInstance();
      lastModified.setTime(lastLoad.getTime());
      lastModified.add(Calendar.HOUR_OF_DAY, 1);
      Calendar now = Calendar.getInstance();
      return (!hasConnection || lastModified.compareTo(now) > 0);
    } else {
      return false;
    }
  }

  private boolean shouldUseCache(Context context, File cacheFile) {
    if(DEBUG_DISABLE_CACHE) {
      return false;
    }

    boolean hasConnection = KMManager.hasConnection(context);

    // Forced cache bypass - we need to load more lexical models (signaled by invalidation).
    if(this.invalidateLexicalCache && cacheFile.equals(CloudDataJsonUtil.getLexicalModelCacheFile(context))) {
      this.invalidateLexicalCache = false;
      return false;
    }

    if (cacheFile.exists()) {
      Calendar lastModified = Calendar.getInstance();
      lastModified.setTime(new Date(cacheFile.lastModified()));
      lastModified.add(Calendar.DAY_OF_MONTH, 7);
      Calendar now = Calendar.getInstance();
      return (!hasConnection || lastModified.compareTo(now) > 0);
    } else {
      return false;
    }
  }

  /**
   * Search the available lexical models list and see there's an associated model for a
   * given language ID. Available models are from the cloud catalog and locally installed models.
   * @param context Context
   * @param languageID String of the language ID to search
   * @return LexicalModel of an associated lexical model. Null if no match found
   */
  public LexicalModel getAssociatedLexicalModel(@NonNull Context context, String languageID) {
    if (memCachedDataset != null) {
      for (int i=0; i < memCachedDataset.lexicalModels.getCount(); i++) {
        LexicalModel lm = memCachedDataset.lexicalModels.getItem(i);
        if (BCP47.languageEquals(lm.getLanguageID(), languageID)) {
          return lm;
        }
      }
    }
    return null;
  }

  // Should be called whenever a new language code starts being managed in order to help signal
  // retrieval of the language code's lexical models.
  public void invalidateLexicalModelCache(@NonNull Context context, boolean deleteCacheFile) {
    this.invalidateLexicalCache = true;

    if (deleteCacheFile) {
      // We should also pre-emptively clear out the old cache file
      // in case of an app close or crash.
      File file = CloudDataJsonUtil.getLexicalModelCacheFile(context);
      if (file != null) {
        file.delete();
      }
    }
  }

  private CloudApiTypes.CloudApiParam prepareResourcesUpdateQuery(Context aContext) {
    // Keyman cloud keyboard
    // Append each keyboard id
    String keyboardQuery = "";
    for(Keyboard k : KeyboardController.getInstance().get()) {
      String keyboardID = k.getKeyboardID();
      if (!keyboardQuery.contains(keyboardID)) {
        keyboardQuery = String.format("%s&keyboard=%s", keyboardQuery, keyboardID);
      }
    }

    String lexicalModelQuery = "";
    for(HashMap<String, String> hashMap : KMManager.getLexicalModelsList(aContext)) {
      if (hashMap != null && hashMap.containsKey(KMManager.KMKey_LexicalModelID)) {
        String lexicalModelID = hashMap.get(KMManager.KMKey_LexicalModelID);
        if (!lexicalModelQuery.contains(lexicalModelID)) {
          lexicalModelQuery = String.format("%s&model=%s", lexicalModelQuery, lexicalModelID);
        }
      }
    }

    String queryURL = String.format(API_PACKAGE_VERSION_FORMATSTR, getHost(), keyboardQuery, lexicalModelQuery);
    return new CloudApiTypes.CloudApiParam(
      CloudApiTypes.ApiTarget.PackageVersion, queryURL).setType(CloudApiTypes.JSONType.Object);
  }

  public static String prepareLexicalModelQuery(String languageID) {
    String lexicalURL = String.format(API_MODEL_LANGUAGE_FORMATSTR, getHost(), languageID);
    return lexicalURL;
  }

  /**
   * initialize the data set from cache on startup of the application
   * (not used until keyboard update is implemented)
   * @param context the main activity of the application
   * @param updateHandler An object that can handle update notification if desired.
   * @param onSuccess  A callback to be triggered on completion of all queries and operations.
   * @param onFailure A callback to be triggered upon failure of a query.
   */
  public void initializeDataSet(@NonNull Context context, UpdateHandler updateHandler, Runnable onSuccess, Runnable onFailure)
  {
    preCacheDataSet(context,updateHandler,onSuccess,onFailure);

    downloadMetaDataFromServer(context,updateHandler,onSuccess,onFailure);
  }

  /**
   * update the data set from cache on startup of the application
   * (not used until keyboard update is implemented)
   * @param context the main activity of the application
   * @param updateHandler An object that can handle update notification if desired.
   * @param onSuccess  A callback to be triggered on completion of all queries and operations.
   * @param onFailure A callback to be triggered upon failure of a query.
   */
  public void updateDatasetIfNeeded(@NonNull Context context, UpdateHandler updateHandler, Runnable onSuccess, Runnable onFailure)
  {
    boolean cacheValid = getCacheValidity(context);

    // For local and PR test builds, force update dataset
    if(cacheValid && shouldUseMemCache(context) && !VersionUtils.isLocalOrTestBuild()) {
      onSuccess.run();
      return; // isn't null - checked by `shouldUseCache`.
    }

    preCacheDataSet(context,updateHandler,onSuccess,onFailure);

    downloadMetaDataFromServer(context,updateHandler,onSuccess,onFailure);
  }


  /**
   * precache dataset and notify callbacks if no update from cloud api services is necessary.
   * @param context   The current Activity requesting the Dataset.
   * @param updateHandler  An object that can handle update notification if desired.
   * @param onSuccess  A callback to be triggered on completion of all queries and operations.
   * @param onFailure  A callback to be triggered upon failure of a query.
   */
  private void preCacheDataSet(@NonNull Context context, UpdateHandler updateHandler, Runnable onSuccess, Runnable onFailure)
  {
    boolean cacheValid = getCacheValidity(context);

    if(cacheValid && shouldUseMemCache(context)) {
      return; // isn't null - checked by `shouldUseCache`.
    }

    // Can't use the mem-cached version as is - let's prep it / reuse the instance.
    if (memCachedDataset == null) {
      memCachedDataset = new Dataset(context);
    } else {
      // Clear the cached data and rebuild it from scratch.
      memCachedDataset.clear();
    }

    lastLoad = Calendar.getInstance(); // Mark a cache timing.

    // Get the installed language codes listing.
    Dataset installedSet = KeyboardPickerActivity.getInstalledDataset(context);
    List<String> languageCodes = new ArrayList<>(installedSet.getCount());
    for(int i=0; i < installedSet.getCount(); i++) {
      languageCodes.add(installedSet.getItem(i).code);
    }

    // Get kmp.json info from installed (adhoc and cloud) models.
    // Consolidate kmp.json info from packages/
    JSONObject kmpLanguagesArray = wrapKmpKeyboardJSON(JSONUtils.getLanguages());
    JSONArray kmpLexicalModelsArray = JSONUtils.getLexicalModels();
    final boolean fromKMP = true;

    try {
      if (kmpLanguagesArray.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Languages).
        getJSONArray(KMKeyboardDownloaderActivity.KMKey_Languages).length() >  0) {
        memCachedDataset.keyboards.addAll(CloudDataJsonUtil.processKeyboardJSON(kmpLanguagesArray, true));
      }
      if (kmpLexicalModelsArray.length() > 0) {
        memCachedDataset.lexicalModels.addAll(CloudDataJsonUtil.processLexicalModelJSON(kmpLexicalModelsArray, fromKMP));
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "preCacheDataSet error ", e);
    }
    CloudCatalogDownloadCallback _download_callback = new CloudCatalogDownloadCallback(
      context, updateHandler, onSuccess, onFailure);

    // Default values:  empty JSON instances.  `null` will instead break things.
    JSONObject kbdData = new JSONObject();
    JSONArray lexData = new JSONArray();
    JSONObject pkgData = new JSONObject();

    if (cacheValid) {
      // Get the lexical model info
      lexData = CloudDataJsonUtil.getCachedJSONArray(CloudDataJsonUtil.getLexicalModelCacheFile(context));

      // In case something went wrong with the last cache attempt, which can cause a null return
      if (lexData == null) {
        lexData = new JSONArray();
        cacheValid = false;
      }

      pkgData = CloudDataJsonUtil.getCachedJSONObject(CloudDataJsonUtil.getResourcesCacheFile(context));

      if (pkgData == null) {
        pkgData = new JSONObject();
      }
    }

    // Reuse any valid parts of the cache.
    if (cacheValid) {
      CloudCatalogDownloadReturns jsonData = new CloudCatalogDownloadReturns(kbdData, lexData, pkgData);

      // Call the processor method directly with the cached API data.
      _download_callback.processCloudReturns(memCachedDataset, jsonData,
        cacheValid); // TODO:  Take params for finish, return val for failures
    }
  }

  /**
   * Fetches a Dataset object corresponding to keyboards and models available from cache or file cache.
   * @param context   The current Activity requesting the Dataset.
   * @return  A Dataset object implementing the Adapter interface to be asynchronously filled.
   */
  public Dataset fetchDataset(@NonNull Context context) {
    boolean cacheValid = getCacheValidity(context);

    if(cacheValid && shouldUseMemCache(context)) {
      return memCachedDataset; // isn't null - checked by `shouldUseCache`.
    }

    preCacheDataSet(context,null,null,null);

    if(CloudDownloadMgr.getInstance().alreadyDownloadingData(DOWNLOAD_IDENTIFIER_CATALOGUE)) {
      BaseActivity.makeToast(context, R.string.catalog_download_is_running_in_background, Toast.LENGTH_SHORT);
    }

    return memCachedDataset;
  }

  /**
   * Downloads keyboards and models available from the Cloud API
   * services.  This object will be populated asynchronously to the cached dataset.
   * @param context   The current Activity requesting the Dataset.
   * @param updateHandler  An object that can handle update notification if desired.
   * @param onSuccess  A callback to be triggered on completion of all queries and operations.
   * @param onFailure  A callback to be triggered upon failure of a query.
   */
  private void downloadMetaDataFromServer(@NonNull Context context, UpdateHandler updateHandler, Runnable onSuccess, Runnable onFailure) {
    boolean cacheValid = getCacheValidity(context);

    // For local and PR test builds, force download of metadata
    if(cacheValid && shouldUseMemCache(context) && !VersionUtils.isLocalOrTestBuild()) {
      return; // isn't null - checked by `shouldUseCache`.
    } else if (!KMManager.hasInternetPermission(context) || !KMManager.hasConnection(context)) {
      // noop if no internet permission or network connection
      return;
    }

    // check if cache file is valid
    if(cacheValid) {
      if(CloudDataJsonUtil.getCachedJSONArray(CloudDataJsonUtil.getLexicalModelCacheFile(context)) == null) {
        cacheValid = false;
      }
    }

    //    CloudApiParam[] cloudQueries = new CloudApiParam[2];
    //    int cloudQueryEntries = 0;
    List<CloudApiTypes.CloudApiParam> cloudQueries = new ArrayList<>(2);

    // For local and PR test builds, force check of keyboard updates
    if (!cacheValid || VersionUtils.isLocalOrTestBuild()) {
      cloudQueries.add(prepareResourcesUpdateQuery(context));
    }

    int cloudQueryEntries = cloudQueries.size();
    CloudCatalogDownloadCallback _download_callback = new CloudCatalogDownloadCallback(
      context, updateHandler, onSuccess, onFailure);

    if (cloudQueryEntries > 0) {
      // We need the array to be exactly the same size as our entry count.
      CloudApiTypes.CloudApiParam[] params = new CloudApiTypes.CloudApiParam[cloudQueryEntries];
      cloudQueries.toArray(params);

      if (CloudDownloadMgr.getInstance().alreadyDownloadingData(DOWNLOAD_IDENTIFIER_CATALOGUE)) {
        BaseActivity.makeToast(context, R.string.catalog_download_is_running_in_background, Toast.LENGTH_SHORT);
      } else {
        updateIsRunning = true;
        CloudDownloadMgr.getInstance().executeAsDownload(
          context, DOWNLOAD_IDENTIFIER_CATALOGUE, memCachedDataset, _download_callback, params);
      }
    }
  }



  protected JSONObject wrapKmpKeyboardJSON(JSONArray languagesArray) {
    try {
      JSONObject json = new JSONObject().put(KMKeyboardDownloaderActivity.KMKey_Languages, languagesArray);
      return new JSONObject().put(KMKeyboardDownloaderActivity.KMKey_Languages, json);
    } catch (JSONException e) {
      KMLog.LogException(TAG, "Failed to properly handle KMP JSON.  Error: ", e);
      return null;
    }
  }


  public void updateFinished()
  {
    updateIsRunning=false;
  }

  public boolean updateIsRunning() {
    return updateIsRunning;
  }
}
