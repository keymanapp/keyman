package com.tavultesoft.kmea.data;

import android.app.DownloadManager;
import android.app.ProgressDialog;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;

import com.tavultesoft.kmea.BuildConfig;
import com.tavultesoft.kmea.JSONParser;
import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.packages.JSONUtils;
import com.tavultesoft.kmea.util.FileUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

public class CloudRepository {
  static public final CloudRepository shared = new CloudRepository();
  static final String TAG = "CloudRepository";

  private Dataset memCachedDataset;
  private Calendar lastLoad; // To be used for Dataset caching.
  private boolean invalidateLexicalCache = false;

  // DEBUG:  Never allow these to be `true` in production.
  private static final boolean DEBUG_DISABLE_CACHE = false;


  private CloudRepository() {
    // Tracks the time of the most recent cache.  We start at null to indicate that we haven't
    // tried to read the cache yet, as we don't yet have a Context instance to reference.
    lastLoad = null;
  }

  public interface UpdateHandler {
    void onUpdateDetection(List<Bundle> updateBundles);
  }

  public boolean hasCache(Context context) {
    if(DEBUG_DISABLE_CACHE) {
      return false;
    }

    if(shouldUseMemCache(context)) {
      return true;
    } else {
      return shouldUseCache(context, getKeyboardCacheFile(context)) &&
          shouldUseCache(context, getLexicalModelCacheFile(context));
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
    if(this.invalidateLexicalCache && cacheFile.equals(this.getLexicalModelCacheFile(context))) {
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

  // Should be called whenever a new language code starts being managed in order to help signal
  // retrieval of the language code's lexical models.
  public void invalidateLexicalModelCache(@NonNull Context context) {
    this.invalidateLexicalCache = true;

    // We should also pre-emptively clear out the old cache file
    // in case of an app close or crash.
    File file = getLexicalModelCacheFile(context);
    file.delete();
  }

  public Dataset fetchDataset(@NonNull Context context) {
    return fetchDataset(context, null, null, null);
  }

  /**
   * Fetches a Dataset object corresponding to keyboards and models available from the Cloud API
   * services.  Unless recently cached, this object will be populated asynchronously.
   * @param context   The current Activity requesting the Dataset.
   * @param updateHandler  An object that can handle update notification if desired.
   * @param onSuccess  A callback to be triggered on completion of all queries and operations.
   * @param onFailure  A callback to be triggered upon failure of a query.
   * @return  A Dataset object implementing the Adapter interface to be asynchronously filled.
   */
  public Dataset fetchDataset(@NonNull Context context, UpdateHandler updateHandler, Runnable onSuccess, Runnable onFailure) {
    return fetchDataset(context,updateHandler,onSuccess,onFailure,false);
  }
  /**
   * Fetches a Dataset object corresponding to keyboards and models available from the Cloud API
   * services.  Unless recently cached, this object will be populated asynchronously.
   * @param context   The current Activity requesting the Dataset.
   * @param updateHandler  An object that can handle update notification if desired.
   * @param onSuccess  A callback to be triggered on completion of all queries and operations.
   * @param onFailure  A callback to be triggered upon failure of a query.
   * @return  A Dataset object implementing the Adapter interface to be asynchronously filled.
   */
  public Dataset fetchDataset(@NonNull Context context, UpdateHandler updateHandler, Runnable onSuccess, Runnable onFailure,
                              boolean anUseDownloadManager) {
    boolean loadKeyboardsFromCache = this.shouldUseCache(context, getKeyboardCacheFile(context));
    boolean loadLexicalModelsFromCache = this.shouldUseCache(context, getLexicalModelCacheFile(context));

    boolean cacheValid = loadKeyboardsFromCache && loadLexicalModelsFromCache;

    if(cacheValid && shouldUseMemCache(context)) {
      return memCachedDataset; // isn't null - checked by `shouldUseCache`.
    }

    // Can't use the mem-cached version as is - let's prep it / reuse the instance.
    if(memCachedDataset == null) {
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

    if (kmpLanguagesArray.length() == 0 && kmpLexicalModelsArray.length() == 0) {
      // May need to note this for handling a 'failure' check.
    } else {
      memCachedDataset.keyboards.addAll(CloudDataJsonUtil.processKeyboardJSON(kmpLanguagesArray, true));
      memCachedDataset.lexicalModels.addAll(CloudDataJsonUtil.processLexicalModelJSON(kmpLexicalModelsArray));
    }

     CloudApiDownloadCallback _download_callback = new CloudApiDownloadCallback(
      context, memCachedDataset, updateHandler, onSuccess, onFailure);

//    CloudApiParam[] cloudQueries = new CloudApiParam[2];
//    int cloudQueryEntries = 0;
    List<CloudApiTypes.CloudApiParam> cloudQueries = new ArrayList<>(2);
    // Default values:  empty JSON instances.  `null` will instead break things.
    JSONObject kbdData = new JSONObject();
    JSONArray lexData = new JSONArray();

    if(loadKeyboardsFromCache) {
      kbdData = CloudDataJsonUtil.getCachedJSONObject(getKeyboardCacheFile(context));

      // In case something went wrong with the last cache attempt, which can cause a null return.
      if(kbdData == null) {
        kbdData = new JSONObject();
        loadKeyboardsFromCache = false;
      }
    }

    if(!loadKeyboardsFromCache) {
      String deviceType = context.getString(R.string.device_type);
      if (deviceType.equals("AndroidTablet")) {
        deviceType = "androidtablet";
      } else {
        deviceType = "androidphone";
      }

      // Retrieves the cloud-based keyboard catalog in Android's preferred format.
      String keyboardURL = String.format("%s?version=%s&device=%s&languageidtype=bcp47",
          KMKeyboardDownloaderActivity.kKeymanApiBaseURL, BuildConfig.VERSION_NAME, deviceType);

      //cloudQueries[cloudQueryEntries++] = new CloudApiParam(ApiTarget.Keyboards, keyboardURL, JSONType.Object);
      cloudQueries.add(new CloudApiTypes.CloudApiParam(CloudApiTypes.ApiTarget.Keyboards, keyboardURL, CloudApiTypes.JSONType.Object));
    }

    if(loadLexicalModelsFromCache) {
      lexData = CloudDataJsonUtil.getCachedJSONArray(getLexicalModelCacheFile(context));

      if(lexData == null) {
        lexData = new JSONArray();
        loadLexicalModelsFromCache = false;
      }
    }

    if(!loadLexicalModelsFromCache) {
      // This allows us to directly get the full lexical model catalog.
      // TODO:  Remove and replace with commented-out code below once the proper multi-language
      //        query is ready!
      String lexicalURL = String.format("%s?q", KMKeyboardDownloaderActivity.kKeymanApiModelURL);

      cloudQueries.add(new CloudApiTypes.CloudApiParam(CloudApiTypes.ApiTarget.LexicalModels, lexicalURL, CloudApiTypes.JSONType.Array));


      // TODO: We want a list of lexical models for every language with an installed resource (kbd, lex model)
//      String lexicalURL = String.format("%s?q=bcp47:", KMKeyboardDownloaderActivity.kKeymanApiModelURL);
//
//      for(String lgCode: languageCodes) {
//        lexicalURL = String.format("%s%s,", lexicalURL, lgCode);
//      }
//
//      lexicalURL = lexicalURL.substring(0, lexicalURL.lastIndexOf(','));

      /* do what's possible here, rather than in the Task */
    }

    boolean executeCallbacks = true;
    int cloudQueryEntries = cloudQueries.size();
    if(cloudQueryEntries > 0) {
      // We need the array to be exactly the same size as our entry count.
      CloudApiTypes.CloudApiParam[] params = new CloudApiTypes.CloudApiParam[cloudQueryEntries];
      cloudQueries.toArray(params);
      if(anUseDownloadManager)
      {
        // TODO check duplicated downloads
        if(CloudDownloadMgr.getInstance().alreadyDownloadingData(context))
        {
          String msg = context.getString(R.string.catalog_download_is_running_in_background);
          Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
        }
        else
        {
          String msg = context.getString(R.string.catalog_download_start_in_background);
          Toast.makeText(context, msg, Toast.LENGTH_SHORT).show();
          CloudDownloadMgr.getInstance().executeAsDownload(context,_download_callback,params);
        }

        executeCallbacks = false;
      }
      else {
        CloudDownloadTask downloadTask = new CloudDownloadTask(context, memCachedDataset, updateHandler, onSuccess, onFailure);

        // We can pass in multiple URLs; this format is extensible if we need extra catalogs in the future.
        downloadTask.execute(params);
        executeCallbacks = false;
      }
    }

    // Reuse any valid parts of the cache.
    if(loadKeyboardsFromCache || loadLexicalModelsFromCache) {
      CloudApiTypes.CloudDownloadReturns jsonData = new CloudApiTypes.CloudDownloadReturns(kbdData, lexData);

      // Call the processor method directly with the cached API data.
      _download_callback.processCloudReturns(jsonData, executeCallbacks); // TODO:  Take params for finish, return val for failures
    }

    return memCachedDataset;
  }

  protected static File getKeyboardCacheFile(Context context) {
    final String jsonCacheFilename = "jsonKeyboardsCache.dat";
    return new File(context.getCacheDir(), jsonCacheFilename);
  }

  protected static File getLexicalModelCacheFile(Context context) {
    final String jsonLexicalCacheFilename = "jsonLexicalModelsCache.dat";
    return new File(context.getCacheDir(), jsonLexicalCacheFilename);
  }


  protected JSONObject wrapKmpKeyboardJSON(JSONArray languagesArray) {
    try {
      JSONObject json = new JSONObject().put(KMKeyboardDownloaderActivity.KMKey_Languages, languagesArray);
      return new JSONObject().put(KMKeyboardDownloaderActivity.KMKey_Languages, json);
    } catch (JSONException e) {
      Log.e(TAG, "Failed to properly handle KMP JSON.  Error: " + e);
      return null;
    }
  }









}
