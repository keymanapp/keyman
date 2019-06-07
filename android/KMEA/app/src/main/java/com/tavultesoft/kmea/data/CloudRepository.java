package com.tavultesoft.kmea.data;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.os.Build;
import android.util.Log;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;

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
  static private final String TAG = "CloudRepository";

  private Dataset memCachedDataset;
  private Calendar lastLoad; // To be used for Dataset caching.

  private CloudRepository() {
    // Tracks the time of the most recent cache.  We start at null to indicate that we haven't
    // tried to read the cache yet, as we don't yet have a Context instance to reference.
    lastLoad = null;
  }

  private boolean shouldUseCache(Context context) {
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
    boolean hasConnection = KMManager.hasConnection(context);

    if (cacheFile.exists()) {
      Calendar lastModified = Calendar.getInstance();
      lastModified.setTime(new Date(cacheFile.lastModified()));
      lastModified.add(Calendar.HOUR_OF_DAY, 1);
      Calendar now = Calendar.getInstance();
      return (!hasConnection || lastModified.compareTo(now) > 0);
    } else {
      return false;
    }
  }

  /**
   * Fetches a Dataset object corresponding to keyboards and models available from the Cloud API
   * services.  Unless recently cached, this object will be populated asynchronously.
   * @param context   The current Activity requesting the Dataset.
   * @param onSuccess  A callback to run immediately upon successful completion of the download.
   * @param onFailure  A callback to run immediately upon download failure.
   * @return  A Dataset object implementing the Adapter interface to be asynchronously filled.
   */
  public Dataset fetchDataset(@NonNull Context context, Runnable onSuccess, Runnable onFailure) {
    // TODO:  Do a similar check to `shouldUseCache` for our mem-cached dataset,
    //        replacing it when outdated.
    if(shouldUseCache(context)) {
      return memCachedDataset; // isn't null - checked by `shouldUseCache`.
    }

    if(memCachedDataset == null) {
      memCachedDataset = new Dataset(context);
    } else {
      // Clear the cached data and rebuild it from scratch.
      memCachedDataset.clear();
    }
    lastLoad = Calendar.getInstance(); // Mark a cache timing.

    // Get kmp.json info from installed models.
    JSONArray kmpLexicalModelsArray = JSONUtils.getLexicalModels();

    if (kmpLexicalModelsArray.length() == 0) {
      // May need to note this for handling a 'failure' check.
    } else {
      memCachedDataset.lexicalModels.addAll(processLexicalModelJSON(kmpLexicalModelsArray, context));
    }

    boolean loadFromCache = this.shouldUseCache(context, getLexicalModelCacheFile(context));
    CloudDownloadTask downloadTask = new CloudDownloadTask(context, memCachedDataset, onSuccess, onFailure);

    if(!loadFromCache) {
      String lexicalURL = String.format("%s?q", KMKeyboardDownloaderActivity.kKeymanApiModelURL);

      /* do what's possible here, rather than in the Task */

      downloadTask.execute(new CloudApiParam(ApiTarget.LexicalModels, lexicalURL));
    } else {
      // Load important parts from the cache.
      JSONArray lexData = getCachedJSONArray(context, getLexicalModelCacheFile(context));

      CloudDownloadReturns jsonData = new CloudDownloadReturns(null, lexData);

      // Call the processor method directly with the cached API data.
      downloadTask.processCloudReturns(jsonData);
    }

    return memCachedDataset;
  }

  protected File getLexicalModelCacheFile(Context context) {
    final String jsonLexicalCacheFilename = "jsonLexicalModelsCache.dat";
    return new File(context.getCacheDir(), jsonLexicalCacheFilename);
  }

  protected JSONArray getCachedJSONArray(Context context, File file) {
    JSONArray lmData = null;
    try {
      // Read from cache file
      if (file.exists()) {
        ObjectInputStream objInput = new ObjectInputStream(new FileInputStream(file));
        lmData = new JSONArray(objInput.readObject().toString());
        objInput.close();
      }
    } catch (Exception e) {
      Log.e(TAG, "Failed to read from cache file. Error: " + e);
      lmData = null;
    }

    return lmData;
  }

  /**
   * Save the JSON catalog data that's available from the cloud.
   * The catalog is saved to a unique file.  Separate files should
   * be used for each API call, such as for keyboards vs lexical models.
   * @param jsonArray - Array of JSON objects containing API return info
   */
  private static void saveJSONArrayToCache(File file, JSONArray jsonArray) {
    ObjectOutput objOutput;
    try {
      // Save to cache file
      objOutput = new ObjectOutputStream(new FileOutputStream(file));
      objOutput.writeObject(jsonArray.toString());
      objOutput.close();
    } catch (Exception e) {
      Log.e(TAG, "Failed to save to cache file. Error: " + e);
    }
  }

  protected List<LexicalModel> processLexicalModelJSON(JSONArray models, Context context) {
    List<LexicalModel> modelList = new ArrayList<>(models.length());

    try {
      // Parse the model JSON Object from the merged list of api.keyman.com query and available kmp's.
      // Known assumption:
      // 2. query is built on a single language ID so the "languages" array will only have one language
      int modelsLength = models.length();
      for (int i = 0; i < modelsLength; i++) {
        JSONObject model = models.getJSONObject(i);
        String packageID = "", modelURL = "";
        if (model.has(KMManager.KMKey_PackageID)) {
          packageID = model.getString(KMManager.KMKey_PackageID);
        } else {
          // Determine package ID from packageFilename
          modelURL = model.optString("packageFilename", "");
          packageID = FileUtils.getFilename(modelURL);
          packageID = packageID.replace(".model.kmp", "");
        }

        // api.keyman.com query returns an array of language IDs Strings while
        // kmp.json "languages" is an array of JSONObject
        String languageID = "", langName = "";
        Object obj = model.getJSONArray("languages");
        if (((JSONArray) obj).get(0) instanceof String) {
          // language name not provided, so re-use language ID
          languageID = model.getJSONArray("languages").getString(0);
          langName = languageID;
        } else if (((JSONArray) obj).get(0) instanceof JSONObject) {
          JSONObject languageObj = model.getJSONArray("languages").getJSONObject(0);
          languageID = languageObj.getString("id");
          langName = languageObj.getString("name");
        }

        String modelID = model.getString("id");
        String modelName = model.getString("name");
        String modelVersion = model.getString("version");

        String isCustom = model.optString("CustomModel", "N");
        String icon = "0";

        HashMap<String, String> hashMap = new HashMap<String, String>();
        hashMap.put(KMManager.KMKey_PackageID, packageID);
        hashMap.put(KMManager.KMKey_LanguageID, languageID);
        hashMap.put(KMManager.KMKey_LexicalModelID, modelID);
        hashMap.put(KMManager.KMKey_LexicalModelName, modelName);
        hashMap.put(KMManager.KMKey_LanguageName, langName);
        hashMap.put(KMManager.KMKey_LexicalModelVersion, modelVersion);
        hashMap.put(KMManager.KMKey_CustomModel, isCustom);
        hashMap.put("isEnabled", "true");
        hashMap.put(KMManager.KMKey_Icon, String.valueOf(R.drawable.ic_arrow_forward));

        // TODO:  (Move to PickerActivity) Display check for installed models
        String modelKey = String.format("%s_%s_%s", packageID, languageID, modelID);
        if (KeyboardPickerActivity.containsLexicalModel(context, modelKey)) {
          hashMap.put("leftIcon", String.valueOf(R.drawable.ic_check));
        } else {
          // Otherwise, include link to .kmp file
          hashMap.put(KMManager.KMKey_LexicalModelPackageFilename, modelURL);
        }
        // TODO: (Move section) END.

        modelList.add(new LexicalModel(hashMap));
      }
    } catch (JSONException e) {
      Log.e("JSONParse", "Error: " + e);
      return new ArrayList<>();  // Is this ideal?
    }

    return modelList;
  }

  private enum ApiTarget {
    Keyboards,
    LexicalModels
  }

  private static class CloudApiParam {
    public final ApiTarget target;
    public final String url;

    CloudApiParam(ApiTarget target, String url) {
      this.target = target;
      this.url = url;
    }
  }

  private static class CloudApiReturns {
    public final ApiTarget target;
    public final JSONArray jsonArray;

    public CloudApiReturns(ApiTarget target, JSONArray jsonArray) {
      this.target = target;
      this.jsonArray = jsonArray;
    }
  }

  private static class CloudDownloadReturns {
    public final JSONArray keyboardJSON;
    public final JSONArray lexicalModelJSON;

    // Used by the CloudDownloadTask, as it fits well with doInBackground's param structure.
    public CloudDownloadReturns(List<CloudApiReturns> returns) {
      JSONArray kbd = null;
      JSONArray lex = null;

      for(CloudApiReturns ret: returns) {
        switch(ret.target) {
          case Keyboards:
            kbd = ret.jsonArray;
            break;
          case LexicalModels:
            lex = ret.jsonArray;
        }
      }

      // Errors are thrown if we try to do this assignment within the loop.
      this.keyboardJSON = kbd;
      this.lexicalModelJSON = lex;
    }

    public CloudDownloadReturns(JSONArray keyboardJSON, JSONArray lexicalModelJSON) {
      this.keyboardJSON = keyboardJSON;
      this.lexicalModelJSON = lexicalModelJSON;
    }
  }

  // This is copied from LanguageListActivity to download a catalog from the cloud.
  // TODO: Keyman roadmap is to refactor to use background WorkManager in Keyman 13.0
  private class CloudDownloadTask extends AsyncTask<CloudApiParam, Integer, CloudDownloadReturns> {
    private final boolean hasConnection;
    private ProgressDialog progressDialog;

    private final Context context;
    private final Runnable success;
    private final Runnable failure;

    private final Dataset dataset;

    public CloudDownloadTask(Context context, Dataset dataset, Runnable success, Runnable failure) {
      this.context = context;
      this.dataset = dataset;
      this.hasConnection = KMManager.hasConnection(context);

      Runnable dummy = new Runnable() {
        public void run() {
          // Do nothing.
        }
      };
      this.success = success != null ? success : dummy;
      this.failure = failure != null ? failure : dummy;
    }

    protected void showProgressDialog(final Runnable finishCallback) {
      if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {
        progressDialog = new ProgressDialog(context, R.style.AppTheme_Dialog_Progress);
      } else {
        progressDialog = new ProgressDialog(context);
      }
      progressDialog.setMessage(context.getString(R.string.getting_model_catalog));
      progressDialog.setButton(DialogInterface.BUTTON_NEGATIVE, context.getString(R.string.label_cancel),
          new DialogInterface.OnClickListener() {

            @Override
            public void onClick(DialogInterface dialogInterface, int which) {
              cancel(true);
              progressDialog.dismiss();
              progressDialog = null;
              if(finishCallback != null) {
                finishCallback.run();
              }
              return;
            }
          });
      progressDialog.setCancelable(true);
      if (!((AppCompatActivity) context).isFinishing()) {
        progressDialog.show();
      } else {
        cancel(true);
        progressDialog = null;
      }
    }

    @Override
    protected void onPreExecute() {
      super.onPreExecute();

      if (hasConnection) {
        showProgressDialog(new Runnable() {
          @Override
          public void run() { // runs on 'cancel' selection.
            failure.run();
          }
        });
      }
    }

    @Override
    protected CloudDownloadReturns doInBackground(CloudApiParam... params) {
      if (isCancelled()) {
        return null;
      }

      List<CloudApiReturns> retrievedJSON = new ArrayList<>(params.length);

      for(CloudApiParam param:params) {
        JSONParser jsonParser = new JSONParser();
        JSONArray data = null;

        if (hasConnection) {
          try {
            String remoteUrl = param.url;
            data = jsonParser.getJSONObjectFromUrl(remoteUrl, JSONArray.class);
          } catch (Exception e) {
            Log.d(TAG, e.getMessage());
            data = null;
          }
        } else {
          data = null;
        }

        retrievedJSON.add(new CloudApiReturns(param.target, data));
      }

      return new CloudDownloadReturns(retrievedJSON);
    }

    protected List<LexicalModel> processLexicalModels(JSONArray lexicalJSON) {
      if (lexicalJSON == null && dataset.isEmpty()) {
        Toast.makeText(context, "Failed to access Keyman server!", Toast.LENGTH_SHORT).show();
        failure.run();
        return null;
      }

      if (!hasConnection) {
        // When offline, only use keyboards available from kmp.json
        return new ArrayList<>();
      }

      // Otherwise, merge kmpLexicalModelsArray with cloud jsonArray
      JSONArray models = (lexicalJSON != null) ? lexicalJSON : new JSONArray(); // Start with cloud model list.

      if (models == null) {
        return new ArrayList<>();
      }

      return processLexicalModelJSON(models, context);
    }

    @Override
    protected void onPostExecute(CloudDownloadReturns jsonTuple) {
      // First things first - we've successfully downloaded from the Cloud.  Cache that stuff!
      if(jsonTuple.keyboardJSON != null) {
        // TODO:  Handle keyboard caching, too.
      }
      if(jsonTuple.lexicalModelJSON != null) {
        saveJSONArrayToCache(getLexicalModelCacheFile(context), jsonTuple.lexicalModelJSON);
      }

      if (progressDialog != null && progressDialog.isShowing()) {
        try {
          progressDialog.dismiss();
          progressDialog = null;
        } catch (Exception e) {
          progressDialog = null;
        }
      }

      processCloudReturns(jsonTuple);
    }

    public void processCloudReturns(CloudDownloadReturns jsonTuple) {
      List<LexicalModel> lexicalModelsArrayList = processLexicalModels(jsonTuple.lexicalModelJSON);

      // We're about to do a big batch of edits.
      this.dataset.setNotifyOnChange(false);

      // Filter out any duplicates from already-installed models!
      for(int i = 0; i < lexicalModelsArrayList.size(); i++) {
        LexicalModel model = lexicalModelsArrayList.get(i);

        // Check for duplicates / possible updates.
        LexicalModel match = dataset.lexicalModels.findMatch(model);

        if(match != null) {
          // TODO:  Automatic update check!

          // After update stuff is reasonably handled...
          lexicalModelsArrayList.remove(model);
          i--; // Decrement our index to reflect the removal.
        } // else no match == no special handling.
      }

      // Add the cloud-returned lexical model info to the CloudRepository's LexicalModelsAdapter.
      dataset.lexicalModels.addAll(lexicalModelsArrayList);

      // And finish.
      this.dataset.notifyDataSetChanged(); // Edits are done - signal that.
      success.run();
    }
  }
}
