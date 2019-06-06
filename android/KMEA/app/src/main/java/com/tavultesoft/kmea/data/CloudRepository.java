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
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;

public class CloudRepository {
  static public final CloudRepository shared = new CloudRepository();
  static private final String TAG = "CloudRepository";

  private Calendar lastLoad;

  private CloudRepository() {
    // Tracks the time of the most recent cache.  We start at null to indicate that we haven't
    // tried to read the cache yet, as we don't yet have a Context instance to reference.
    lastLoad = null;
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
    Dataset dataset = new Dataset(context);

    // Get kmp.json info from installed models.
    JSONArray kmpLexicalModelsArray = JSONUtils.getLexicalModels();

    if (kmpLexicalModelsArray.length() == 0) {
      // May need to note this for handling a 'failure' check.
    } else {
      dataset.lexicalModels.addAll(processLexicalModelJSON(kmpLexicalModelsArray, context));
    }

    boolean loadFromCache = this.shouldUseCache(context, getLexicalModelCacheFile(context));

    if(!loadFromCache) {
      CloudDownloadTask downloadTask = new CloudDownloadTask(context, dataset, onSuccess, onFailure);

      String lexicalURL = String.format("%s?q", KMKeyboardDownloaderActivity.kKeymanApiModelURL);

      /* do what's possible here, rather than in the Task */

      downloadTask.execute(new CloudApiParam(ApiTarget.LexicalModels, lexicalURL));
    } else {
      // just load from the cache.
    }

    return dataset;
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
        if (KeyboardPickerActivity.containsLexicalModel(context, modelKey)) { // FIXME:
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

    // TODO:  Process a CloudApiReturns array instead!
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
  }

  // This is copied from LanguageListActivity to download a catalog from the cloud.
  // TODO: Keyman roadmap is to refactor to use background WorkManager in Keyman 13.0
  private class CloudDownloadTask extends AsyncTask<CloudApiParam, Integer, CloudDownloadReturns> {
    private final boolean hasConnection;
    private ProgressDialog progressDialog;
    private boolean loadFromCache; // FIXME:  Needs to be assigned/removed!;

    private final Context context;
    private final Runnable success;
    private final Runnable failure;

    private final Dataset dataset;

    public CloudDownloadTask(Context context, Dataset dataset, Runnable success, Runnable failure) {
      this.context = context;
      this.dataset = dataset;
      this.hasConnection = KMManager.hasConnection(context);

      if(failure == null) {
        failure = new Runnable() {
          @Override
          public void run() {
            // Do nothing.
          }
        };
      }
      this.success = success;
      this.failure = failure;
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

      if (hasConnection && !loadFromCache) {
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

      // Facilitates debugging the AsyncTask.
      if(android.os.Debug.isDebuggerConnected()) {
        android.os.Debug.waitForDebugger();
      }

      List<CloudApiReturns> retrievedJSON = new ArrayList<>(params.length);

      for(CloudApiParam param:params) {
        JSONParser jsonParser = new JSONParser();
        JSONArray data = null;
        if (loadFromCache) {
          // Do this based on param.target if needed.
          data = getCachedJSONArray(context, getLexicalModelCacheFile(context));
        } else if (hasConnection) {
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
      // Clear pre-existing list
      List<LexicalModel> lexicalModelsArrayList =  new ArrayList<>();

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
    protected void onPostExecute(CloudDownloadReturns jsonArray) {
      if (progressDialog != null && progressDialog.isShowing()) {
        try {
          progressDialog.dismiss();
          progressDialog = null;
        } catch (Exception e) {
          progressDialog = null;
        }
      }

      List<LexicalModel> lexicalModelsArrayList = processLexicalModels(jsonArray.lexicalModelJSON);
      // TODO:  Filter out any duplicates from already-installed models!
      for(LexicalModel model: lexicalModelsArrayList) {
        // TODO: Check for duplicates / possible updates!
        //if(dataset.lexicalModels.____) // if the model's already in the list, do a thing.
      }

      // TODO: Set the CloudRepository's LexicalModelsAdapter to have the same members.

      // And finish.
      success.run();
    }
  }
}
