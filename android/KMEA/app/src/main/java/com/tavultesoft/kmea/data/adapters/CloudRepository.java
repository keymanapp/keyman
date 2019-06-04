package com.tavultesoft.kmea.data.adapters;

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
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.LexicalModel;
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
   * @param onFinish  A callback to run immediately upon successful completion of the download.
   * @return  A Dataset object implementing the Adapter interface to be asynchronously filled.
   */
  public Dataset fetchDataset(@NonNull Context context, Runnable onFinish) {
    boolean loadFromCache = this.shouldUseCache(context, getLexicalModelCacheFile(context));

    if(!loadFromCache) {
      CloudDownloadTask downloadTask = new CloudDownloadTask(context, onFinish);

      /* do what's possible here, rather than in the Task */

      downloadTask.execute("" /* use actual URLs here */);
    } else {
      // just load from the cache.
    }

    throw new UnsupportedOperationException();
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

  private class CloudApiReturns {
    public final JSONArray keyboardJSON;
    public final JSONArray lexicalModelJSON;

    public CloudApiReturns (JSONArray kbdJSON, JSONArray lexJSON) {
      this.keyboardJSON = kbdJSON;
      this.lexicalModelJSON = lexJSON;
    }
  }

  // This is copied from LanguageListActivity to download a catalog from the cloud.
  // TODO: Keyman roadmap is to refactor to use background WorkManager in Keyman 13.0
  private class CloudDownloadTask extends AsyncTask<String, Integer, CloudApiReturns> {
    private final boolean hasConnection;
    private ProgressDialog progressDialog;
    private boolean loadFromCache;
    private final String iconKey = "icon";

    private final Context context;
    private final Runnable finish;

    public CloudDownloadTask(Context context, Runnable finish) {
      this.context = context;
      this.hasConnection = KMManager.hasConnection(context);

      if(finish == null) {
        finish = new Runnable() {
          @Override
          public void run() {
            // Do nothing.
          }
        };
      }
      this.finish = finish;
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
            finish.run();
          }
        });
      }
    }

    @Override
    protected CloudApiReturns doInBackground(String... urls) {
      if (isCancelled()) {
        return null;
      }

      int count = urls.length;

      JSONParser jsonParser = new JSONParser();
      JSONArray lmData = null;
      if (loadFromCache) {
        lmData = getCachedJSONArray(context, getLexicalModelCacheFile(context));
      } else if (hasConnection) {
        try {
          String remoteUrl = String.format("%s?q=bcp47:%s", KMKeyboardDownloaderActivity.kKeymanApiModelURL);
          lmData = jsonParser.getJSONObjectFromUrl(remoteUrl, JSONArray.class);
        } catch (Exception e) {
          lmData = null;
        }
      } else {
        lmData = null;
      }

      return new CloudApiReturns(null, lmData);
    }

    protected List<LexicalModel> processLexicalModels(JSONArray lexicalJSON) {
      // Clear pre-existing list
      List<LexicalModel> lexicalModelsArrayList =  new ArrayList<>();

      // TODO:  This part should be utilized to init the lexical model array before the Cloud returns.
      // Consolidate kmp.json info from models.
      JSONArray kmpLexicalModelsArray = JSONUtils.getLexicalModels();

      // TODO: Should this be handled here?  Probably split instead - the lexicalJSON check is valid,
      //       but the kmpLexicalModelsArray check goes with pre-init.
      if (lexicalJSON == null && kmpLexicalModelsArray.length() == 0) {
        Toast.makeText(context, "Failed to access Keyman server!", Toast.LENGTH_SHORT).show();
        finish.run();
        return null;
      }
      // TODO: End "should this be handled?" section.

      try {
        JSONArray models;
        if (!hasConnection) {
          // When offline, only use keyboards available from kmp.json
          models = kmpLexicalModelsArray;
        } else {
          // Otherwise, merge kmpLexicalModelsArray with cloud jsonArray
          models = (lexicalJSON != null) ? lexicalJSON : new JSONArray(); // Start with cloud model list.
          for (int i = 0; i < kmpLexicalModelsArray.length(); i++) {  // Iterate over local KMPs.
            JSONObject kmpLexicalModel = kmpLexicalModelsArray.getJSONObject(i);
            String kmpModelID = kmpLexicalModel.getString("id");

            int modelIndex = JSONUtils.findID(models, kmpModelID);
            if (modelIndex == -1) {
              // Lexical model from KMP didn't exist in cloud so add new entry
              models.put(kmpLexicalModel);
            } else {
              // Lexical model already installed from local kmp so replace models entry
              models.put(modelIndex, kmpLexicalModel);
            }
          }
        }

        if (models == null) {
          return null;
        }

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

          lexicalModelsArrayList.add(new LexicalModel(hashMap));
        }

        return lexicalModelsArrayList;
      } catch (JSONException e) {
        Log.e("JSONParse", "Error: " + e);
        return null;  // Is this ideal?
      }
    }

    @Override
    protected void onPostExecute(CloudApiReturns jsonArray) {
      if (progressDialog != null && progressDialog.isShowing()) {
        try {
          progressDialog.dismiss();
          progressDialog = null;
        } catch (Exception e) {
          progressDialog = null;
        }
      }

      List<LexicalModel> lexicalModelsArrayList = processLexicalModels(jsonArray.lexicalModelJSON);

      // TODO: Set the CloudRepository's LexicalModelsAdapter to have the same members.
    }
  }
}
