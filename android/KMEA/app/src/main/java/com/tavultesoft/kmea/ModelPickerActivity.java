
package com.tavultesoft.kmea;

import android.annotation.SuppressLint;
import android.app.DialogFragment;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Typeface;
import android.inputmethodservice.Keyboard;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ImageButton;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.widget.Toolbar;

import com.tavultesoft.kmea.data.adapters.LexicalModelAdapter;
import com.tavultesoft.kmea.packages.JSONUtils;
import com.tavultesoft.kmea.packages.LexicalModelPackageProcessor;
import com.tavultesoft.kmea.packages.PackageProcessor;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.MapCompat;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static com.tavultesoft.kmea.ConfirmDialogFragment.DialogType.DIALOG_TYPE_DOWNLOAD_MODEL;

/**
 * Keyman Settings --> Languages Settings --> Language Settings --> Model List
 * Gets the list of installable lexical models from Keyman cloud and allows user to download a model.
 * Displays a list of available models for a language ID.
 */
public final class ModelPickerActivity extends AppCompatActivity {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;

  // Merged array list of lexical models to display in ListView
  private static ArrayList<HashMap<String, String>> lexicalModelsArrayList = null;
  private boolean didExecuteParser = false;

  private final static String TAG = "ModelPickerActivity";

  private static JSONArray models = null;

  protected static JSONArray models() {
    return models;
  }

  private String languageID = "";

  private static HashMap<String, HashMap<String, String>> lexicalModelsInfo = null;
  private static HashMap<String, String> lexicalModelModifiedDates = null;

  private int selectedIndex = 0;

  private static AlertDialog alertDialog;

  private static ArrayList<KeyboardEventHandler.OnKeyboardDownloadEventListener> lmDownloadEventListeners = null;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    context = this;
    setContentView(R.layout.activity_list_layout);

    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    TextView textView = (TextView) findViewById(R.id.bar_title);

    Bundle bundle = getIntent().getExtras();
    String newLanguageID = bundle.getString(KMManager.KMKey_LanguageID);

    // Sometimes we need to re-initialize the list of models that are displayed in the ListView
    if (!languageID.equalsIgnoreCase(newLanguageID) ||
        lexicalModelsArrayList == null) {
      lexicalModelsArrayList = new ArrayList<HashMap<String, String>>();
    }
    languageID = newLanguageID;

    final String languageName = bundle.getString(KMManager.KMKey_LanguageName);
    textView.setText(String.format("%s model", languageName));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);
  }

  @Override
  protected void onResume() {
    super.onResume();
    if (!didExecuteParser) {
      didExecuteParser = true;
      new JSONParse().execute();
    }
  }

  @Override
  protected void onPause() {
    super.onPause();

  }

  @Override
  public boolean onSupportNavigateUp() {
    onBackPressed();
    return true;
  }

  @Override
  public void onBackPressed() {
    finish();
  }

  @Override
  protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    if (resultCode == 1) {
      finish();
    }
  }

  // Each language ID will save to a separate cache
  private static final String cacheFilename(String languageID) {
    final String jsonCacheFilename = "jsonLexicalModelsCache";
    return String.format("%s-%s.dat", jsonCacheFilename, languageID);
  }

  protected static File getCacheFile(Context context, String languageID) {
    return new File(context.getCacheDir(), cacheFilename(languageID));
  }

  protected static JSONArray getCachedJSONArray(Context context, String languageID) {
    JSONArray lmData = null;
    try {
      // Read from cache file
      File file = getCacheFile(context, languageID);
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
   * Save the lexical model catalog that's available from the cloud.
   * The catalog is saved to a unique file based on the language ID
   * @param context
   * @param jsonArray - Array of JSON objects containing lexical model info
   * @param languageID - String of the language ID
   */
  private static void saveToCache(Context context, JSONArray jsonArray, String languageID) {
    ObjectOutput objOutput;
    try {
      // Save to cache file
      objOutput = new ObjectOutputStream(new FileOutputStream(getCacheFile(context, languageID)));
      objOutput.writeObject(jsonArray.toString());
      objOutput.close();
    } catch (Exception e) {
      Log.e(TAG, "Failed to save to cache file. Error: " + e);
    }
  }

  @SuppressLint("SimpleDateFormat")
  protected static Date getLexicalModelModifiedDate(String modelID) {
    Date date = null;
    if (lexicalModelModifiedDates != null) {
      String modDate = lexicalModelModifiedDates.get(modelID);
      if (modDate != null && !modDate.isEmpty()) {
        try {
          SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ssZZZZ");
          date = dateFormat.parse(modDate);
        } catch (Exception e) {
          date = null;
        }
      }
    }
    return date;
  }

  // This is copied from LanguageListActivity to download a catalog from the cloud.
  // TODO: Keyman roadmap is to refactor to use background WorkManager in Keyman 13.0
  private class JSONParse extends AsyncTask<Void, Integer, JSONArray> {

    private final boolean hasConnection = KMManager.hasConnection(context);
    private ProgressDialog progressDialog;
    private boolean loadFromCache;
    private final String iconKey = "icon";

    @Override
    protected void onPreExecute() {
      super.onPreExecute();
      loadFromCache = false;
      File cacheFile = getCacheFile(context, languageID);
      if (cacheFile.exists()) {
        Calendar lastModified = Calendar.getInstance();
        lastModified.setTime(new Date(cacheFile.lastModified()));
        lastModified.add(Calendar.HOUR_OF_DAY, 1);
        Calendar now = Calendar.getInstance();
        if (!hasConnection || lastModified.compareTo(now) > 0)
          loadFromCache = true;
      }

      if (hasConnection && !loadFromCache) {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.LOLLIPOP) {
          progressDialog = new ProgressDialog(context, R.style.AppTheme_Dialog_Progress);
        } else {
          progressDialog = new ProgressDialog(context);
        }
        progressDialog.setMessage(getString(R.string.getting_model_catalog));
        progressDialog.setButton(DialogInterface.BUTTON_NEGATIVE, context.getString(R.string.label_cancel),
          new DialogInterface.OnClickListener() {

            @Override
            public void onClick(DialogInterface dialogInterface, int which) {
              cancel(true);
              progressDialog.dismiss();
              progressDialog = null;
              finish();
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
    }

    @Override
    protected JSONArray doInBackground(Void... voids) {
      if (isCancelled()) {
        return null;
      }

      JSONParser jsonParser = new JSONParser();
      JSONArray lmData = null;
      if (loadFromCache) {
        lmData = getCachedJSONArray(context, languageID);
      } else if (hasConnection) {
        try {
          String remoteUrl = String.format("%s?q=bcp47:%s", KMKeyboardDownloaderActivity.kKeymanApiModelURL, languageID);
          lmData = jsonParser.getJSONObjectFromUrl(remoteUrl, JSONArray.class);
        } catch (Exception e) {
          lmData = null;
        }
      } else {
        lmData = null;
      }

      return lmData;
    }

    @Override
    protected void onPostExecute(JSONArray jsonArray) {
      if (progressDialog != null && progressDialog.isShowing()) {
        try {
          progressDialog.dismiss();
          progressDialog = null;
        } catch (Exception e) {
          progressDialog = null;
        }
      }

      // Clear pre-existing list
      lexicalModelsArrayList =  new ArrayList<HashMap<String, String>>();

      // Consolidate kmp.json info from models/
      // TODO: Filter by language ID?
      JSONArray kmpLexicalModelsArray = JSONUtils.getLexicalModels();

      if (jsonArray == null && kmpLexicalModelsArray.length() == 0) {
        Toast.makeText(context, "Failed to access Keyman server!", Toast.LENGTH_SHORT).show();
        finish();
        return;
      } else if (!loadFromCache) {
        saveToCache(context, jsonArray, languageID);
      }

      try {
        lexicalModelsInfo = new HashMap<String, HashMap<String, String>>();
        lexicalModelModifiedDates = new HashMap<String, String>();

        if (!hasConnection) {
          // When offline, only use keyboards available from kmp.json
          models = kmpLexicalModelsArray;
        } else {
          // Otherwise, merge kmpLexicalModelsArray with cloud jsonArray
          models = (jsonArray != null) ? jsonArray : new JSONArray();
          for (int i = 0; i < kmpLexicalModelsArray.length(); i++) {
            JSONObject kmpLexicalModel = kmpLexicalModelsArray.getJSONObject(i);
            String kmpModelID = kmpLexicalModel.getString("id");

            // Filter by language ID
            JSONObject languageObj = kmpLexicalModel.getJSONArray("languages").getJSONObject(0);
            if (!languageID.equalsIgnoreCase(languageObj.getString("id"))) {
              continue;
            }

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
          return;
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

          // Display check for installed models
          String modelKey = String.format("%s_%s_%s", packageID, languageID, modelID);
          if (KeyboardPickerActivity.containsLexicalModel(context, modelKey)) {
            hashMap.put("leftIcon", String.valueOf(R.drawable.ic_check));
          } else {
            // Otherwise, include link to .kmp file
            hashMap.put(KMManager.KMKey_LexicalModelPackageFilename, modelURL);
          }

          lexicalModelsArrayList.add(hashMap);
        }

        String[] from = new String[]{"leftIcon", KMManager.KMKey_LexicalModelName, KMManager.KMKey_Icon};
        int[] to = new int[]{R.id.image1, R.id.text1, R.id.image2};

        ListAdapter adapter = new KMListAdapter(context, lexicalModelsArrayList,
          R.layout.models_list_row_layout, from, to);
        listView.setAdapter(adapter);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

          @Override
          public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
            selectedIndex = position;
            HashMap<String, String> modelInfo = lexicalModelsArrayList.get(position);
            String packageID = modelInfo.get(KMManager.KMKey_PackageID);
            String languageID = modelInfo.get(KMManager.KMKey_LanguageID);
            String modelID = modelInfo.get(KMManager.KMKey_LexicalModelID);
            String modelName = modelInfo.get(KMManager.KMKey_LexicalModelName);
            String langName = modelInfo.get(KMManager.KMKey_LanguageName);

            // File check to see if lexical model already exists locally
            File modelCheck = new File(KMManager.getLexicalModelsDir() + packageID + File.separator + modelID + ".model.js");

            // Using the presence of the left icon "check" to determine if the model is installed
            boolean modelInstalled = modelInfo.containsKey("leftIcon");
            if (modelInstalled) {
              // Show Model Info
              listView.setItemChecked(position, true);
              listView.setSelection(position);

              // Start intent for selected Predictive Text Model screen
              if (!languageID.equalsIgnoreCase(modelInfo.get(KMManager.KMKey_LanguageID))) {
                Log.d(TAG, "Language ID " + languageID + " doesn't match model language ID: " +
                  modelInfo.get(KMManager.KMKey_LanguageID));
              }
              Bundle bundle = new Bundle();
              // Note: package ID of a model is different from package ID for a keyboard.
              // Language ID can be re-used
              bundle.putString(KMManager.KMKey_PackageID, packageID);
              bundle.putString(KMManager.KMKey_LanguageID, languageID);
              bundle.putString(KMManager.KMKey_LexicalModelID, modelID);
              bundle.putString(KMManager.KMKey_LexicalModelName, modelName);
              bundle.putString(KMManager.KMKey_LexicalModelVersion,
                modelInfo.get(KMManager.KMKey_LexicalModelVersion));
              bundle.putString(KMManager.KMKey_CustomModel,
                MapCompat.getOrDefault(modelInfo, KMManager.KMKey_CustomModel, "N"));
              Intent i = new Intent(context, ModelInfoActivity.class);
              i.putExtras(bundle);
              startActivityForResult(i, 1);
            } else if (modelCheck.exists()) {
              // Handle scenario where previously installed kmp already exists so
              // we only need to add the model to the list of installed models
              // Add help link
              modelInfo.put(KMManager.KMKey_CustomHelpLink, "");
              boolean result = KMManager.addLexicalModel(context, modelInfo);
              if (result) {
                Toast.makeText(context, "Model installed", Toast.LENGTH_SHORT).show();
              }

              // Don't register associated lexical model because this is not a UI view
            } else {
              // Model isn't installed so prompt to download it
              Bundle args = new Bundle();
              args.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, packageID);
              args.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, languageID);
              args.putString(KMKeyboardDownloaderActivity.ARG_MODEL_ID, modelID);
              args.putString(KMKeyboardDownloaderActivity.ARG_MODEL_NAME, modelName);
              args.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, langName);
              args.putBoolean(KMKeyboardDownloaderActivity.ARG_IS_CUSTOM, false);
              args.putString(KMKeyboardDownloaderActivity.ARG_MODEL_URL,
                modelInfo.get(KMManager.KMKey_LexicalModelPackageFilename));
              Intent i = new Intent(getApplicationContext(), KMKeyboardDownloaderActivity.class);
              i.putExtras(args);
              startActivity(i);
            }
          }
        });

        Intent i = getIntent();
        listView.setSelectionFromTop(i.getIntExtra("listPosition", 0),
          i.getIntExtra("offsetY", 0));
      } catch (JSONException e) {
        Log.e("JSONParse", "Error: " + e);
      }
    }
  }
}
