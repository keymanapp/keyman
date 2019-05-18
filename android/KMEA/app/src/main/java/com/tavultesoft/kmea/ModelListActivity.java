
package com.tavultesoft.kmea;

import android.annotation.SuppressLint;
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

import com.tavultesoft.kmea.packages.JSONUtils;
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

/**
 * Keyman Settings --> Languages Settings --> Language Settings --> Model List
 * Gets the list of installable lexical models from Keyman cloud and allows user to download a model.
 * Displays a list of available models for a language ID.
 */
public final class ModelListActivity extends AppCompatActivity {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static ArrayList<HashMap<String, String>> lexicalModelsArrayList = null;
  private boolean didExecuteParser = false;

  private final static String TAG = "ModelListActivity";

  private static JSONArray models = null;

  protected static JSONArray models() {
    return models;
  }

  private String languageID = null;

  private static HashMap<String, HashMap<String, String>> lexicalModelsInfo = null;
  private static HashMap<String, String> lexicalModelModifiedDates = null;

  private int selectedIndex = 0;

  private static AlertDialog alertDialog;

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
    languageID = bundle.getString(KMManager.KMKey_LanguageID);
    final String languageName = bundle.getString(KMManager.KMKey_LanguageName);
    textView.setText(String.format("%s model", languageName));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    lexicalModelsArrayList = new ArrayList<HashMap<String, String>>();
    //lexicalModelsArrayList = getModelsList(context, languageID);

    // TODO: comment out the rest of this?
    /*
    String[] from = new String[]{"leftIcon", KMManager.KMKey_LexicalModelName, KMManager.KMKey_Icon};
    int[] to = new int[]{R.id.image1, R.id.text1, R.id.image2};
    ListAdapter listAdapter = new KMListAdapter(context, lexicalModelsArrayList, R.layout.models_list_row_layout, from, to);
    listView.setAdapter(listAdapter);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        listView.setItemChecked(position, true);
        listView.setSelection(position);

        // Start intent for selected Predictive Text Model screen
        HashMap<String, String> modelInfo = lexicalModelsArrayList.get(position);
        if (!languageID.equalsIgnoreCase(modelInfo.get(KMManager.KMKey_LanguageID))) {
          Log.d(TAG, "Language ID " + languageID + " doesn't match model language ID: " + modelInfo.get(KMManager.KMKey_LanguageID));
        }
        Bundle bundle = new Bundle();
        // Note: package ID of a model is different from package ID for a keyboard.
        // Language ID can be re-used
        bundle.putString(KMManager.KMKey_PackageID, modelInfo.get(KMManager.KMKey_PackageID));
        bundle.putString(KMManager.KMKey_LanguageID, languageID);
        bundle.putString(KMManager.KMKey_LexicalModelID, modelInfo.get(KMManager.KMKey_LexicalModelID));
        bundle.putString(KMManager.KMKey_LexicalModelName, modelInfo.get(KMManager.KMKey_LexicalModelName));
        bundle.putString(KMManager.KMKey_LexicalModelVersion, modelInfo.get(KMManager.KMKey_LexicalModelVersion));
        String isCustom = MapCompat.getOrDefault(modelInfo, KMManager.KMKey_CustomModel, "N");
        bundle.putBoolean(KMManager.KMKey_CustomModel, isCustom.toUpperCase().equals("Y"));
        Intent i = new Intent(context, ModelInfoActivity.class);
        i.putExtras(bundle);
        startActivity(i);
      }
    });
    */

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

  public static ArrayList<HashMap<String, String>> getModelsList(Context context, String languageID) {
    ArrayList<HashMap<String, String>> list = new ArrayList<HashMap<String, String>>();

    // Start with the list of currently installed models
    ArrayList<HashMap<String, String>> availableLexicalModels = KeyboardPickerActivity.getLexicalModelsList(context);

    for(HashMap<String, String> modelInfo : availableLexicalModels) {
      if (modelInfo.get(KMManager.KMKey_LanguageID).equalsIgnoreCase(languageID)) {
        // Add icons showing model is installed (check)
        modelInfo.put("leftIcon", String.valueOf(R.drawable.ic_check));
        modelInfo.put(KMManager.KMKey_Icon, String.valueOf(R.drawable.ic_arrow_forward));
        modelInfo.put("isEnabled", "true");
        list.add(modelInfo);
      }
    }

    // TODO: Check the list with models available in cloud. api.keyman.com needs to be done in a background network task

    return list;
  }



  // Each language ID will save to a separate cache
  private static final String cacheFilename(String languageID) {
    final String jsonCacheFilename = "jsonLexicalModelsCache";
    return String.format("%s-%s.dat", jsonCacheFilename, languageID);
  }

  protected static File getCacheFile(Context context, String languageID) {
    return new File(context.getCacheDir(), cacheFilename(languageID));
  }

  protected static JSONArray getCachedJSONObject(Context context, String languageID) {
    JSONArray lmData;
    try {
      // Read from cache file
      ObjectInputStream objInput = new ObjectInputStream(new FileInputStream(getCacheFile(context, languageID)));
      lmData = new JSONArray(objInput.readObject().toString());
      objInput.close();
    } catch (Exception e) {
      Log.e(TAG, "Failed to read from cache file. Error: " + e);
      lmData = null;
    }

    return lmData;
  }

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
  // TODO: Refactor to use background WorkManager in 13.0
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

      // TODO: do not leave for production
      if(android.os.Debug.isDebuggerConnected())
        android.os.Debug.waitForDebugger();

      JSONParser jsonParser = new JSONParser();
      JSONArray lmData = null;
      if (loadFromCache) {
        lmData = getCachedJSONObject(context, languageID);
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

      // Consolidate kmp.json info from models/
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
          // Use default options
          models = kmpLexicalModelsArray;
        } else {
          // Otherwise, merge kmpLexicalModelsArray with cloud jsonArray
          models = jsonArray;
          for (int i = 0; i < kmpLexicalModelsArray.length(); i++) {
            JSONObject kmpLexicalModel = kmpLexicalModelsArray.getJSONObject(i);
            String kmpModelID = kmpLexicalModel.getString("id");

            int modelIndex = JSONUtils.findID(models, kmpModelID);
            if (modelIndex == -1) {
              // Add new language object
              models.put(kmpLexicalModel);
            } else {
              // Skip to next?
              /*
              // Merge language info
              JSONObject model = lexicalModels.getJSONObject(languageIndex);
              JSONArray models = lexicalModels.getJSONArray("keyboards");
              JSONArray kmpKeyboards = kmpLanguage.getJSONArray("keyboards");
              for (int j = 0; j < kmpKeyboards.length(); j++) {
                JSONObject kmpKeyboard = kmpKeyboards.getJSONObject(j);
                String kmpKeyboardID = kmpKeyboard.getString("id");
                int keyboardIndex = JSONUtils.findID(keyboards, kmpKeyboardID);
                if (keyboardIndex == -1) {
                  // Add new keyboard object
                  models.put(kmpKeyboard);
                } else {
                  // Merge keyboard info
                  JSONObject keyboard = keyboards.getJSONObject(keyboardIndex);

                  String keyboardVersion = keyboard.getString(KMManager.KMKey_KeyboardVersion);
                  String kmpKeyboardVersion = kmpKeyboard.getString(KMManager.KMKey_KeyboardVersion);
                  int versionComparison = FileUtils.compareVersions(kmpKeyboardVersion, keyboardVersion);
                  if ((versionComparison == FileUtils.VERSION_GREATER) || (versionComparison == FileUtils.VERSION_EQUAL)) {
                    // Keyboard from package >= Keyboard from cloud so replace keyboard entry with local kmp info
                    keyboards.put(keyboardIndex, kmpKeyboard);
                  }
                }
              }
              */
            }
          }
        }

        /*
        int langLength = languages.length();
        for (int i = 0; i < langLength; i++) {
          JSONObject language = languages.getJSONObject(i);

          String kbKey = "";
          String kbID = "";
          String langID = language.getString(KMManager.KMKey_ID);
          String kbName = "";
          String langName = language.getString(KMManager.KMKey_Name);
          String kbVersion = "1.0";
          String isCustom = "N";
          String kbFont = "";
          String icon = "0";
          String isEnabled = "true";
          JSONArray langKeyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);
          JSONObject keyboard = null;

          int kbLength = langKeyboards.length();
          if (kbLength == 1) {
            keyboard = langKeyboards.getJSONObject(0);
            kbID = keyboard.getString(KMManager.KMKey_ID);
            kbName = keyboard.getString(KMManager.KMKey_Name);
            kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
            kbFont = keyboard.optString(KMManager.KMKey_Font, "");

            kbKey = String.format("%s_%s", langID, kbID);
            if (KeyboardPickerActivity.containsKeyboard(context, kbKey)) {
              isEnabled = "false";
              icon = String.valueOf(R.drawable.ic_check);
            }

            HashMap<String, String> hashMap = new HashMap<String, String>();
            hashMap.put(KMManager.KMKey_KeyboardName, kbName);
            hashMap.put(KMManager.KMKey_LanguageName, langName);
            hashMap.put(KMManager.KMKey_KeyboardVersion, kbVersion);
            hashMap.put(KMManager.KMKey_CustomKeyboard, isCustom);
            hashMap.put(KMManager.KMKey_Font, kbFont);
            keyboardsInfo.put(kbKey, hashMap);

            if (keyboardModifiedDates.get(kbID) == null)
              keyboardModifiedDates.put(kbID, keyboard.getString(KMManager.KMKey_KeyboardModified));
          } else {
            icon = String.valueOf(R.drawable.ic_arrow_forward);
            for (int j = 0; j < kbLength; j++) {
              keyboard = langKeyboards.getJSONObject(j);
              kbID = keyboard.getString(KMManager.KMKey_ID);
              kbName = keyboard.getString(KMManager.KMKey_Name);
              kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
              kbFont = keyboard.optString(KMManager.KMKey_Font, "");

              kbKey = String.format("%s_%s", langID, kbID);
              HashMap<String, String> hashMap = new HashMap<String, String>();
              hashMap.put(KMManager.KMKey_KeyboardName, kbName);
              hashMap.put(KMManager.KMKey_LanguageName, langName);
              hashMap.put(KMManager.KMKey_KeyboardVersion, kbVersion);
              hashMap.put(KMManager.KMKey_CustomKeyboard, isCustom);
              hashMap.put(KMManager.KMKey_Font, kbFont);
              keyboardsInfo.put(kbKey, hashMap);

              if (keyboardModifiedDates.get(kbID) == null)
                keyboardModifiedDates.put(kbID, keyboard.getString(KMManager.KMKey_KeyboardModified));
            }
            kbName = "";
          }

          HashMap<String, String> hashMap = new HashMap<String, String>();
          hashMap.put(KMManager.KMKey_LanguageName, langName);
          hashMap.put(KMManager.KMKey_KeyboardName, kbName);
          hashMap.put(iconKey, icon);
          hashMap.put("isEnabled", isEnabled);
          languagesArrayList.add(hashMap);
        }
        */
        String[] from = new String[]{KMManager.KMKey_LanguageName, KMManager.KMKey_LexicalModelName, iconKey};
        int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};
        ListAdapter adapter = new KMListAdapter(context, lexicalModelsArrayList, R.layout.list_row_layout2, from, to);
        listView.setAdapter(adapter);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

          @Override
          public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
            selectedIndex = position;
            String modelName = lexicalModelsArrayList.get(+position).get(KMManager.KMKey_LexicalModelName);
            String langName = lexicalModelsArrayList.get(+position).get(KMManager.KMKey_LanguageName);

            boolean modelInstalled = true; // TODO: compute this
            if (modelInstalled) {
              // Show Model Info
              listView.setItemChecked(position, true);
              listView.setSelection(position);

              // Start intent for selected Predictive Text Model screen
              HashMap<String, String> modelInfo = lexicalModelsArrayList.get(position);
              if (!languageID.equalsIgnoreCase(modelInfo.get(KMManager.KMKey_LanguageID))) {
                Log.d(TAG, "Language ID " + languageID + " doesn't match model language ID: " + modelInfo.get(KMManager.KMKey_LanguageID));
              }
              Bundle bundle = new Bundle();
              // Note: package ID of a model is different from package ID for a keyboard.
              // Language ID can be re-used
              bundle.putString(KMManager.KMKey_PackageID, modelInfo.get(KMManager.KMKey_PackageID));
              bundle.putString(KMManager.KMKey_LanguageID, languageID);
              bundle.putString(KMManager.KMKey_LexicalModelID, modelInfo.get(KMManager.KMKey_LexicalModelID));
              bundle.putString(KMManager.KMKey_LexicalModelName, modelInfo.get(KMManager.KMKey_LexicalModelName));
              bundle.putString(KMManager.KMKey_LexicalModelVersion, modelInfo.get(KMManager.KMKey_LexicalModelVersion));
              String isCustom = MapCompat.getOrDefault(modelInfo, KMManager.KMKey_CustomModel, "N");
              bundle.putBoolean(KMManager.KMKey_CustomModel, isCustom.toUpperCase().equals("Y"));
              Intent i = new Intent(context, ModelInfoActivity.class);
              i.putExtras(bundle);
              startActivityForResult(i, 1);

              /*
              Intent i = new Intent(context, KeyboardListActivity.class);
              i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
              i.putExtra("selectedIndex", selectedIndex);
              int listPosition = listView.getFirstVisiblePosition();
              i.putExtra("listPosition", listPosition);
              View v = listView.getChildAt(0);
              int offsetY = (v == null) ? 0 : v.getTop();
              i.putExtra("offsetY", offsetY);
              startActivityForResult(i, 1);
              */
            } else {
              // Model file already exists locally so add association

              /*
              HashMap<String, String> kbInfo = getKeyboardInfo(selectedIndex, 0);
              final String pkgID = kbInfo.get(KMManager.KMKey_PackageID);
              final String kbID = kbInfo.get(KMManager.KMKey_KeyboardID);
              final String langID = kbInfo.get(KMManager.KMKey_LanguageID);

              if (!pkgID.equals(KMManager.KMDefault_UndefinedPackageID)) {
                // Custom keyboard already exists in models/ so just add the language association
                KeyboardPickerActivity.addKeyboard(context, kbInfo);
                KMManager.setKeyboard(pkgID, kbID, langID, modelName, langName, kFont, kOskFont);
                Toast.makeText(context, "Keyboard installed", Toast.LENGTH_SHORT).show();
                setResult(RESULT_OK);
                ((AppCompatActivity) context).finish();
              } else {
                // Keyboard needs to be downloaded
                Bundle bundle = new Bundle();
                bundle.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, pkgID);
                bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_ID, kbID);
                bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, langID);
                bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_NAME, kbName);
                bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, langName);
                bundle.putBoolean(KMKeyboardDownloaderActivity.ARG_IS_CUSTOM, isCustom.toUpperCase().equals("Y"));
                Intent i = new Intent(getApplicationContext(), KMKeyboardDownloaderActivity.class);
                i.putExtras(bundle);
                startActivity(i);
              }
              */
            }
          }
        });

        Intent i = getIntent();
        listView.setSelectionFromTop(i.getIntExtra("listPosition", 0), i.getIntExtra("offsetY", 0));
      } catch (JSONException e) {
        Log.e("JSONParse", "Error: " + e);
      }
    }
  }

}