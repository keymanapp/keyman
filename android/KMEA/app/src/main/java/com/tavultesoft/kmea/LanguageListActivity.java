/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

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

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.packages.JSONUtils;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.MapCompat;

import android.annotation.SuppressLint;
import android.content.DialogInterface;
import android.os.Build;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import androidx.appcompat.widget.Toolbar;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

/**
 * "Add new keyboard"
 * Gets the list of installable languages from Keyman cloud and allows user to download a keyboard.
 * After downloading, the new keyboard is selected.
 *
 * The language list is saved to a cache file "jsonKeyboardsCache.dat".
 */
public final class LanguageListActivity extends AppCompatActivity implements OnKeyboardDownloadEventListener {

  private Context context;
  private static Toolbar toolbar = null;
  private static ListView listView = null;

  // Merged array list of languages to display in ListView
  private static ArrayList<HashMap<String, String>> languagesArrayList = null;
  private boolean didExecuteParser = false;
  private static final String TAG = "LanguageListActivity";

  private static JSONArray languages = null;

  protected static JSONArray languages() {
    return languages;
  }

  private static JSONObject options = null;

  protected static JSONObject options() {
    return options;
  }

  private static HashMap<String, HashMap<String, String>> keyboardsInfo = null;
  private static HashMap<String, String> keyboardModifiedDates = null;

  private int selectedIndex = 0;
  private static AlertDialog alertDialog;
  //protected static int selectedIndex() { return selectedIndex; }

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
    textView.setText(getString(R.string.title_add_keyboard));

    listView = (ListView) findViewById(R.id.listView);
    listView.setFastScrollEnabled(true);

    languagesArrayList = new ArrayList<HashMap<String, String>>();
  }

  @Override
  protected void onResume() {
    super.onResume();
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);
    if (!didExecuteParser) {
      didExecuteParser = true;
      new JSONParse().execute();
    }
  }

  @Override
  protected void onPause() {
    super.onPause();

    // Intentionally not removing KeyboardDownloadEventListener to
    // ensure onKeyboardDownloadFinished() gets called
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

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Do nothing
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    String languageName = keyboardInfo.get(KMManager.KMKey_LanguageName);
    String keyboardName = keyboardInfo.get(KMManager.KMKey_KeyboardName);
    if (result > 0) {
      String packageID = keyboardInfo.get(KMManager.KMKey_PackageID);
      String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String kFont = keyboardInfo.get(KMManager.KMKey_Font);
      String kOskFont = keyboardInfo.get(KMManager.KMKey_OskFont);

      KeyboardPickerActivity.addKeyboard(this, keyboardInfo);
      KMManager.setKeyboard(packageID, keyboardID, languageID, keyboardName, languageName, kFont, kOskFont);

      if (result == 2) {
        Toast.makeText(context, context.getString(R.string.font_failed_to_download), Toast.LENGTH_LONG).show();
      }
      finish();
    } else if (!((AppCompatActivity)context).isFinishing()) {
      String title = String.format("%s: %s", languageName, keyboardName);
      showErrorDialog(context, title, context.getString(R.string.keyboard_failed_to_download));
    }
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    // Do nothing.
  }

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
    // Do nothing.
  }

  protected static HashMap<String, String> getKeyboardInfo(int languageIndex, int keyboardIndex) {
    if (languages == null)
      return null;

    HashMap<String, String> kbInfo = null;
    try {
      JSONObject language = languages.getJSONObject(languageIndex);
      String langID = language.getString(KMManager.KMKey_ID);
      String langName = language.getString(KMManager.KMKey_Name);

      JSONArray keyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);
      String pkgID = keyboards.getJSONObject(keyboardIndex).optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
      String kbID = keyboards.getJSONObject(keyboardIndex).getString(KMManager.KMKey_ID);
      String kbName = keyboards.getJSONObject(keyboardIndex).getString(KMManager.KMKey_Name);
      String kbVersion = keyboards.getJSONObject(keyboardIndex).optString(KMManager.KMKey_KeyboardVersion, "1.0");
      String isCustom = keyboards.getJSONObject(keyboardIndex).optString(KMManager.KMKey_CustomKeyboard, "N");
      String kbFont = keyboards.getJSONObject(keyboardIndex).optString(KMManager.KMKey_Font, "");

      kbInfo = new HashMap<String, String>();
      kbInfo.put(KMManager.KMKey_PackageID, pkgID);
      kbInfo.put(KMManager.KMKey_KeyboardID, kbID);
      kbInfo.put(KMManager.KMKey_LanguageID, langID);
      kbInfo.put(KMManager.KMKey_KeyboardName, kbName);
      kbInfo.put(KMManager.KMKey_LanguageName, langName);
      kbInfo.put(KMManager.KMKey_KeyboardVersion, kbVersion);
      kbInfo.put(KMManager.KMKey_CustomKeyboard, isCustom);
      kbInfo.put(KMManager.KMKey_Font, kbFont);
    } catch (JSONException e) {
      kbInfo = null;
      Log.e(TAG, "getKeyboardInfo - JSON Error: " + e);
    }

    return kbInfo;
  }

  protected static HashMap<String, HashMap<String, String>> getKeyboardsInfo(Context context) {
    if (keyboardsInfo != null) {
      return keyboardsInfo;
    } else {
      try {
        JSONObject jsonObj = getCachedJSONObject(context);
        if (jsonObj == null) {
          return null;
        }

        languages = jsonObj.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Languages).getJSONArray(KMKeyboardDownloaderActivity.KMKey_Languages);
        options = jsonObj.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Options);
        keyboardsInfo = new HashMap<String, HashMap<String, String>>();
        keyboardModifiedDates = new HashMap<String, String>();

        int langLength = languages.length();
        for (int i = 0; i < langLength; i++) {
          JSONObject language = languages.getJSONObject(i);
          String kbKey = "";
          String pkgID = "";
          String kbID = "";
          String langID = language.getString(KMManager.KMKey_ID);
          String kbName = "";
          String langName = language.getString(KMManager.KMKey_Name);
          String kbVersion = "1.0";
          String isCustom = "N";
          String kbFont = "";
          JSONArray langKeyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);
          JSONObject keyboard = null;

          int kbLength = langKeyboards.length();
          if (kbLength == 1) {
            keyboard = langKeyboards.getJSONObject(0);
            pkgID = keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
            kbID = keyboard.getString(KMManager.KMKey_ID);
            kbName = keyboard.getString(KMManager.KMKey_Name);
            kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
            kbFont = keyboard.optString(KMManager.KMKey_Font, "");

            kbKey = String.format("%s_%s", langID, kbID);
            HashMap<String, String> hashMap = new HashMap<String, String>();
            hashMap.put(KMManager.KMKey_PackageID, pkgID);
            hashMap.put(KMManager.KMKey_KeyboardName, kbName);
            hashMap.put(KMManager.KMKey_LanguageName, langName);
            hashMap.put(KMManager.KMKey_KeyboardVersion, kbVersion);
            hashMap.put(KMManager.KMKey_CustomKeyboard, isCustom);
            hashMap.put(KMManager.KMKey_Font, kbFont);
            keyboardsInfo.put(kbKey, hashMap);

            if (keyboardModifiedDates.get(kbID) == null) {
              keyboardModifiedDates.put(kbID, keyboard.getString(KMManager.KMKey_KeyboardModified));
            }
          } else {
            for (int j = 0; j < kbLength; j++) {
              keyboard = langKeyboards.getJSONObject(j);
              pkgID = keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
              kbID = keyboard.getString(KMManager.KMKey_ID);
              kbName = keyboard.getString(KMManager.KMKey_Name);
              kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
              kbFont = keyboard.optString(KMManager.KMKey_Font, "");

              kbKey = String.format("%s_%s", langID, kbID);
              HashMap<String, String> hashMap = new HashMap<String, String>();
              hashMap.put(KMManager.KMKey_PackageID, pkgID);
              hashMap.put(KMManager.KMKey_KeyboardName, kbName);
              hashMap.put(KMManager.KMKey_LanguageName, langName);
              hashMap.put(KMManager.KMKey_KeyboardVersion, kbVersion);
              hashMap.put(KMManager.KMKey_CustomKeyboard, isCustom);
              hashMap.put(KMManager.KMKey_Font, kbFont);
              keyboardsInfo.put(kbKey, hashMap);

              if (keyboardModifiedDates.get(kbID) == null) {
                keyboardModifiedDates.put(kbID, keyboard.getString(KMManager.KMKey_KeyboardModified));
              }
            }
          }
        }

        return keyboardsInfo;
      } catch (Exception e) {
        Log.e(TAG, "getKeyboardsInfo() error: " + e);
        return null;
      }
    }
  }

  protected static File getCacheFile(Context context) {
    final String jsonCacheFilename = "jsonKeyboardsCache.dat";
    return new File(context.getCacheDir(), jsonCacheFilename);
  }

  protected static JSONObject getCachedJSONObject(Context context) {
    JSONObject jsonObj = null;
    try {
      // Read from cache file
      File file = getCacheFile(context);
      if (file.exists()) {
        ObjectInputStream objInput = new ObjectInputStream(new FileInputStream(file));
        jsonObj = new JSONObject(objInput.readObject().toString());
        objInput.close();
      }
    } catch (Exception e) {
      Log.e(TAG, "Failed to read from cache file. Error: " + e);
      jsonObj = null;
    }

    return jsonObj;
  }

  private static void saveToCache(Context context, JSONObject jsonObj) {
    ObjectOutput objOutput;
    try {
      // Save to cache file
      objOutput = new ObjectOutputStream(new FileOutputStream(getCacheFile(context)));
      objOutput.writeObject(jsonObj.toString());
      objOutput.close();
    } catch (Exception e) {
      Log.e(TAG, "Failed to save to cache file. Error: " + e);
    }
  }

  @SuppressLint("SimpleDateFormat")
  protected static Date getKeyboardModifiedDate(String keyboardID) {
    Date date = null;
    if (keyboardModifiedDates != null) {
      String modDate = keyboardModifiedDates.get(keyboardID);
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

  private class JSONParse extends AsyncTask<Void, Integer, JSONObject> {

    private final boolean hasConnection = KMManager.hasConnection(context);
    private ProgressDialog progressDialog;
    private boolean loadFromCache;
    private final String iconKey = "icon";

    @Override
    protected void onPreExecute() {
      super.onPreExecute();
      loadFromCache = false;
      File cacheFile = getCacheFile(context);
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
        progressDialog.setMessage(getString(R.string.getting_keyboard_catalog));
        progressDialog.setButton(DialogInterface.BUTTON_NEGATIVE, context.getString(R.string.label_cancel),
          new DialogInterface.OnClickListener() {

            @Override
            public void onClick(DialogInterface dialogInterface, int which) {
              cancel(true);
              progressDialog.dismiss();
              progressDialog = null;
              LanguageListActivity.this.finish();
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
    protected JSONObject doInBackground(Void... voids) {
      if (isCancelled()) {
        return null;
      }

      JSONParser jsonParser = new JSONParser();
      JSONObject jsonObj = null;
      if (loadFromCache) {
        jsonObj = getCachedJSONObject(context);
      } else if (hasConnection) {
        try {
          String deviceType = getString(R.string.device_type);
          if (deviceType.equals("AndroidTablet")) {
            deviceType = "androidtablet";
          } else {
            deviceType = "androidphone";
          }
          String remoteUrl = String.format("%s?version=%s&device=%s&languageidtype=bcp47",
            KMKeyboardDownloaderActivity.kKeymanApiBaseURL, BuildConfig.VERSION_NAME, deviceType);
          jsonObj = jsonParser.getJSONObjectFromUrl(remoteUrl);
        } catch (Exception e) {
          jsonObj = null;
        }
      } else {
        jsonObj = null;
      }

      return jsonObj;
    }

    @Override
    protected void onPostExecute(JSONObject jsonObj) {
      if (progressDialog != null && progressDialog.isShowing()) {
        try {
          progressDialog.dismiss();
          progressDialog = null;
        } catch (Exception e) {
          progressDialog = null;
        }
      }

      // Consolidate kmp.json info from packages/
      JSONArray kmpLanguagesArray = JSONUtils.getLanguages();

      if (jsonObj == null && kmpLanguagesArray.length() == 0) {
        Toast.makeText(context, "Failed to access Keyman server!", Toast.LENGTH_SHORT).show();
        finish();
        return;
      } else if (!loadFromCache) {
        saveToCache(context, jsonObj);
      }

      try {
        keyboardsInfo = new HashMap<String, HashMap<String, String>>();
        keyboardModifiedDates = new HashMap<String, String>();

        if (!hasConnection) {
          // When offline, only use keyboards available from kmp.json
          // Use default options
          options = JSONUtils.defaultOptions(context.getString(R.string.device_type));
          languages = kmpLanguagesArray;
        } else {
          // Otherwise, merge kmpLanguagesArray with cloud languagesArray
          options = jsonObj.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Options);
          languages = jsonObj.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Languages).getJSONArray(KMKeyboardDownloaderActivity.KMKey_Languages);
          for (int i = 0; i < kmpLanguagesArray.length(); i++) {
            JSONObject kmpLanguage = kmpLanguagesArray.getJSONObject(i);
            String kmpLanguageID = kmpLanguage.getString("id");
            int languageIndex = JSONUtils.findID(languages, kmpLanguageID);
            if (languageIndex == -1) {
              // Add new language object
              languages.put(kmpLanguage);
            } else {
              // Merge language info
              JSONObject language = languages.getJSONObject(languageIndex);
              JSONArray keyboards = language.getJSONArray("keyboards");
              JSONArray kmpKeyboards = kmpLanguage.getJSONArray("keyboards");
              for (int j = 0; j < kmpKeyboards.length(); j++) {
                JSONObject kmpKeyboard = kmpKeyboards.getJSONObject(j);
                String kmpKeyboardID = kmpKeyboard.getString("id");
                int keyboardIndex = JSONUtils.findID(keyboards, kmpKeyboardID);
                if (keyboardIndex == -1) {
                  // Add new keyboard object
                  keyboards.put(kmpKeyboard);
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
            }
          }
        }

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
          hashMap.put(KMManager.KMKey_LanguageID, langID);
          hashMap.put(KMManager.KMKey_KeyboardName, kbName);
          hashMap.put(iconKey, icon);
          hashMap.put("isEnabled", isEnabled);
          languagesArrayList.add(hashMap);
        }

        String[] from = new String[]{KMManager.KMKey_LanguageName, KMManager.KMKey_KeyboardName, iconKey};
        int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};
        ListAdapter adapter = new KMListAdapter(context, languagesArrayList, R.layout.list_row_layout2, from, to);
        listView.setAdapter(adapter);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {

          @Override
          public void onItemClick(AdapterView<?> parent, View view, final int position, long id) {
            selectedIndex = position;
            String kbName = languagesArrayList.get(+position).get(KMManager.KMKey_KeyboardName);
            String langName = languagesArrayList.get(+position).get(KMManager.KMKey_LanguageName);

            if (kbName == "") {
              Intent i = new Intent(context, KeyboardListActivity.class);
              i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
              Map<String, String> map = languagesArrayList.get(selectedIndex);
              i.putExtra("languageCode", map.get(KMManager.KMKey_LanguageID));
              i.putExtra("languageName", map.get(KMManager.KMKey_LanguageName));
              i.putExtra("selectedIndex", selectedIndex);
              int listPosition = listView.getFirstVisiblePosition();
              i.putExtra("listPosition", listPosition);
              View v = listView.getChildAt(0);
              int offsetY = (v == null) ? 0 : v.getTop();
              i.putExtra("offsetY", offsetY);
              startActivityForResult(i, 1);
            } else {
              HashMap<String, String> kbInfo = getKeyboardInfo(selectedIndex, 0);
              final String pkgID = kbInfo.get(KMManager.KMKey_PackageID);
              final String kbID = kbInfo.get(KMManager.KMKey_KeyboardID);
              final String langID = kbInfo.get(KMManager.KMKey_LanguageID);
              String kFont = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_Font, "");
              String kOskFont = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_OskFont, "");
              String isCustom = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_CustomKeyboard, "N");

              if (!pkgID.equals(KMManager.KMDefault_UndefinedPackageID)) {
                // Custom keyboard already exists in packages/ so just add the language association
                KeyboardPickerActivity.addKeyboard(context, kbInfo);
                KMManager.setKeyboard(pkgID, kbID, langID, kbName, langName, kFont, kOskFont);
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

  private static void showErrorDialog(Context context, String title, String message) {
    AlertDialog.Builder alertDialogBuilder = new AlertDialog.Builder(context);

    alertDialogBuilder.setTitle(title);
    alertDialogBuilder
      .setMessage(message)
      .setCancelable(false)
      .setPositiveButton(context.getString(R.string.label_close),new DialogInterface.OnClickListener() {
        public void onClick(DialogInterface dialog,int id) {
        if (dialog != null) {
          dialog.dismiss();
        }
        }
      });

    alertDialog = alertDialogBuilder.create();
    alertDialog.show();
  }

}