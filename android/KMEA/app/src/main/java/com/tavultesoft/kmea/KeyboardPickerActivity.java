/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;

import com.tavultesoft.kmea.KeyboardEventHandler.OnKeyboardDownloadEventListener;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.MapCompat;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Typeface;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import androidx.appcompat.widget.Toolbar;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.PopupMenu;
import android.widget.TextView;
import android.widget.Toast;

public final class KeyboardPickerActivity extends AppCompatActivity implements OnKeyboardDownloadEventListener {

  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static ImageButton addButton = null;
  private static Button closeButton = null;
  private static KMKeyboardPickerAdapter listAdapter = null;
  private static ArrayList<HashMap<String, String>> keyboardsList = null;
  private static HashMap<String, String> keyboardVersions = null;
  private static ArrayList<HashMap<String, String>> lexicalModelsList = null;
  private static boolean checkingUpdates = false;
  private static int updateCount = 0;
  private static int failedUpdateCount = 0;
  private static ProgressDialog updateProgress;
  private static boolean didUpdate = false;
  private static boolean updateCheckFailed = false;
  private static boolean updateFailed = false;
  private static Calendar lastUpdateCheck = null;
  private static int selectedIndex = 0;
  private static final String TAG = "KeyboardPickerActivity";

  protected static int selectedIndex() {
    return selectedIndex;
  }

  private boolean dismissOnSelect = true;
  protected static boolean canAddNewKeyboard = true;
  protected static boolean canRemoveKeyboard = true;
  protected static boolean shouldCheckKeyboardUpdates = true;
  protected static Typeface listFont = null;

  @Override
  public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    supportRequestWindowFeature(Window.FEATURE_NO_TITLE);
    final Context context = this;
    setContentView(R.layout.keyboard_picker_list_layout);

    toolbar = (Toolbar) findViewById(R.id.list_toolbar);
    setSupportActionBar(toolbar);
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    TextView textView = (TextView) findViewById(R.id.bar_title);
    textView.setText(getString(R.string.title_keyboards));

    closeButton = (Button) findViewById(R.id.close_keyman_button);
    Bundle bundle = getIntent().getExtras();
    if (bundle != null) {
      if (!bundle.getBoolean(KMManager.KMKey_DisplayKeyboardSwitcher)) {
        closeButton.setVisibility(View.GONE);
      }
    }
    closeButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        KMManager.advanceToNextInputMode();
        if (dismissOnSelect) {
          finish();
        }
      }
    });

    listView = (ListView) findViewById(R.id.listView);
    keyboardsList = getKeyboardsList(context);
    if (keyboardsList == null) {
      keyboardsList = new ArrayList<HashMap<String, String>>();
      HashMap<String, String> kbInfo = new HashMap<String, String>();
      kbInfo.put(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
      kbInfo.put(KMManager.KMKey_KeyboardID, KMManager.KMDefault_KeyboardID);
      kbInfo.put(KMManager.KMKey_LanguageID, KMManager.KMDefault_LanguageID);
      kbInfo.put(KMManager.KMKey_KeyboardName, KMManager.KMDefault_KeyboardName);
      kbInfo.put(KMManager.KMKey_LanguageName, KMManager.KMDefault_LanguageName);
      kbInfo.put(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(
        context, KMManager.KMDefault_UndefinedPackageID, KMManager.KMDefault_KeyboardID));
      kbInfo.put(KMManager.KMKey_CustomKeyboard, "N");
      kbInfo.put(KMManager.KMKey_Font, KMManager.KMDefault_KeyboardFont);
      keyboardsList.add(kbInfo);

      // We'd prefer not to overwrite a file if it exists
      File file = new File(context.getDir("userdata", Context.MODE_PRIVATE),
        KMManager.KMFilename_KeyboardsList);
      if (!file.exists()) {
        saveList(context, KMManager.KMFilename_KeyboardsList);
      }
    }

    lexicalModelsList = getLexicalModelsList(context);
    if (lexicalModelsList == null) {
      lexicalModelsList = new ArrayList<HashMap<String, String>>();

      // We'd prefer not to overwrite a file if it exists
      File file = new File(context.getDir("userdata", Context.MODE_PRIVATE),
        KMManager.KMFilename_LexicalModelsList);
      if (!file.exists()) {
        saveList(context, KMManager.KMFilename_LexicalModelsList);
      }
    }

    String[] from = new String[]{KMManager.KMKey_LanguageName, KMManager.KMKey_KeyboardName};
    int[] to = new int[]{R.id.text1, R.id.text2};
    listAdapter = new KMKeyboardPickerAdapter(context, keyboardsList, R.layout.list_row_layout3, from, to);
    listAdapter.listFont = listFont;
    listView.setAdapter(listAdapter);
    listView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
    listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
      @Override
      public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
        switchKeyboard(position);
        if (dismissOnSelect)
          finish();
      }
    });

    listView.setOnItemLongClickListener(new OnItemLongClickListener() {
      @Override
      public boolean onItemLongClick(AdapterView<?> parent, View view, final int position, long id) {
        if (position > 0 && canRemoveKeyboard) {
          PopupMenu popup = new PopupMenu(context, view);
          popup.getMenuInflater().inflate(R.menu.popup, popup.getMenu());
          popup.setOnMenuItemClickListener(new PopupMenu.OnMenuItemClickListener() {
            public boolean onMenuItemClick(MenuItem item) {
              if (item.getItemId() == R.id.popup_delete) {
                deleteKeyboard(position);
                return true;
              } else {
                return false;
              }
            }
          });
          popup.show();
          return true;
        } else {
          return false;
        }
      }
    });

    addButton = (ImageButton) findViewById(R.id.add_button);
    addButton.setOnClickListener(new View.OnClickListener() {
      public void onClick(View v) {
        // Check that available keyboard information can be obtained via:
        // 1. connection to cloud catalog
        // 2. cached file
        // 3. local kmp.json files in packages/
        if (KMManager.hasConnection(context) || LanguageListActivity.getCacheFile(context).exists() ||
          KeyboardPickerActivity.hasKeyboardFromPackage()){
          dismissOnSelect = false;
          Intent i = new Intent(context, LanguageListActivity.class);
          i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
          context.startActivity(i);
        } else {
          AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
          dialogBuilder.setTitle(getString(R.string.title_add_keyboard));
          dialogBuilder.setMessage(String.format("\n%s\n", getString(R.string.cannot_connect)));
          dialogBuilder.setPositiveButton(getString(R.string.label_ok), null);
          AlertDialog dialog = dialogBuilder.create();
          dialog.show();
        }
      }
    });
    if (!canAddNewKeyboard) {
      addButton.setVisibility(View.GONE);
    }

    int curKbPos = getCurrentKeyboardIndex();
    setSelection(curKbPos);
  }

  @Override
  protected void onResume() {
    super.onResume();
    KMKeyboardDownloaderActivity.addKeyboardDownloadEventListener(this);
    BaseAdapter adapter = (BaseAdapter) listAdapter;
    adapter.notifyDataSetChanged();
    int curKbPos = getCurrentKeyboardIndex();
    setSelection(curKbPos);
    if (!shouldCheckKeyboardUpdates)
      return;

    final Context context = this;
    Handler handler = new Handler();
    handler.postDelayed(new Runnable() {
      @Override
      public void run() {
        boolean shouldCheckUpdate = false;
        if (lastUpdateCheck == null) {
          SharedPreferences prefs = context.getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
          Long lastUpdateCheckTime = prefs.getLong("lastUpdateCheck", 0);
          if (lastUpdateCheckTime > 0) {
            lastUpdateCheck = Calendar.getInstance();
            lastUpdateCheck.setTime(new Date(lastUpdateCheckTime));
          }
        }

        if (lastUpdateCheck != null) {
          Calendar lastChecked = Calendar.getInstance();
          lastChecked.setTime(lastUpdateCheck.getTime());
          if (updateCheckFailed || updateFailed) {
            lastChecked.add(Calendar.HOUR_OF_DAY, 1);
          } else {
            lastChecked.add(Calendar.HOUR_OF_DAY, 24);
          }

          Calendar now = Calendar.getInstance();
          if (now.compareTo(lastChecked) > 0) {
            shouldCheckUpdate = true;
          }
        } else {
          shouldCheckUpdate = true;
        }

        if (shouldCheckUpdate) {
          updateCheckFailed = false;
          updateFailed = false;
          if (!checkingUpdates) {
            checkKeyboardUpdates(context);
          }
        }
      }
    }, 1000);
  }

  @Override
  protected void onPause() {
    super.onPause();

    // Intentionally not removing KeyboardDownloadEventListener to
    // ensure onKeyboardDownloadFinished() gets called

    if (didUpdate) {
      if (KMManager.InAppKeyboard != null) {
        KMManager.InAppKeyboard.loadKeyboard();
      }
      if (KMManager.SystemKeyboard != null) {
        KMManager.SystemKeyboard.loadKeyboard();
      }
      didUpdate = false;
    }
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

  private static int getCurrentKeyboardIndex() {
    int pos = 0;

    if (keyboardsList != null) {
      int length = keyboardsList.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> kbInfo = keyboardsList.get(i);
        String langId = kbInfo.get(KMManager.KMKey_LanguageID);
        String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
        String kbKey = String.format("%s_%s", langId, kbId);
        if (kbKey.equals(KMKeyboard.currentKeyboard())) {
          pos = i;
          break;
        }
      }
    }

    return pos;
  }

  protected static boolean hasKeyboardFromPackage() {
    for(HashMap<String, String> kbInfo: keyboardsList) {
      String packageID = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
      if (!packageID.equals(KMManager.KMDefault_UndefinedPackageID)) {
        return true;
      }
    }

    return false;
  }

  private static boolean saveList(Context context, String listName) {
    boolean result;
    try {
      File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), listName);
      ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(file));
      if (listName.equalsIgnoreCase(KMManager.KMFilename_KeyboardsList)) {
        outputStream.writeObject(keyboardsList);
      } else if (listName.equalsIgnoreCase(KMManager.KMFilename_LexicalModelsList)) {
        outputStream.writeObject(lexicalModelsList);
      }
      outputStream.flush();
      outputStream.close();
      result = true;
    } catch (Exception e) {
      Log.e(TAG, "Failed to save " + listName + ". Error: " + e);
      result = false;
    }

    return result;
  }

  protected static boolean updateKeyboardsList(Context context, ArrayList<HashMap<String, String>> list) {
    boolean result;
    keyboardsList = list;
    result = saveList(context, KMManager.KMFilename_KeyboardsList);
    return result;
  }

  protected static boolean updateLexicalModelsList(Context context, ArrayList<HashMap<String, String>> list) {
    boolean result;
    lexicalModelsList = list;
    result = saveList(context, KMManager.KMFilename_LexicalModelsList);
    return result;
  }

  private void setSelection(int position) {
    listView.setItemChecked(position, true);
    listView.setSelection(position);
    selectedIndex = position;
  }

  private void switchKeyboard(int position) {
    setSelection(position);
    HashMap<String, String> kbInfo = keyboardsList.get(position);
    String pkgId = kbInfo.get(KMManager.KMKey_PackageID);
    if (pkgId == null || pkgId.isEmpty()) {
      pkgId = KMManager.KMDefault_UndefinedPackageID;
    }
    String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
    String langId = kbInfo.get(KMManager.KMKey_LanguageID);
    String kbName = kbInfo.get(KMManager.KMKey_KeyboardName);
    String langName = kbInfo.get(KMManager.KMKey_LanguageName);
    String kFont = kbInfo.get(KMManager.KMKey_Font);
    String kOskFont = kbInfo.get(KMManager.KMKey_OskFont);
    KMManager.setKeyboard(pkgId, kbId, langId, kbName, langName, kFont, kOskFont);

    // Register associated lexical model
    HashMap<String, String> lmInfo = getAssociatedLexicalModel(langId);
    if (lmInfo != null) {
      KMManager.registerLexicalModel(lmInfo);
    }

  }

  protected static boolean addKeyboard(Context context, HashMap<String, String> keyboardInfo) {
    boolean result = false;

    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList == null) {
      keyboardsList = new ArrayList<HashMap<String, String>>();
    }

    if (keyboardInfo != null) {
      String pkgID = keyboardInfo.get(KMManager.KMKey_PackageID);
      String kbID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      String langID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      if (pkgID != null && kbID != null && langID != null) {
        String kbKey = String.format("%s_%s", langID, kbID);
        if (kbKey.length() >= 3) {
          int x = getKeyboardIndex(context, kbKey);
          if (x >= 0) {
            keyboardsList.set(x, keyboardInfo);
            result = saveList(context, KMManager.KMFilename_KeyboardsList);
          } else {
            keyboardsList.add(keyboardInfo);
            result = saveList(context, KMManager.KMFilename_KeyboardsList);
            if (!result) {
              keyboardsList.remove(keyboardsList.size() - 1);
            }
          }
        }
      }
    }

    return result;
  }

  protected static boolean addLexicalModel(Context context, HashMap<String, String> lexicalModelInfo) {
    boolean result = false;

    if (lexicalModelsList == null) {
      lexicalModelsList = new ArrayList<HashMap<String, String>>();
    }

    if (lexicalModelInfo != null) {
      String pkgID = lexicalModelInfo.get(KMManager.KMKey_PackageID);
      String modelID = lexicalModelInfo.get(KMManager.KMKey_LexicalModelID);
      String langID = lexicalModelInfo.get(KMManager.KMKey_LanguageID);

      if (pkgID != null && modelID != null && langID != null) {
        String lmKey = String.format("%s_%s_%s", langID, pkgID, modelID);
        if (lmKey.length() >= 3) {
          int x = getLexicalModelIndex(context, lmKey);
          if (x >= 0) {
            lexicalModelsList.set(x, lexicalModelInfo);
            result = saveList(context, KMManager.KMFilename_LexicalModelsList);
          } else {
            lexicalModelsList.add(lexicalModelInfo);
            result = saveList(context, KMManager.KMFilename_LexicalModelsList);
            if (!result) {
              lexicalModelsList.remove(lexicalModelsList.size() - 1);
            }
          }
        }
      }
    }

    return result;
  }

  protected static boolean removeKeyboard(Context context, int position) {
    boolean result = false;
    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList != null && position >= 0 && position < keyboardsList.size()) {
      keyboardsList.remove(position);
      result = saveList(context, KMManager.KMFilename_KeyboardsList);
    }

    return result;
  }

  protected void deleteKeyboard(int position) {
    int curKbPos = getCurrentKeyboardIndex();
    boolean result = removeKeyboard(this, position);
    ;

    if (result) {
      Toast.makeText(this, "Keyboard deleted", Toast.LENGTH_SHORT).show();
      BaseAdapter adapter = (BaseAdapter) listAdapter;
      adapter.notifyDataSetChanged();
      if (position == curKbPos) {
        switchKeyboard(0);
      } else {
        curKbPos = getCurrentKeyboardIndex();
        setSelection(curKbPos);
      }
    }
  }

  @SuppressWarnings("unchecked")
  private static ArrayList<HashMap<String, String>> getList(Context context, String filename) {
    ArrayList<HashMap<String, String>> list = null;

    File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), filename);
    if (file.exists()) {
      try {
        ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
        list = (ArrayList<HashMap<String, String>>) inputStream.readObject();
        inputStream.close();
      } catch (Exception e) {
        Log.e(TAG, "Failed to read " + filename + ". Error: " + e);
        list = null;
      }
    }

    return list;
  }

  protected static ArrayList<HashMap<String, String>> getKeyboardsList(Context context) {
    return getList(context, KMManager.KMFilename_KeyboardsList);
  }

  protected static ArrayList<HashMap<String, String>> getLexicalModelsList(Context context) {
    return getList(context, KMManager.KMFilename_LexicalModelsList);
  }

  protected static boolean containsKeyboard(Context context, String keyboardKey) {
    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    String kbKeys = "";
    if (keyboardsList != null) {
      int length = keyboardsList.size();
      for (int i = 0; i < length; i++) {
        kbKeys += String.format("%s_%s", keyboardsList.get(i).get(KMManager.KMKey_LanguageID), keyboardsList.get(i).get(KMManager.KMKey_KeyboardID));
        if (i < length - 1) {
          kbKeys += ",";
        }
      }
    }

    return kbKeys.contains(keyboardKey);
  }

  protected static int getCurrentKeyboardIndex(Context context) {
    int index = -1;

    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList != null) {
      int length = keyboardsList.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> kbInfo = keyboardsList.get(i);
        String langId = kbInfo.get(KMManager.KMKey_LanguageID);
        String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
        String kbKey = String.format("%s_%s", langId, kbId);
        if (kbKey.equals(KMKeyboard.currentKeyboard())) {
          index = i;
          break;
        }
      }
    }

    return index;
  }

  protected static HashMap<String, String> getCurrentKeyboardInfo(Context context) {
    int index = getCurrentKeyboardIndex(context);
    if (index >= 0) {
      return keyboardsList.get(index);
    } else {
      return null;
    }
  }

  protected static int getKeyboardIndex(Context context, String keyboardKey) {
    int index = -1;

    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList != null) {
      int length = keyboardsList.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> kbInfo = keyboardsList.get(i);
        String langId = kbInfo.get(KMManager.KMKey_LanguageID);
        String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
        String kbKey = String.format("%s_%s", langId, kbId);
        if (kbKey.equals(keyboardKey)) {
          index = i;
          break;
        }
      }
    }

    return index;
  }

  /**
   * Get the list of associated keyboard names for a given language ID
   * @param langId
   * @return ArrayList of keyboard names
   */
  protected static ArrayList<String> getAssociatedKeyboards(String langId) {
    if (keyboardsList != null) {
      ArrayList<String> associatedKeyboards = new ArrayList<String>();
      for (HashMap<String, String> keyboardInfo: keyboardsList) {
        if (keyboardInfo.get(KMManager.KMKey_LanguageID).equalsIgnoreCase(langId)) {
          associatedKeyboards.add(keyboardInfo.get(KMManager.KMKey_KeyboardName));
        }
      }
      return associatedKeyboards;
    }

    return null;
  }

  protected static int getLexicalModelIndex(Context context, String lexicalModelKey) {
    int index = -1;

    if (lexicalModelsList == null) {
      lexicalModelsList = getLexicalModelsList(context);
    }

    if (lexicalModelsList != null) {
      int length = lexicalModelsList.size();
      for (int i=0; i < length; i++) {
        HashMap<String, String> lmInfo = lexicalModelsList.get(i);
        String langId = lmInfo.get(KMManager.KMKey_LanguageID);
        String pkgId = lmInfo.get(KMManager.KMKey_PackageID);
        String lmId = lmInfo.get(KMManager.KMKey_LexicalModelID);
        String lmKey = String.format("%s_%s_%s", langId, pkgId, lmId);
        if (lmKey.equals(lexicalModelKey)) {
          index = i;
          break;
        }
      }
    }

    return index;
  }

  protected static HashMap<String, String> getAssociatedLexicalModel(String langId) {
    if (lexicalModelsList != null) {
      int length = lexicalModelsList.size();
      for (int i = 0; i < length; i++) {
        HashMap<String, String> lmInfo = lexicalModelsList.get(i);
        if (langId.equalsIgnoreCase(lmInfo.get(KMManager.KMKey_LanguageID))) {
          return lmInfo;
        }
      }
    }

    return null;
  }

  protected static HashMap<String, String> getKeyboardInfo(Context context, int index) {
    if (index < 0) {
      return null;
    }

    if (keyboardsList == null) {
      keyboardsList = getKeyboardsList(context);
    }

    if (keyboardsList != null && index < keyboardsList.size()) {
      HashMap<String, String> kbInfo = keyboardsList.get(index);
      String pkgID = kbInfo.get(KMManager.KMKey_PackageID);
      if (pkgID == null || pkgID.isEmpty()) {
        kbInfo.put(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
      }
      return kbInfo;
    }

    return null;
  }

  protected static void showLanguageList(Context context) {
    if (KMManager.hasConnection(context) || LanguageListActivity.getCacheFile(context).exists()) {
      Intent i = new Intent(context, LanguageListActivity.class);
      i.addFlags(Intent.FLAG_ACTIVITY_NO_HISTORY);
      context.startActivity(i);
    } else {
      AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
      dialogBuilder.setTitle(context.getString(R.string.title_add_keyboard));
      dialogBuilder.setMessage(String.format("\n%s\n", context.getString(R.string.cannot_connect)));
      dialogBuilder.setPositiveButton(context.getString(R.string.label_ok), null);
      AlertDialog dialog = dialogBuilder.create();
      dialog.show();
    }
  }

  private static void checkKeyboardUpdates(final Context context) {
    new AsyncTask<Void, Integer, Integer>() {
      private final boolean hasConnection = KMManager.hasConnection(context);
      private ProgressDialog progressDialog;
      JSONParser jsonParser = new JSONParser();

      @Override
      protected void onPreExecute() {
        super.onPreExecute();
        checkingUpdates = true;
        if (hasConnection) {
          progressDialog = new ProgressDialog(context);
          progressDialog.setMessage(context.getString(R.string.checking_keyboard_updates));
          progressDialog.setCancelable(false);
          if (!((AppCompatActivity) context).isFinishing()) {
            progressDialog.show();
          } else {
            cancel(true);
            progressDialog = null;
          }
        }
      }

      @Override
      protected Integer doInBackground(Void... voids) {
        int ret = 0;
        if (hasConnection && !isCancelled()) {
          try {
            String deviceType = context.getString(R.string.device_type);
            if (deviceType.equals("AndroidTablet")) {
              deviceType = "androidtablet";
            } else {
              deviceType = "androidphone";
            }

            keyboardVersions = new HashMap<String, String>();
            int len = keyboardsList.size();
            for (int i = 0; i < len; i++) {
              String packageID = keyboardsList.get(i).get(KMManager.KMKey_PackageID);
              String languageID = keyboardsList.get(i).get(KMManager.KMKey_LanguageID);
              String keyboardID = keyboardsList.get(i).get(KMManager.KMKey_KeyboardID);
              String kbVersion = keyboardsList.get(i).get(KMManager.KMKey_KeyboardVersion);
              String url = String.format("%s/%s/%s?version=%s&device=%s&languageidtype=bcp47",
                KMKeyboardDownloaderActivity.kKeymanApiBaseURL, languageID, keyboardID,  BuildConfig.VERSION_NAME, deviceType);
              JSONObject kbData = jsonParser.getJSONObjectFromUrl(url);
              JSONObject language = kbData.optJSONObject(KMKeyboardDownloaderActivity.KMKey_Language);
              JSONArray keyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);
              JSONObject keyboard = keyboards.getJSONObject(0);
              String newKbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
              String kbKey = String.format("%s_%s", languageID, keyboardID);
              if (keyboardVersions.get(kbKey) == null) {
                keyboardVersions.put(kbKey, newKbVersion);
              }

              if (FileUtils.compareVersions(newKbVersion, kbVersion) == FileUtils.VERSION_GREATER) {
                ret++;
              }
            }
          } catch (Exception e) {
            Log.e(TAG, "Failed to compare keyboard version. Error: " + e);
            keyboardVersions = null;
            ret = -1;
          }
        }

        return ret;
      }

      @Override
      protected void onProgressUpdate(Integer... progress) {
        // Do nothing
      }

      @Override
      protected void onPostExecute(Integer result) {
        if (progressDialog != null && progressDialog.isShowing()) {
          try {
            progressDialog.dismiss();
            progressDialog = null;
          } catch (Exception e) {
            progressDialog = null;
          }
        }

        if (result > 0) {
          failedUpdateCount = 0;
          updateCount = result;
          AlertDialog.Builder dialogBuilder = new AlertDialog.Builder(context);
          dialogBuilder.setTitle(context.getString(R.string.keyboard_updates_available));
          dialogBuilder.setMessage(context.getString(R.string.confirm_update));
          dialogBuilder.setPositiveButton(context.getString(R.string.label_update), new DialogInterface.OnClickListener() {
            public void onClick(DialogInterface dialog, int which) {
              // Update keyboards
              if (KMManager.hasConnection(context)) {
                int len = keyboardsList.size();
                for (int i = 0; i < len; i++) {
                  String pkgID = keyboardsList.get(i).get(KMManager.KMKey_PackageID);
                  String kbID = keyboardsList.get(i).get(KMManager.KMKey_KeyboardID);
                  String langID = keyboardsList.get(i).get(KMManager.KMKey_LanguageID);
                  String kbKey = String.format("%s_%s", langID, kbID);
                  String langName = keyboardsList.get(i).get(KMManager.KMKey_LanguageName);
                  String kbName = keyboardsList.get(i).get(KMManager.KMKey_KeyboardName);
                  String kbVersion = keyboardsList.get(i).get(KMManager.KMKey_KeyboardVersion);
                  String newKbVersion = keyboardVersions.get(kbKey);
                  if (newKbVersion != null) {
                    keyboardVersions.put(kbKey, newKbVersion);
                    if (FileUtils.compareVersions(newKbVersion, kbVersion) == FileUtils.VERSION_GREATER) {
                      if (updateProgress == null || !updateProgress.isShowing()) {
                        updateProgress = new ProgressDialog(context);
                        updateProgress.setMessage(context.getString(R.string.updating_keyboards));
                        updateProgress.setCancelable(false);
                        updateProgress.show();
                      }

                      Bundle bundle = new Bundle();
                      bundle.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, pkgID);
                      bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_ID, kbID);
                      bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, langID);
                      bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_NAME, kbName);
                      bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, langName);
                      bundle.putBoolean(KMKeyboardDownloaderActivity.ARG_IS_CUSTOM, false);
                      Intent intent = new Intent(context, KMKeyboardDownloaderActivity.class);
                      intent.putExtras(bundle);
                      context.startActivity(intent);
                    }
                  }
                }
              } else {
                Toast.makeText(context, "No internet connection", Toast.LENGTH_SHORT).show();
                checkingUpdates = false;
              }
            }
          });

          dialogBuilder.setNegativeButton(context.getString(R.string.label_later), new DialogInterface.OnClickListener() {
            public void onClick(DialogInterface dialog, int which) {
              lastUpdateCheck = Calendar.getInstance();
              checkingUpdates = false;
            }
          });

          AlertDialog dialog = dialogBuilder.create();
          if (!((AppCompatActivity) context).isFinishing()) {
            dialog.setOnCancelListener(new OnCancelListener() {
              @Override
              public void onCancel(DialogInterface dialog) {
                lastUpdateCheck = Calendar.getInstance();
                checkingUpdates = false;
              }
            });
            dialog.show();
          } else {
            checkingUpdates = false;
          }
        } else if (result == 0) {
          Toast.makeText(context, "All keyboards are up to date!", Toast.LENGTH_SHORT).show();
          lastUpdateCheck = Calendar.getInstance();
          SharedPreferences prefs = context.getSharedPreferences(context.getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
          SharedPreferences.Editor editor = prefs.edit();
          editor.putLong("lastUpdateCheck", lastUpdateCheck.getTime().getTime());
          editor.commit();
          checkingUpdates = false;
        } else {
          Toast.makeText(context, "Failed to access Keyman server!", Toast.LENGTH_SHORT).show();
          lastUpdateCheck = Calendar.getInstance();
          updateCheckFailed = true;
          checkingUpdates = false;
        }
      }
    }.execute();
  }

  @Override
  public void onKeyboardDownloadStarted(HashMap<String, String> keyboardInfo) {
    // Do nothing
  }

  @Override
  public void onKeyboardDownloadFinished(HashMap<String, String> keyboardInfo, int result) {
    if (result > 0) {
      didUpdate = true;
      String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
      String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
      String kbKey = String.format("%s_%s", languageID, keyboardID);
      int index = getKeyboardIndex(this, kbKey);
      if (index == -1) {
        // Add the downloaded keyboard if not found
        addKeyboard(this, keyboardInfo);
        index = getKeyboardIndex(this, kbKey);
      }
      keyboardsList.set(index, keyboardInfo);
      saveList(this, KMManager.KMFilename_KeyboardsList);
    } else if (result < 0) {
      failedUpdateCount++;
    }

    if (updateCount > 0) {
      updateCount--;
    }

    if (updateCount == 0 && updateProgress != null && updateProgress.isShowing()) {
      if (updateProgress != null && updateProgress.isShowing()) {
        try {
          updateProgress.dismiss();
          updateProgress = null;
        } catch (Exception e) {
          updateProgress = null;
        }
      }

      if (failedUpdateCount > 0) {
        Toast.makeText(this, "One or more keyboards failed to update!", Toast.LENGTH_SHORT).show();
        keyboardVersions = null;
        lastUpdateCheck = Calendar.getInstance();
        updateFailed = true;
        checkingUpdates = false;
      } else {
        Toast.makeText(this, "Keyboards successfully updated!", Toast.LENGTH_SHORT).show();
        lastUpdateCheck = Calendar.getInstance();
        SharedPreferences prefs = getSharedPreferences(getString(R.string.kma_prefs_name), Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.putLong("lastUpdateCheck", lastUpdateCheck.getTime().getTime());
        editor.commit();
        checkingUpdates = false;
      }
    }
    if (updateProgress != null && updateProgress.isShowing()) {
      updateProgress.dismiss();
    }
  }

  @Override
  public void onPackageInstalled(List<Map<String, String>> keyboardsInstalled) {
    // Do nothing
  }

  @Override
  public void onLexicalModelInstalled(List<Map<String, String>> lexicalModelsInstalled) {
    // Do nothing
  }
}
