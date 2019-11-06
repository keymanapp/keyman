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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.tavultesoft.kmea.data.CloudDataJsonUtil;
import com.tavultesoft.kmea.data.CloudRepository;
import com.tavultesoft.kmea.data.Dataset;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.LexicalModel;
import com.tavultesoft.kmea.util.MapCompat;

import androidx.appcompat.app.AppCompatActivity;
import androidx.appcompat.app.AlertDialog;
import android.app.ProgressDialog;
import android.content.Context;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
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

public final class KeyboardPickerActivity extends AppCompatActivity {

  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static Button closeButton = null;
  private static KMKeyboardPickerAdapter listAdapter = null;

  // Lists of installed keyboards and installed lexical models
  private static ArrayList<HashMap<String, String>> keyboardsList = null;
  private static ArrayList<HashMap<String, String>> lexicalModelsList = null;
  private static Dataset storageDataset = null;

  //private static boolean didUpdate = false;
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

    // TODO:  Use a persistently-loaded version of the installed dataset.
    listAdapter = new KMKeyboardPickerAdapter(context, getInstalledDataset(context).keyboards);
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
                deleteKeyboard(context, position);
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

    int curKbPos = getCurrentKeyboardIndex();
    setSelection(curKbPos);
  }

  @Override
  protected void onResume() {
    super.onResume();
    BaseAdapter adapter = (BaseAdapter) listAdapter;
    if(listAdapter != null) {
      adapter.notifyDataSetChanged();
    }

    int curKbPos = getCurrentKeyboardIndex();
    setSelection(curKbPos);
    if (!shouldCheckKeyboardUpdates)
      return;
  }

  @Override
  protected void onPause() {
    super.onPause();

    if (KMManager.InAppKeyboard != null) {
      KMManager.InAppKeyboard.loadKeyboard();
    }
    if (KMManager.SystemKeyboard != null) {
      KMManager.SystemKeyboard.loadKeyboard();
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
      String customKeyboard = MapCompat.getOrDefault(kbInfo, KMManager.KMKey_CustomKeyboard, "N");
      if (customKeyboard.equalsIgnoreCase("Y")) {
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

  /**
   * Save the list of installed keyboards
   * @param context
   * @param list
   * @return boolean - Status if the keyboard list was successfully saved
   */
  protected static boolean updateKeyboardsList(Context context, ArrayList<HashMap<String, String>> list) {
    boolean result;
    keyboardsList = list;
    result = saveList(context, KMManager.KMFilename_KeyboardsList);
    notifyKeyboardsUpdate(context);
    return result;
  }

  /**
   * Save the list of installed lexical models
   * @param context
   * @param list
   * @return boolean - Status if the lexical models list was successfully saved
   */
  protected static boolean updateLexicalModelsList(Context context, ArrayList<HashMap<String, String>> list) {
    boolean result;
    lexicalModelsList = list;
    result = saveList(context, KMManager.KMFilename_LexicalModelsList);
    notifyLexicalModelsUpdate(context);
    return result;
  }

  private static void setSelection(int position) {
    listView.setItemChecked(position, true);
    listView.setSelection(position);
    selectedIndex = position;
  }

  private static void switchKeyboard(int position) {
    setSelection(position);
    int listPosition = (position >= keyboardsList.size()) ? keyboardsList.size()-1 : position;
    HashMap<String, String> kbInfo = keyboardsList.get(listPosition);
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

      // TODO:  Possible optimization - do we have anything with this language code already?
      //        Only invalidate the lexical cache if not.
      CloudRepository.shared.invalidateLexicalModelCache(context);

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

    notifyKeyboardsUpdate(context);

    return result;
  }

  protected static boolean addLexicalModel(Context context, HashMap<String, String> lexicalModelInfo) {
    boolean result = false;

    if (lexicalModelsList == null) {
      // First, try loading our existing (file-backed) model list.
      lexicalModelsList = getLexicalModelsList(context);
      // If there is no existing list, time to build one from scratch.
      if(lexicalModelsList == null) {
        lexicalModelsList = new ArrayList<>();
      }
    }

    if (lexicalModelInfo != null) {
      String pkgID = lexicalModelInfo.get(KMManager.KMKey_PackageID);
      String modelID = lexicalModelInfo.get(KMManager.KMKey_LexicalModelID);
      String langID = lexicalModelInfo.get(KMManager.KMKey_LanguageID);

      if (pkgID != null && modelID != null && langID != null) {
        String lmKey = String.format("%s_%s_%s", pkgID, langID, modelID);
        if (lmKey.length() >= 5) {
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

    notifyLexicalModelsUpdate(context);

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

    notifyKeyboardsUpdate(context);

    return result;
  }

  protected static void deleteKeyboard(Context context, int position) {
    int curKbPos = getCurrentKeyboardIndex();
    boolean result = removeKeyboard(context, position);

    if (result) {
      Toast.makeText(context, "Keyboard deleted", Toast.LENGTH_SHORT).show();
      BaseAdapter adapter = (BaseAdapter) listAdapter;
      if(adapter != null) {
        adapter.notifyDataSetChanged();
      }
      if (position == curKbPos && listView != null) {
        switchKeyboard(0);
      } else if(listView != null) { // A bit of a hack, since LanguageSettingsActivity calls this method too.
        curKbPos = getCurrentKeyboardIndex();
        setSelection(curKbPos);
      }
    }

    notifyKeyboardsUpdate(context);
  }

  /**
   * getModelIDFromPosition - Get the lexical model ID at a given position
   * @param context
   * @param position - int position of the lexical model
   * @return String - model ID. Blank if invalid position
   */
  protected static String getModelIDFromPosition(Context context, int position) {
    if (lexicalModelsList == null) {
      lexicalModelsList = getLexicalModelsList(context);
    }

    String modelID = "";
    if (lexicalModelsList != null && position >= 0 && position < lexicalModelsList.size()) {
      HashMap<String, String> lexicalModelInfo = lexicalModelsList.get(position);
      modelID = lexicalModelInfo.get(KMManager.KMKey_LexicalModelID);
    }

    return modelID;
  }

  /**
   * removeLexicalModel - Remove lexical model at a given position from the installed lexical models list
   * @param context
   * @param position - int position of the lexical model to remove
   * @return boolean - result of the model could be removed and list saved
   */
  protected static boolean removeLexicalModel(Context context, int position) {
    boolean result = false;

    if (lexicalModelsList == null) {
      lexicalModelsList = getLexicalModelsList(context);
    }

    if (lexicalModelsList != null && position >= 0 && position < lexicalModelsList.size()) {
      lexicalModelsList.remove(position);
      result = saveList(context, KMManager.KMFilename_LexicalModelsList);
    }

    notifyLexicalModelsUpdate(context);

    return result;
  }


  /**
   * deleteLexicalModel - Remove lexical model from the installed list
   * and deregister the model with KMW
   * @param context
   * @param position - int position in the models list
   */
  protected static void deleteLexicalModel(Context context, int position, boolean silenceNotification) {
    String modelID = getModelIDFromPosition(context, position);
    boolean result = removeLexicalModel(context, position);

    if (result) {
      if(!silenceNotification) {
        Toast.makeText(context, "" + "Model deleted", Toast.LENGTH_SHORT).show();
      }
      KMManager.deregisterLexicalModel(modelID);
    }

    notifyLexicalModelsUpdate(context);
  }

  // Gets a raw list of installed lexical models.
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

  public static Dataset getInstalledDataset(Context context) {
    if(storageDataset != null) {
      return storageDataset;
    }

    List<? extends Map<String, String>> kbdMapList = getKeyboardsList(context);
    List<Keyboard> kbdsList = new ArrayList<>(kbdMapList.size());

    for(Map<String, String> map: kbdMapList) {
      kbdsList.add(new Keyboard(map));
    }

    List<? extends Map<String, String>> lexMapList = getLexicalModelsList(context);
    if(lexMapList == null) {
      lexMapList = new ArrayList<>(0);
    }
    List<LexicalModel> lexList = new ArrayList<>(lexMapList.size());

    for(Map<String, String> map: lexMapList) {
      lexList.add(new LexicalModel(map));
    }

    storageDataset = new Dataset(context);
    storageDataset.keyboards.addAll(kbdsList);
    storageDataset.lexicalModels.addAll(lexList);

    return storageDataset;
  }

  // While the two following methods aren't exactly ideal, they should be enough to get the job done
  // for 12.0 before a more complete refactor of this class is done.
  protected static void notifyKeyboardsUpdate(Context context) {
    Dataset storage = getInstalledDataset(context);
    storage.keyboards.setNotifyOnChange(false);
    storage.keyboards.clear();

    List<? extends Map<String, String>> mapList = getKeyboardsList(context);
    List<Keyboard> kbdList = new ArrayList<>(mapList.size());
    for(Map<String, String> map: mapList) {
      kbdList.add(new Keyboard(map));
    }
    storage.keyboards.addAll(kbdList);
    storage.keyboards.notifyDataSetChanged();
  }

  protected static void notifyLexicalModelsUpdate(Context context) {
    Dataset storage = getInstalledDataset(context);
    storage.lexicalModels.setNotifyOnChange(false);
    storage.lexicalModels.clear();

    List<? extends Map<String, String>> mapList = getLexicalModelsList(context);
    List<LexicalModel> lexList = new ArrayList<>(mapList.size());
    for(Map<String, String> map: mapList) {
      lexList.add(new LexicalModel(map));
    }
    storage.lexicalModels.addAll(lexList);

    storage.lexicalModels.notifyDataSetChanged();
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

  public static boolean containsLexicalModel(Context context, String lexicalModelKey) {
    if (lexicalModelsList == null) {
      lexicalModelsList = getLexicalModelsList(context);
    }

    if (lexicalModelsList == null) {
      return false;
    }

    for(HashMap<String, String> lmInfo : lexicalModelsList) {
      String key = String.format("%s_%s_%s", lmInfo.get(KMManager.KMKey_PackageID),
        lmInfo.get(KMManager.KMKey_LanguageID), lmInfo.get(KMManager.KMKey_LexicalModelID));
      if (lexicalModelKey.equalsIgnoreCase(key)) {
        return true;
      }
    }

    return false;
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
   * Get the list of associated keyboards for a given language ID
   * @param langId
   * @return ArrayList of keyboard
   */
  public static ArrayList<HashMap<String, String>> getAssociatedKeyboards(String langId) {
    if (keyboardsList != null) {
      ArrayList<HashMap<String, String>> associatedKeyboards = new ArrayList<HashMap<String, String>>();
      for (HashMap<String, String> keyboardInfo: keyboardsList) {
        if (keyboardInfo.get(KMManager.KMKey_LanguageID).equalsIgnoreCase(langId)) {
          keyboardInfo.put(KMManager.KMKey_Icon, String.valueOf(R.drawable.ic_arrow_forward));
          keyboardInfo.put("isEnabled", "true");
          associatedKeyboards.add(keyboardInfo);
        }
      }
      return associatedKeyboards;
    }

    return null;
  }

  /**
   * Get the index of a lexical model key in the list of installed lexical models.
   * @param context
   * @param lexicalModelKey - key of "{package ID}_{language ID}_{lexical model ID}"
   * @return Index >= 0 if the lexical model key exists. Otherwise -1
   */
  protected static int getLexicalModelIndex(Context context, String lexicalModelKey) {
    int index = -1;

    if (lexicalModelsList == null) {
      lexicalModelsList = getLexicalModelsList(context);
    }

    if (lexicalModelsList != null) {
      int length = lexicalModelsList.size();
      for (int i=0; i < length; i++) {
        HashMap<String, String> lmInfo = lexicalModelsList.get(i);
        String pkgId = lmInfo.get(KMManager.KMKey_PackageID);
        String langId = lmInfo.get(KMManager.KMKey_LanguageID);
        String lmId = lmInfo.get(KMManager.KMKey_LexicalModelID);
        String lmKey = String.format("%s_%s_%s", pkgId, langId, lmId);
        if (lmKey.equals(lexicalModelKey)) {
          index = i;
          break;
        }
      }
    }

    return index;
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

  protected static HashMap<String, String> getLexicalModelInfo(Context context,int index) {
    if (index < 0) {
      return null;
    }

    if (lexicalModelsList == null) {
      lexicalModelsList = getLexicalModelsList(context);
    }

    if (lexicalModelsList != null && index < lexicalModelsList.size()) {
      HashMap<String, String> lmInfo = lexicalModelsList.get(index);
      return lmInfo;
    }

    return null;
  }

  protected static void showLanguageList(Context context) {
    if (KMManager.hasConnection(context) || CloudDataJsonUtil.getKeyboardCacheFile(context).exists()) {
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

  static void handleDownloadedKeyboard(Context context, HashMap<String, String> keyboardInfo) {
    String keyboardID = keyboardInfo.get(KMManager.KMKey_KeyboardID);
    String languageID = keyboardInfo.get(KMManager.KMKey_LanguageID);
    String kbKey = String.format("%s_%s", languageID, keyboardID);
    int index = getKeyboardIndex(context, kbKey);
    if (index == -1) {
      // Add the downloaded keyboard if not found
      addKeyboard(context, keyboardInfo);
      index = getKeyboardIndex(context, kbKey);
    }
    keyboardsList.set(index, keyboardInfo);
    saveList(context, KMManager.KMFilename_KeyboardsList);
  }
}
