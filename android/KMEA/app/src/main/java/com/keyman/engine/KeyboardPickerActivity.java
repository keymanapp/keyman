/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.keyman.engine;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import com.keyman.engine.data.CloudRepository;
import com.keyman.engine.data.Dataset;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.data.LexicalModel;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.MapCompat;

import android.content.ComponentName;
import android.content.Context;
import android.content.pm.ApplicationInfo;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager;
import android.graphics.Typeface;
import android.inputmethodservice.InputMethodService;
import android.os.Build;
import android.os.Bundle;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;
import android.view.inputmethod.InputMethodInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.BaseAdapter;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.PopupMenu;
import android.widget.SimpleAdapter;
import android.widget.TextView;
import android.widget.Toast;

import androidx.appcompat.widget.Toolbar;

public final class KeyboardPickerActivity extends BaseActivity {
  private boolean hasDeleted = false;

  //TODO: view instances should not be static
  private static Toolbar toolbar = null;
  private static ListView listView = null;
  private static ListView imeListView = null;
  private static final String titleKey = "title";
  private static final String subtitleKey = "subtitle";
  private static final String iconKey = "icon";

  private static ArrayList<HashMap<String, String>> imeList = null;
  private static KMKeyboardPickerAdapter listAdapter = null;
  private static ListAdapter imeListAdapter = null;

  public static final String  KMKEY_INTERNAL_NEW_KEYBOARD = "_internal_new_keyboard_";

  // List of  installed lexical models
  private static ArrayList<HashMap<String, String>> lexicalModelsList = null;
  private static Dataset storageDataset = null;

  //private static boolean didUpdate = false;
  private static int selectedIndex = 0;
  private static final String TAG = "KeyboardPickerActivity";

  public static int selectedIndex() {
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
    int actionBarHeight = toolbar.getLayoutParams().height;
    getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    getSupportActionBar().setDisplayShowHomeEnabled(true);
    getSupportActionBar().setDisplayShowTitleEnabled(false);
    TextView textView = (TextView) findViewById(R.id.bar_title);
    textView.setText(getResources().getQuantityString(R.plurals.title_keyboards, 2));

    listView = (ListView) findViewById(R.id.listView);

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
        // As long as more than 1 keyboard installed, display uninstall keyboard
        if (KeyboardController.getInstance().get().size() > 1 && canRemoveKeyboard) {
          PopupMenu popup = new PopupMenu(context, view);
          popup.getMenuInflater().inflate(R.menu.popup, popup.getMenu());
          popup.setOnMenuItemClickListener(new PopupMenu.OnMenuItemClickListener() {
            public boolean onMenuItemClick(MenuItem item) {
              if (item.getItemId() == R.id.popup_delete) {
                deleteKeyboard(context, position);
                KeyboardPickerActivity.this.hasDeleted = true;
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

    int curKbPos = KeyboardController.getInstance().getKeyboardIndex(KMKeyboard.currentKeyboard());
    setSelection(curKbPos);

    KMKeyboard.addOnKeyboardEventListener(new KeyboardEventHandler.OnKeyboardEventListener() {
      @Override
      public void onKeyboardLoaded(KMManager.KeyboardType keyboardType) {

      }

      @Override
      public void onKeyboardChanged(String newKeyboard) {
          int _index = KeyboardController.getInstance().getKeyboardIndex(newKeyboard);
          if(_index>=0)
          {
            Keyboard _keyboard = KeyboardController.getInstance().getKeyboardInfo(_index);
            if(_keyboard==null) {
              return;
            }
            if(!_keyboard.getNewKeyboard()) {
              return;
            }

            if (_keyboard.getNewKeyboard()) {
              _keyboard.setNewKeyboard(false);
              // Update entry
              KeyboardController.getInstance().add(_keyboard);
              KeyboardController.getInstance().save(context);
            }
            notifyKeyboardsUpdate(context);
          }
      }

      @Override
      public void onKeyboardShown() {

      }

      @Override
      public void onKeyboardDismissed() {

      }
    });

    Bundle bundle = getIntent().getExtras();
    if (bundle != null) {
      if (bundle.getBoolean(KMManager.KMKey_DisplayKeyboardSwitcher)) {
        // Create list of other Enabled IME's (excluding self)
        View view = getLayoutInflater().inflate(R.layout.ime_list_layout, null);
        LinearLayout imeLinearLayout = (LinearLayout) view.findViewById(R.id.ime_linear_layout);
        imeListView = (ListView) imeLinearLayout.findViewById(R.id.listView);
        imeList = getIMEList(context);
        if (imeListView != null && imeList != null && imeList.size() > 0) {
          // "Other Input Methods" titlebar
          TextView imeTextView = (TextView) view.findViewById(R.id.other_ime_title);
          imeTextView.setText(getResources().getQuantityString(R.plurals.title_other_input_methods, imeList.size()));

          String[] from = new String[]{titleKey, subtitleKey, iconKey};
          int[] to = new int[]{R.id.text1, R.id.text2, R.id.image1};
          // Using list_row_layout5 to hide displaying subtitle (IME ID)
          imeListAdapter = new SimpleAdapter(this, imeList, R.layout.list_row_layout5, from, to);
          imeListView.setAdapter(imeListAdapter);
          // Rescale IME listview so entire list is visible (wrap_content not working)
          imeListView.getLayoutParams().height = ((int) context.getResources().getDimension(R.dimen.other_ime_row_height)) *
            imeListAdapter.getCount();
          imeListView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
              HashMap<String, String> hashMap = (HashMap<String, String>) parent.getItemAtPosition(position);
              if (hashMap == null) {
                return;
              }

              String imeID = MapCompat.getOrDefault(hashMap, subtitleKey, "");
              InputMethodService ims = KMManager.getInputMethodService();
              if (!imeID.equals("") && ims != null) {
                ims.switchInputMethod(imeID);
              }
              finish();
            }
          });

          // Append the IME list to the overall listView
          listView.addFooterView(imeLinearLayout);
        }
      }
    }
  }

  @Override
  protected void onResume() {
    super.onResume();
    BaseAdapter adapter = (BaseAdapter) listAdapter;
    if(listAdapter != null) {
      adapter.notifyDataSetChanged();
    }

    int curKbPos = KeyboardController.getInstance().getKeyboardIndex(KMKeyboard.currentKeyboard());
    setSelection(curKbPos);

    imeList = getIMEList(this);
    BaseAdapter imeAdapter = (BaseAdapter)imeListAdapter;
    if (imeListAdapter != null) {
      imeAdapter.notifyDataSetChanged();
    }

    if (!shouldCheckKeyboardUpdates)
      return;
  }

  @Override
  protected void onPause() {
    super.onPause();

    if (this.hasDeleted) {
      this.hasDeleted = false;

      if (KMManager.InAppKeyboard != null) {
        KMManager.InAppKeyboard.loadKeyboard();
      }

      if (KMManager.SystemKeyboard != null) {
        KMManager.SystemKeyboard.loadKeyboard();
      }
    }
  }

  @Override
  public boolean onSupportNavigateUp() {
    onBackPressed();
    return true;
  }

  @Override
  public void onBackPressed() {
    super.onBackPressed();
    finish();
  }

  private static boolean saveList(Context context, String listName) {
    boolean result;
    List<Keyboard> keyboardsList = KeyboardController.getInstance().get();
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
      KMLog.LogException(TAG, "Failed to save " + listName + ". Error: ", e);
      result = false;
    }

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
    if (listView != null) {
      listView.setItemChecked(position, true);
      listView.setSelection(position);
    }
    selectedIndex = position;
  }

  /**
   * switch to the given keyboard.
   * @param position the keyboard index in list
   * @param aPrepareOnly prepare switch, it is executed on keyboard reload
   */
  private static void switchKeyboard(int position) {
    setSelection(position);
    int size = KeyboardController.getInstance().get().size();
    int listPosition = (position >= size) ? size-1 : position;
    Keyboard kbInfo = KeyboardController.getInstance().getKeyboardInfo(listPosition);
    String pkgId = kbInfo.getPackageID();
    String kbId = kbInfo.getKeyboardID();
    String langId = kbInfo.getLanguageID();
    String kbName = kbInfo.getKeyboardName();
    KMManager.setKeyboard(kbInfo);
  }

  protected static boolean addKeyboard(Context context, Keyboard keyboardInfo) {
    boolean result = false;
    List<Keyboard> keyboardsList = KeyboardController.getInstance().get();

    if (keyboardInfo != null) {
      String languageID = keyboardInfo.getLanguageID();
      if (CloudRepository.shared.getAssociatedLexicalModel(context, languageID) == null) {
        // Only invalidate the lexical cache if there's no associated lexical model
        CloudRepository.shared.invalidateLexicalModelCache(context, true);
      }

      keyboardInfo.setNewKeyboard(true);
      KeyboardController.getInstance().add(keyboardInfo);
      // Check if "other" keyboards of the same packageID and keyboardID also need to update version
      // Don't use forEach because we might be updating entries
      for (int i=0; i<KeyboardController.getInstance().get().size(); i++) {
        Keyboard otherKeyboard = KeyboardController.getInstance().getKeyboardInfo(i);
        if (otherKeyboard.getPackageID().equals(keyboardInfo.getPackageID())
            && otherKeyboard.getKeyboardID().equals(keyboardInfo.getKeyboardID())
            && !otherKeyboard.getLanguageID().equals(keyboardInfo.getLanguageID())) {
          otherKeyboard.setVersion(keyboardInfo.getVersion());
          KeyboardController.getInstance().set(i, otherKeyboard);
        }
      }
      result = KeyboardController.getInstance().save(context);
      if (!result) {
        KMLog.LogError(TAG, "addKeyboard failed to save");
      }
    }
    notifyKeyboardsUpdate(context);

    return result;
  }

  public static boolean addLexicalModel(Context context, HashMap<String, String> lexicalModelInfo) {
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
        String lmKey = KMString.format("%s_%s_%s", pkgID, langID, modelID);
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

          // Invalidate cache to rebuild the list (don't delete cache file since we just updated it)
          CloudRepository.shared.invalidateLexicalModelCache(context, false);
        }
      }
    }

    notifyLexicalModelsUpdate(context);

    return result;
  }

  protected static boolean removeKeyboard(Context context, int position) {
    boolean result = false;

    KeyboardController.getInstance().remove(position);
    result = KeyboardController.getInstance().save(context);

    notifyKeyboardsUpdate(context);

    return result;
  }

  protected static void deleteKeyboard(Context context, int position) {
    int curKbPos = KeyboardController.getInstance().getKeyboardIndex(KMKeyboard.currentKeyboard());
    boolean result = removeKeyboard(context, position);

    if (result) {
      Toast.makeText(context, context.getString(R.string.keyboard_deleted_toast), Toast.LENGTH_SHORT).show();
      BaseAdapter adapter = (BaseAdapter) listAdapter;
      if(adapter != null) {
        adapter.notifyDataSetChanged();
      }
      if (position == curKbPos) {
        switchKeyboard(0);
      } else if(listView != null) { // A bit of a hack, since LanguageSettingsActivity calls this method too.
        curKbPos = KeyboardController.getInstance().getKeyboardIndex(KMKeyboard.currentKeyboard());
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
        Toast.makeText(context, context.getString(R.string.model_deleted), Toast.LENGTH_SHORT).show();
      }
      KMManager.deregisterLexicalModel(modelID);
    }

    notifyLexicalModelsUpdate(context);
  }

  // Gets a raw list of installed lexical models.
  @SuppressWarnings("unchecked")
  private static ArrayList<HashMap<String, String>> getList(Context context, String filename) {
    ArrayList<HashMap<String, String>> list = new ArrayList<HashMap<String, String>>();
    File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), filename);
    if (file.exists()) {
      try {
        ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
        list = (ArrayList<HashMap<String, String>>) inputStream.readObject();
        inputStream.close();
      } catch (Exception e) {
        KMLog.LogException(TAG, "Failed to read " + filename + ". Error: ", e);
      }
    }

    return list;
  }

  // Get the list of IME's excluding self
  private static ArrayList<HashMap<String, String>> getIMEList(Context context) {
    String selfPackageName = context.getApplicationContext().getPackageName();
    ArrayList<HashMap<String, String>> list = new ArrayList<HashMap<String, String>>();
    InputMethodManager imManager = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
    List<InputMethodInfo> imeList = imManager.getEnabledInputMethodList();
    PackageManager packageManager = context.getPackageManager();
    for (InputMethodInfo imeInfo : imeList) {
      String id = imeInfo.getId();
      if (!id.startsWith(selfPackageName)) {
        // Attempt to get a readable name
        // Reference: https://stackoverflow.com/questions/62512501/get-human-readable-name-of-default-keyboard-not-package-name
        ComponentName componentName = ComponentName.unflattenFromString(id);
        if (componentName != null) {
          String packageName = componentName.getPackageName();
          String imeName = "";
          try {
            ApplicationInfo info = packageManager.getApplicationInfo(packageName, PackageManager.GET_META_DATA);
            imeName = (String)packageManager.getApplicationLabel(info);
          } catch (PackageManager.NameNotFoundException e) {
            // For Android 11+, this exception is thrown because we don't have QUERY_ALL_PACKAGES permission.
            // We'll just display the package name instead.
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.R) {
              KMLog.LogException(TAG, "Name not found", e);
            }
            imeName = packageName;
          }

          if (!imeName.isEmpty()) {
            HashMap<String, String> hashMap = new HashMap<String, String>();
            hashMap.put(titleKey, imeName);
            hashMap.put(subtitleKey, id);
            list.add(hashMap);
          }
        }
      }
    }
    return list;
  }

  public static Dataset getInstalledDataset(Context context) {
    if(storageDataset != null) {
      return storageDataset;
    }

    List<Keyboard> kbdsList = getKeyboardsList(context);
    List<HashMap<String, String>> lexMapList = getLexicalModelsList(context);
    if(lexMapList == null) {
      lexMapList = new ArrayList<>(0);
    }
    List<LexicalModel> lexList = new ArrayList<>(lexMapList.size());

    for(HashMap<String, String> lmMap: lexMapList) {
      LexicalModel m = new LexicalModel(
        lmMap.get(KMManager.KMKey_PackageID),
        lmMap.get(KMManager.KMKey_LexicalModelID),
        lmMap.get(KMManager.KMKey_LexicalModelName),
        lmMap.get(KMManager.KMKey_LanguageID),
        lmMap.get(KMManager.KMKey_LanguageName),
        MapCompat.getOrDefault(lmMap, KMManager.KMKey_LexicalModelVersion, "1.0"),
        MapCompat.getOrDefault(lmMap, KMManager.KMKey_CustomHelpLink, ""),
        MapCompat.getOrDefault(lmMap, KMManager.KMKey_KMPLink, ""));
      lexList.add(m);
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

    List<Keyboard> kbdList = KeyboardController.getInstance().get();
    storage.keyboards.addAll(kbdList);
    storage.keyboards.notifyDataSetChanged();
  }

  protected static void notifyLexicalModelsUpdate(Context context) {
    Dataset storage = getInstalledDataset(context);
    storage.lexicalModels.setNotifyOnChange(false);
    storage.lexicalModels.clear();

    List<HashMap<String, String>> mapList = getLexicalModelsList(context);
    List<LexicalModel> lexList = new ArrayList<>(mapList.size());
    for(HashMap<String, String> lmMap: mapList) {
      LexicalModel m = new LexicalModel(
        lmMap.get(KMManager.KMKey_PackageID),
        lmMap.get(KMManager.KMKey_LexicalModelID),
        lmMap.get(KMManager.KMKey_LexicalModelName),
        lmMap.get(KMManager.KMKey_LanguageID),
        lmMap.get(KMManager.KMKey_LanguageName),
        MapCompat.getOrDefault(lmMap, KMManager.KMKey_LexicalModelVersion, "1.0"),
        MapCompat.getOrDefault(lmMap, KMManager.KMKey_CustomHelpLink, ""),
        MapCompat.getOrDefault(lmMap, KMManager.KMKey_KMPLink, ""));
      lexList.add(m);
    }
    storage.lexicalModels.addAll(lexList);
    storage.lexicalModels.notifyDataSetChanged();
  }

  protected static List<Keyboard> getKeyboardsList(Context context) {
    return KeyboardController.getInstance().get();
  }

  protected static ArrayList<HashMap<String, String>> getLexicalModelsList(Context context) {
    return getList(context, KMManager.KMFilename_LexicalModelsList);
  }

  public static boolean containsLexicalModel(Context context, String lexicalModelKey) {
    if (lexicalModelsList == null) {
      lexicalModelsList = getLexicalModelsList(context);
    }

    if (lexicalModelsList == null) {
      return false;
    }

    for(HashMap<String, String> lmInfo : lexicalModelsList) {
      String key = KMString.format("%s_%s_%s", lmInfo.get(KMManager.KMKey_PackageID),
        lmInfo.get(KMManager.KMKey_LanguageID), lmInfo.get(KMManager.KMKey_LexicalModelID));
      if (lexicalModelKey.equalsIgnoreCase(key)) {
        return true;
      }
    }

    return false;
  }

  /**
   * Get the index of a lexical model key in the list of installed lexical models.
   * @param context
   * @param lexicalModelKey - key of "{package ID}_{language ID}_{lexical model ID}"
   * @return Index >= 0 if the lexical model key exists. Otherwise -1
   */
  public static int getLexicalModelIndex(Context context, String lexicalModelKey) {
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
        String lmKey = KMString.format("%s_%s_%s", pkgId, langId, lmId);
        if (lmKey.equals(lexicalModelKey)) {
          index = i;
          break;
        }
      }
    }

    return index;
  }

  public static HashMap<String, String> getLexicalModelInfo(Context context,int index) {
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

  // This is deprecated in Keyman 14.0
  protected static void showLanguageList(Context context) {
    return;
  }

  public static void handleDownloadedKeyboard(Context context, HashMap<String, String> keyboardInfo) {
    boolean isNewKeyboard = true;
    Keyboard k = new Keyboard(
      keyboardInfo.get(KMManager.KMKey_PackageID),
      keyboardInfo.get(KMManager.KMKey_KeyboardID),
      keyboardInfo.get(KMManager.KMKey_KeyboardName),
      keyboardInfo.get(KMManager.KMKey_LanguageID),
      keyboardInfo.get(KMManager.KMKey_LanguageName),
      keyboardInfo.get(KMManager.KMKey_KeyboardVersion),
      keyboardInfo.get(KMManager.KMKey_CustomHelpLink),
      keyboardInfo.get(KMManager.KMKey_KMPLink),
      isNewKeyboard,
      keyboardInfo.get(KMManager.KMKey_Font),
      keyboardInfo.get(KMManager.KMKey_OskFont));
    KeyboardController.getInstance().add(k);
    KeyboardController.getInstance().save(context);
  }
}
