/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.keyman.engine.data;

import android.content.Context;
import android.util.Log;

import com.keyman.engine.JSONParser;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.util.BCP47;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMString;
import com.keyman.engine.util.MapCompat;
import com.keyman.engine.KMManager;
import com.keyman.engine.util.KMLog;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class KeyboardController {
  public static final String TAG = "KeyboardController";
  public static final String KMFilename_Installed_KeyboardsList = "keyboards_list.json";
  public static final int INDEX_NOT_FOUND = -1;

  private static KeyboardController instance;

  /**
   * @return get or create shared singleton instance.
   */
  public static KeyboardController getInstance() {
    if (instance == null) {
      instance = new KeyboardController();
    }
    return instance;
  }

  private boolean isInitialized = false;
  private List<Keyboard> list;

  public synchronized void initialize(Context context) {
    if (isInitialized) {
      Log.w(TAG, "initialize called multiple times");
      return;
    }

    File keyboards_dat = new File(context.getDir("userdata", Context.MODE_PRIVATE),
      KMManager.KMFilename_KeyboardsList);
    File keyboards_json = new File(context.getDir("userdata", Context.MODE_PRIVATE),
      KMFilename_Installed_KeyboardsList);
    if (list == null) {
      list = new ArrayList<Keyboard>();
      if (keyboards_dat.exists() && !keyboards_json.exists()) {
        try {
          // Migrate installed_keyboards.dat to keyboards_list.json
          ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(keyboards_dat));
          ArrayList<HashMap<String, String>> dat_list = (ArrayList<HashMap<String, String>>) inputStream.readObject();
          inputStream.close();
          list = KMManager.updateOldKeyboardsList(context, dat_list);
        } catch (Exception e) {
          KMLog.LogException(TAG, "Exception migrating installed_keyboards.dat", e);
          list.add(Keyboard.getDefaultKeyboard(context));
        }
      } else if (keyboards_json.exists()) {
        JSONArray json_list = null;
        try {
          // Get installed keyboards from keyboards_list.json
          JSONParser jsonParser = new JSONParser();
          json_list = jsonParser.getJSONObjectFromFile(keyboards_json, JSONArray.class);
          if (json_list != null) {
            // Can't foreach JSONArray
            for (int i=0; i<json_list.length(); i++) {
              JSONObject o = json_list.getJSONObject(i);
              if (o != null) {
                Keyboard k = new Keyboard(o);
                list.add(k);
              }
            }
          } else {
            KMLog.LogError(TAG, KMFilename_Installed_KeyboardsList + " is null");
          }
        } catch (Exception e) {
          KMLog.LogExceptionWithData(TAG, "Exception reading " + KMFilename_Installed_KeyboardsList,
            KMFilename_Installed_KeyboardsList, json_list, e);
          list.add(Keyboard.getDefaultKeyboard(context));
        }
      } else {
        // No installed keyboards lists
        // 3rd-party OEM may not have sil_euro_latin, so don't assign a default keyboard
        //list.add(Keyboard.getDefaultKeyboard(context));
        Log.w(TAG, "initialize with no default keyboard");
      }

      // We'd prefer not to overwrite a file if it exists
      if (!keyboards_json.exists() && list != null && list.size() > 0) {
        save(context);

        // Now we can delete legacy keyboards list
        if (keyboards_dat.exists()) {
          keyboards_dat.delete();
        }
      }
    }
    isInitialized = true;
  }

  /**
   * Return the installed keyboards list
   * @return
   */
  public List<Keyboard> get() {
    if (!isInitialized) {
      KMLog.LogError(TAG, "get while KeyboardController() not initialized");
      return null;
    }
    synchronized (list) {
      return list;
    }
  }

  /**
   * Returns the list of installed keyboards with unique packageID/keyboardID
   */
  public List<Keyboard> getInstalledPackagesList() {
    if (!isInitialized) {
      KMLog.LogError(TAG, "getInstalledPackagesList while KeyboardController() not initialized");
      return null;
    }
    synchronized (list) {
      List<Keyboard> packagesList = new ArrayList<Keyboard>();
      // Iterate through the installed keyboards list to find unique packageID/keyboardID
      for (int i=0; i<list.size(); i++) {
        Keyboard k = list.get(i);
        String pkgID = k.getPackageID();
        String keyboardID = k.getKeyboardID();
        // Ignore "cloud" keyboards"
        if (pkgID.equals(KMManager.KMDefault_UndefinedPackageID)) {
          continue;
        }

        // If we search getKeyboardIndex with blank languageID, it will give us the first
        // unique pkgID/keyboardID keyboard in the list
        int firstMatchingIndex = getKeyboardIndex(pkgID, keyboardID, "");
        if (firstMatchingIndex != KeyboardController.INDEX_NOT_FOUND && (firstMatchingIndex == i)) {
          packagesList.add(k);
        }
      }
      return packagesList;
    }
  }

  /**
   * Return the keyboard info at index
   * @param index - int
   * @return Keyboard
   */
  public Keyboard getKeyboardInfo(int index) {
    if (!isInitialized) {
      KMLog.LogError(TAG, "getKeyboardInfo while KeyboardController() not initialized");
      return null;
    } else if (index < 0) {
      // Don't need to report to Sentry
      return null;
    }

    synchronized (list) {
      if (list != null && index < list.size()) {
        Keyboard k = list.get(index);
        return k;
      }
    }

    Log.w(TAG, "getKeyboardInfo failed with index " + index);
    return null;
  }

  /**
   * Given a key, return the index of the matching keyboard.
   * If no match, returns INDEX_NOT_FOUND
   * @param key - String of the key to find
   * @return int - Index of the matching keyboard
   */
  public int getKeyboardIndex(String key) {
    int index = INDEX_NOT_FOUND;
    if (!isInitialized || list == null) {
      KMLog.LogError(TAG, "getKeyboardIndex while KeyboardController() not initialized");
      return index;
    }
    if (key == null || key.isEmpty()) {
      KMLog.LogError(TAG, "getKeyboardIndex while key is null");
      return index;
    }

    synchronized (list) {
      for (int i=0; i<list.size(); i++) {
        Keyboard k = list.get(i);
        if (k.getKey().equalsIgnoreCase(key)) {
          return i;
        }
      }
    }

    // We'll only log if key isn't for fallback keyboard
    if (!KMManager.isDefaultKey(key)) {
      KMLog.LogError(TAG, "getKeyboardIndex failed for key " + key);
    }
    return index;
  }

  /**
   * Given a packageID, keyboardID, and languageID, return the index of the matching keyboard.
   * If language ID not specified, only the package ID and keyboard ID are matched
   * @param packageID - String of the package ID
   * @param keyboardID - String of the keyboard ID
   * @param languageID - String of the language ID (optional)
   * @return int - Index of the matching keyboard
   */
  public int getKeyboardIndex(String packageID, String keyboardID, String languageID) {
    int index = INDEX_NOT_FOUND;
    if (!isInitialized || list == null) {
      KMLog.LogError(TAG, "getIndexOfKey while KeyboardController() not initialized");
      return index;
    }
    if (packageID == null || packageID.isEmpty() || keyboardID == null || keyboardID.isEmpty()) {
      return index;
    }

    boolean matchLanguage = (languageID != null && !languageID.isEmpty());

    synchronized (list) {
      for (int i=0; i<list.size(); i++) {
        Keyboard k = list.get(i);
        if (k.getPackageID().equalsIgnoreCase(packageID) && k.getKeyboardID().equalsIgnoreCase(keyboardID)) {
          if ( (matchLanguage && BCP47.languageEquals(k.getLanguageID(), languageID)) ||
              !matchLanguage ) {
            return i;
          }
        }
      }
    }

    // Sometimes it's expected that languageID isn't found in the keyboard list
    // (e.g. rendering the list of additional languages to install for an existing keyboard package)
    // See keyboardExists()
    return index;
  }

  /**
   * Given a key, return if the keyboard exists in the installed keyboards list
   * @param key - String of the key to find
   * @return boolean whether the matching keyboard exists
   */
  public boolean keyboardExists(String key) {
    return getKeyboardIndex(key) != INDEX_NOT_FOUND;
  }

  /**
   * Given a package ID, keyboard ID, and language ID, return if the keyboard exists
   * in the installed keyboards list
   * @param packageID - String of the package ID
   * @param keyboardID - String of the keyboard ID
   * @param languageID - String of the language ID
   * @return boolean whether the matching keyboard exists
   */
  public boolean keyboardExists(String packageID, String keyboardID, String languageID) {
    return getKeyboardIndex(packageID, keyboardID, languageID) != INDEX_NOT_FOUND;
  }

  /**
   * Add a new keyboard to the keyboard list. If the keyboard already exists, the keyboard
   * information is updated.
   * @param newKeyboard
   */
  public void add(Keyboard newKeyboard) {
    if (!isInitialized || list == null) {
      KMLog.LogError(TAG, "add while KeyboardController() not initialized");
      return;
    }

    synchronized (list) {
      for (int i = 0; i < list.size(); i++) {
        // Update existing keyboard entry
        if (newKeyboard.equals(list.get(i))) {
          Log.d(TAG, "Updating keyboard with newKeyboard");
          list.set(i, newKeyboard);
          return;
        }
      }

      // Add new keyboard
      list.add(newKeyboard);
    }
  }

  /**
   * Update a keyboard entry at a specified index
   * @param index int of the keyboard index to update
   * @param currentKeyboard Keyboard info
   */
  public void set(int index, Keyboard currentKeyboard) {
    if (!isInitialized || list == null) {
      KMLog.LogError(TAG, "set while KeyboardController() not initialized");
      return;
    }

    if (index < 0 || index >= list.size()) {
      KMLog.LogError(TAG, "set with index: " + index + " out of bounds");
      return;
    }

    synchronized (list) {
      list.set(index, currentKeyboard);
    }
  }

  /**
   * Remove the keyboard at the specified index.
   * @param index
   */
  public void remove(int index) {
    // Check initialized
    if (!isInitialized) {
      return;
    }

    synchronized (list) {
      if (index != INDEX_NOT_FOUND && index < list.size()) {
        list.remove(index);
      }
    }
  }

  /**
   * Convert the installed keyboard list to JSONArray and write to file
   * @param context
   * @return boolean - Status if the keyboard list was successfully saved
   */
  public boolean save(Context context) {
    boolean result = false;
    if (list == null || list.size() < 1) {
      return result;
    }

    JSONArray arr = new JSONArray();
    for (Keyboard k : list) {
      JSONObject o = k.toJSON();
      arr.put(o);
    }

    if (arr.length() < 1) {
      return result;
    }

    result = FileUtils.saveList(new File(context.getDir("userdata", Context.MODE_PRIVATE),
      KMFilename_Installed_KeyboardsList), arr);
    return result;
  }
}
