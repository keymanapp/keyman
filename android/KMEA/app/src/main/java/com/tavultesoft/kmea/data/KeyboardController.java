/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea.data;

import android.content.Context;
import android.util.Log;

import com.tavultesoft.kmea.JSONParser;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.MapCompat;
import com.tavultesoft.kmea.KMManager;

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
          // Migrate installed_keyboards.dat to installed_keyboards.json
          ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(keyboards_dat));
          ArrayList<HashMap<String, String>> dat_list = (ArrayList<HashMap<String, String>>) inputStream.readObject();
          inputStream.close();

          for(HashMap<String, String> kbdMap : dat_list) {
            boolean isNewKeyboard = kbdMap.containsKey(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD) &&
              kbdMap.get(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD).equals(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD);

            Keyboard k = new Keyboard(
              kbdMap.get(KMManager.KMKey_PackageID),
              kbdMap.get(KMManager.KMKey_KeyboardID),
              kbdMap.get(KMManager.KMKey_KeyboardName),
              kbdMap.get(KMManager.KMKey_LanguageID),
              kbdMap.get(KMManager.KMKey_LanguageName),
              MapCompat.getOrDefault(kbdMap, KMManager.KMKey_Version, "1.0"),
              MapCompat.getOrDefault(kbdMap, KMManager.KMKey_CustomHelpLink, ""),
              isNewKeyboard,
              MapCompat.getOrDefault(kbdMap, KMManager.KMKey_Font, null),
              MapCompat.getOrDefault(kbdMap, KMManager.KMKey_OskFont, null)
            );
            list.add(k);
          }

        } catch (Exception e) {
          Log.e(TAG, "Exception migrating installed_keyboards.dat");
          list.add(Keyboard.DEFAULT_KEYBOARD);
        }
      } else if (keyboards_json.exists()) {
        try {
          // Get installed keyboards from installed_keyboards.json
          JSONParser jsonParser = new JSONParser();
          JSONArray json_list = jsonParser.getJSONObjectFromFile(keyboards_json, JSONArray.class);
          if (json_list != null) {
            // Can't foreach JSONArray
            for (int i=0; i<json_list.length(); i++) {
              JSONObject o = json_list.getJSONObject(i);
              if (o != null) {
                Keyboard k = new Keyboard(o);
                list.add(k);
              }
            }
          }
        } catch (Exception e) {
          Log.e(TAG, "Exception reading installed_keyboards.json");
          list.add(Keyboard.DEFAULT_KEYBOARD);
        }
      } else {
        // No installed keyboards lists
        // 3rd-party OEM may not have sil_euro_latin, so don't assign a default keyboard
        //list.add(Keyboard.DEFAULT_KEYBOARD);
        Log.w(TAG, "initialize with no default keyboard");
      }

      // We'd prefer not to overwrite a file if it exists
      if (!keyboards_json.exists() && list != null && list.size() > 0) {
        save(context);
      }

      // TODO when feature stabilized: Delete legacy keyboards_list.dat (KMManager.KMFilename_KeyboardsList)
    }
    isInitialized = true;
  }

  /**
   * Return the installed keyboards list
   * @return
   */
  public List<Keyboard> get() {
    if (!isInitialized) {
      Log.e(TAG, "get while KeyboardController() not initialized");
      return null;
    }
    synchronized (list) {
      return list;
    }
  }

  /**
   * Return the keyboard info at index
   * @param index - int
   * @return Keyboard
   */
  public Keyboard getKeyboardInfo(int index) {
    if (!isInitialized) {
      Log.e(TAG, "getKeyboardInfo while KeyboardController() not initialized");
      return null;
    } else if (index < 0) {
      Log.e(TAG, "getKeyboardInfo with invalid index: " + index);
      return null;
    }

    synchronized (list) {
      if (list != null && index < list.size()) {
        Keyboard k = list.get(index);
        return k;
      }
    }

    Log.e(TAG, "getKeyboardInfo failed with index " + index);
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
      Log.e(TAG, "getIndexOfKey while KeyboardController() not initialized");
      return index;
    }
    if (key == null || key.isEmpty()) {
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

    Log.e(TAG, "getKeyboardIndex failed for key " + key);
    return index;
  }

  /**
   * Given a languageID and keyboardID, return the index of the matching keyboard.
   * If no match, returns INDEX_NOT_FOUND
   * @param languageID - String of the language ID
   * @param keyboardID - String of the keyboard ID
   * @return int - Index of the matching keyboard
   */
  public int getKeyboardIndex(String languageID, String keyboardID) {
    String key = String.format("%S_%s", languageID, keyboardID);
    return getKeyboardIndex(key);
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
   * Add a new keyboard to the keyboard list. If the keyboard already exists, the keyboard
   * information is updated.
   * @param newKeyboard
   */
  public void add(Keyboard newKeyboard) {
    if (!isInitialized || list == null) {
      Log.e(TAG, "add while KeyboardController() not initialized");
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
   * Remove the keyboard at the specified index.
   * @param index
   */
  public void remove(int index) {
    // Check initialized, and disallow removing default keyboard
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

    result = FileUtils.saveList(context, KMFilename_Installed_KeyboardsList, arr);
    return result;
  }
}
