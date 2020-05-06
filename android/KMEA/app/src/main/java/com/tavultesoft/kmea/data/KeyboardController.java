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
  public static final String TAG = "InstalledKBList";
  public static final String KMFilename_Installed_KeyboardsList = "keyboards_list.json";

  private static KeyboardController instance;

  /**
   * @return get or create shared instance.
   */
  public static KeyboardController getInstance() {
    if (instance == null) {
      createInstance();
    }
    return instance;
  }

  /**
   * create singleton instance.
   */
  private synchronized static void createInstance()
  {
    if (instance != null) {
      return;
    }
    instance = new KeyboardController();
  }

  private boolean isInitialized = false;
  private List<Keyboard> list;

  public synchronized  void initialize(Context context) {
    if (isInitialized) {
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
        // No installed keyboards lists so assume default
        // TODO: What about 3rd-party apps w/o sil_euro_latin?
        list.add(Keyboard.DEFAULT_KEYBOARD);
      }

      // We'd prefer not to overwrite a file if it exists
      if (!keyboards_json.exists()) {
        save(context);
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
      return null; // Log error?
    }
    return list;
  }

  /**
   * Return the keyboard info at index
   * @param index - int
   * @return Keyboard
   */
  public Keyboard getKeyboardInfo(int index) {
    if (!isInitialized || (index < 0)) {
      return null;
    }

    if (list != null && index < list.size()) {
      Keyboard k = list.get(index);
      return k;
    }

    return null;
  }

  /**
   * Add a new keyboard to the keyboard list. If the keyboard already exists, the keyboard
   * information is updated
   * @param newKeyboard
   */
  public void add(Keyboard newKeyboard) {
    if (!isInitialized || list == null) {
      return;
    }

    for (int i=0; i<list.size(); i++) {
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


  /**
   * Convert the installed keyboard list to JSONArray and write to file
   * @param context
   */
  public void save(Context context) {
    JSONArray arr = new JSONArray();
    for (Keyboard k : list) {
      JSONObject o = k.toJSON();
      arr.put(o);
    }
    FileUtils.saveList(context, KMFilename_Installed_KeyboardsList, arr);
  }
}
