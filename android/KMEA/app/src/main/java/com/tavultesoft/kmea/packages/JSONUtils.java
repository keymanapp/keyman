package com.tavultesoft.kmea.packages;

import android.util.Log;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Map;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.JSONParser;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Class of static methods for parsing kmp.json files.
 * Extract keyboard/language info from keyboards installed via .kmp packages so it can be used in
 * Keyboard/Language picker menus
 */
public class JSONUtils {
  private static File resourceRoot = null;
  private static final String TAG = "JSONUtils";

  public static void initialize(File resourceRoot) {
    JSONUtils.resourceRoot = resourceRoot;
  }

  /**
   * Iterate through each package folder and parse kmp.json
   * Will need to swap kmp.json (keyboards : languages) to cloud order (languages : keyboards)
   */
  public static JSONArray getLanguages() {
    File[] packages = resourceRoot.listFiles();
    JSONArray languagesArray = new JSONArray();
    JSONParser parser = new JSONParser();

    for (File pkg: packages) {
      for (File file: pkg.listFiles()) {
        if (file.getName().toLowerCase().endsWith(PackageProcessor.PP_DEFAULT_METADATA)) {
          try {
            JSONObject kmp = parser.getJSONObjectFromFile(file);
            JSONArray kmpKeyboards = kmp.getJSONArray("keyboards");

            // Determine if kmp contains welcome.htm help file
            JSONArray kmpFiles = kmp.getJSONArray("files");
            boolean containsHelp = JSONUtils.findHelp(kmpFiles);

            for (int i=0; i<kmpKeyboards.length(); i++) {
              JSONObject kmpKeyboardObj = kmpKeyboards.getJSONObject(i);
              String kbdName = kmpKeyboardObj.getString(KMManager.KMKey_Name);
              String kbdID = kmpKeyboardObj.getString(KMManager.KMKey_ID);
              String kbdVersion = kmpKeyboardObj.getString(KMManager.KMKey_KeyboardVersion);
              String kbdFilename = pkg.getName() + "/" + kbdID + ".js";

              // Merge languages
              JSONArray kmpLanguageArray = kmpKeyboardObj.getJSONArray("languages");
              for (int j=0; j<kmpLanguageArray.length(); j++) {
                JSONObject languageObj = kmpLanguageArray.getJSONObject(j);
                String packageID = pkg.getName();
                String languageName = languageObj.getString(KMManager.KMKey_Name);
                String languageID = languageObj.getString(KMManager.KMKey_ID);

                JSONObject kbdObj = new JSONObject();
                kbdObj.put(KMManager.KMKey_PackageID, packageID);
                kbdObj.put(KMManager.KMKey_ID, kbdID);
                kbdObj.put(KMManager.KMKey_Name, kbdName);
                kbdObj.put("filename", kbdFilename);
                kbdObj.put(KMManager.KMKey_KeyboardVersion, kbdVersion);
                kbdObj.put(KMManager.KMKey_CustomKeyboard, "Y");
                if (kmpKeyboardObj.has(KMManager.KMKey_DisplayFont)) {
                 kbdObj.put(KMManager.KMKey_Font, kmpKeyboardObj.getString(KMManager.KMKey_DisplayFont));
                }
                if (kmpKeyboardObj.has(KMManager.KMKey_OskFont)) {
                  kbdObj.put(KMManager.KMKey_OskFont, kmpKeyboardObj.getString(KMManager.KMKey_OskFont));
                }
                if (containsHelp) {
                  File welcomeFile = new File(pkg, "welcome.htm");
                  kbdObj.put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
                }

                File jsFile = new File(pkg, kbdID + ".js");
                if (jsFile.exists()) {
                  SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-DD");
                  kbdObj.put("lastModified", sdf.format(jsFile.lastModified()));
                } else {
                  Log.d(TAG, "getLanguages() can't generate modified date for " + jsFile);
                }
                // TODO: source, filesize

                int index = findID(languagesArray, languageID);
                if (languagesArray.length() == 0 || index == -1) {
                  // Populate new entry entry into languagesArray
                  JSONArray tempKbdArray = new JSONArray();
                  tempKbdArray.put(kbdObj);

                  JSONObject tempLanguageObj = new JSONObject();
                  tempLanguageObj.put(KMManager.KMKey_ID, languageID);
                  tempLanguageObj.put(KMManager.KMKey_Name, languageName);
                  tempLanguageObj.put("keyboards", tempKbdArray);

                  languagesArray.put(tempLanguageObj);
                } else {
                  JSONObject tempLanguageObj = languagesArray.getJSONObject(index);
                  JSONArray tempKbdArray = tempLanguageObj.getJSONArray("keyboards");
                  tempKbdArray.put(kbdObj);
                }
              }
            }
          } catch (JSONException e) {
            Log.e(TAG, "getLanguages() Error parsing " + file.getName());
          }
        }
      }
    }

    return languagesArray;
  }

  /**
   * Iterate through a JSONArray to determine if a language/keyboard ID exists.
   * @param {a} JSONArray to search
   * @param {id} String of the language/keyboard ID
   * @return int - Index if the ID is found (starting with 0). -1 if the ID doesn't exist
   */
  public static int findID(JSONArray a, String id) {
    int ret = -1;

    if ((a == null) || (a.length() == 0) ||
      (id == null) || (id.isEmpty())) {
      return ret;
    }

    try {
      for (int i=0; i < a.length(); i++) {
        JSONObject o = a.getJSONObject(i);
        if (o.getString("id").equals(id)) {
          return i;
        }
      }
    } catch (JSONException e) {
      Log.e(TAG, "find ID() error: " + e);
    }
    return ret;
  }

  /**
   * Iterate through a JSONArray to determine if welcome.htm help file exists.
   * @param {a} JSONArray of files to search
   * @return boolean - if welcome.htm file is found
   */
  public static boolean findHelp(JSONArray a) {
    if ((a == null) || (a.length() == 0)) {
      return false;
    }

    try {
      for (int i=0; i<a.length(); i++) {
        JSONObject o = a.getJSONObject(i);
        if (FileUtils.isWelcomeFile(o.getString("name"))) {
          return true;
        }
      }
    } catch (JSONException e) {
      Log.e(TAG, "findHelp() error: " + e);
    }
    return false;
  }

  /**
   * Mirror options information that comes from keyboard cloud catalog
   * @param String deviceType
   * @return JSONObject
   */
  public static JSONObject defaultOptions(String deviceType) {
    JSONObject options = new JSONObject();
    try {
      options.put("context", "language");
      options.put("dateFormat", "standard");
      if (deviceType.equals("AndroidTablet")) {
        deviceType = "androidtablet";
      } else {
        deviceType = "androidphone";
      }
      options.put("device", deviceType);
      options.put("keyboardBaseUri", "https://s.keyman.com/keyboard/");
      options.put("fontBaseUri", "https://s.keyman.com/font/deploy/");
      options.put("keyboardVersion", "current");
    } catch (JSONException e) {
      Log.e(TAG, "defaultOptions() error: " + e);
    }
    return options;
  }
}
