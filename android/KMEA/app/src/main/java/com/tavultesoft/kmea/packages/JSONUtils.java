package com.tavultesoft.kmea.packages;

import android.util.Log;

import java.io.File;
import java.util.ArrayList;
import java.util.Map;

import org.apache.commons.io.FileUtils;
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

    for (File packageID: packages) {
      for (File file: packageID.listFiles()) {
        if (file.getName().toLowerCase().endsWith(PackageProcessor.PPDefault_Metadata)) {
          try {
            JSONObject kmp = parser.getJSONObjectFromFile(file);
            JSONArray kmpKeyboards = kmp.getJSONArray("keyboards");
            for (int i=0; i<kmpKeyboards.length(); i++) {
              JSONObject kmpKeyboardObj = kmpKeyboards.getJSONObject(i);
              String kbdName = kmpKeyboardObj.getString("name");
              String kbdID = kmpKeyboardObj.getString("id");
              String kbdVersion = kmpKeyboardObj.getString("version");
              String kbdFilename = packageID.getName() + "/" + kbdID + "-" + kbdVersion + ".js";

              JSONArray kmpLanguageArray = kmpKeyboardObj.getJSONArray("languages");
              for (int j=0; j<kmpLanguageArray.length(); j++) {
                JSONObject languageObj = kmpLanguageArray.getJSONObject(j);
                String languageName = languageObj.getString("name");
                String languageID = languageObj.getString("id");

                // Populate new entry entry into languagesArray
                int index = findLanguageID(languagesArray, languageID);
                if (languagesArray.length() == 0 || index == -1) {
                  JSONObject kbdObj = new JSONObject();
                  kbdObj.put("id", kbdID);
                  kbdObj.put("name", kbdName);
                  kbdObj.put("filename", kbdFilename);
                  kbdObj.put("version", kbdVersion);
                  kbdObj.put("custom", "Y");

                  JSONArray tempKbdArray = new JSONArray();
                  tempKbdArray.put(kbdObj);

                  JSONObject tempLanguageObj = new JSONObject();
                  tempLanguageObj.put("id", languageID);
                  tempLanguageObj.put("name", languageName);
                  tempLanguageObj.put("keyboards", tempKbdArray);

                  languagesArray.put(tempLanguageObj);
                } else {
                  JSONObject kbdObj = new JSONObject();
                  kbdObj.put("id", kbdID);
                  kbdObj.put("name", kbdName);
                  kbdObj.put("filename", kbdFilename);
                  kbdObj.put("version", kbdVersion);
                  kbdObj.put("custom", "Y");

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
   * Iterate through a JSON object to determine if a language ID exists.
   * @param languageArray JSONArray to search
   * @param languageID String of the langugae ID
   * @return int - Index if the language ID is found (starting with 0). -1 if the language ID doesn't exist
   */
  public static int findLanguageID(JSONArray languageArray, String languageID) {
    int ret = -1;

    if ((languageArray == null) || (languageArray.length() == 0) ||
      (languageID == null) || (languageID.isEmpty())) {
      return ret;
    }

    try {
      for (int i=0; i < languageArray.length(); i++) {
        JSONObject o = languageArray.getJSONObject(i);
        if (o.getString("id").equals(languageID)) {
          return i;
        }
      }
    } catch (JSONException e) {
      Log.e(TAG, "findLanguageID() error " + e);
    }
    return ret;
  }
}