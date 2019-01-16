package com.tavultesoft.kmea.packages;

import android.util.Log;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Map;

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

    for (File packageID: packages) {
      for (File file: packageID.listFiles()) {
        if (file.getName().toLowerCase().endsWith(PackageProcessor.PPDefault_Metadata)) {
          try {
            JSONObject kmp = parser.getJSONObjectFromFile(file);
            JSONArray kmpKeyboards = kmp.getJSONArray("keyboards");

            // Determine if kmp contains welcome.htm help file
            JSONArray kmpFiles = kmp.getJSONArray("files");
            boolean containsHelp = JSONUtils.findHelp(kmpFiles);

            for (int i=0; i<kmpKeyboards.length(); i++) {
              JSONObject kmpKeyboardObj = kmpKeyboards.getJSONObject(i);
              String kbdName = kmpKeyboardObj.getString("name");
              String kbdID = kmpKeyboardObj.getString("id");
              String kbdVersion = kmpKeyboardObj.getString("version");
              String kbdFilename = packageID.getName() + "/" + kbdID + ".js";

              // Create optional font JSON object
              JSONObject fontObj = new JSONObject();
              String kmpFont = kmpKeyboardObj.optString("oskFont");
              if (kmpFont.isEmpty()) {
                kmpFont = kmpKeyboardObj.optString("displayFont");
              }
              if (!kmpFont.isEmpty()) {
                JSONArray kmpFontArray = new JSONArray();
                // TODO: does this need a fuller path (include package ID?)
                kmpFontArray.put(kmpFont);
                fontObj.put("family", kmpFont);
                fontObj.put("source", kmpFontArray);
              }

              // Merge languages
              JSONArray kmpLanguageArray = kmpKeyboardObj.getJSONArray("languages");
              for (int j=0; j<kmpLanguageArray.length(); j++) {
                JSONObject languageObj = kmpLanguageArray.getJSONObject(j);
                String languageName = languageObj.getString("name");
                String languageID = languageObj.getString("id");

                JSONObject kbdObj = new JSONObject();
                kbdObj.put("id", kbdID);
                kbdObj.put("name", kbdName);
                kbdObj.put("filename", kbdFilename);
                kbdObj.put("version", kbdVersion);
                kbdObj.put("custom", "Y");
                if (fontObj != null) {
                  kbdObj.put("font", fontObj);
                }
                if (containsHelp) {
                  kbdObj.put("helpFile", packageID + "/welcome.htm");
                }

                File jsFile = new File(packageID, kbdID + ".js");
                if (jsFile.exists()) {
                  SimpleDateFormat sdf = new SimpleDateFormat("YYYY-MM-DD");
                  kbdObj.put("lastModified", sdf.format(jsFile.lastModified()));
                }
                // TODO: source, filesize

                int index = findID(languagesArray, languageID);
                if (languagesArray.length() == 0 || index == -1) {
                  // Populate new entry entry into languagesArray
                  JSONArray tempKbdArray = new JSONArray();
                  tempKbdArray.put(kbdObj);

                  JSONObject tempLanguageObj = new JSONObject();
                  tempLanguageObj.put("id", languageID);
                  tempLanguageObj.put("name", languageName);
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
}