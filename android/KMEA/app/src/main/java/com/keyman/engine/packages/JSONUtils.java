package com.keyman.engine.packages;

import android.util.Log;

import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Map;

import com.keyman.engine.KMManager;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.JSONParser;
import com.keyman.engine.util.KMLog;

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
    if (resourceRoot == null) {
      return new JSONArray();
    }
    File[] packages = resourceRoot.listFiles();
    if (packages == null) {
      return new JSONArray();
    }
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
              if (!kmpKeyboardObj.has("languages")) {
                continue;
              }
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
                  SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
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
            KMLog.LogException(TAG, "getLanguages() Error parsing " + file.getName(), e);
          }
        }
      }
    }

    return languagesArray;
  }

  /**
   * Iterate through a JSONArray to determine if a language/keyboard/model ID exists.
   * @param a JSONArray to search
   * @param id String of the language/keyboard ID
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
        if (o.getString("id").toLowerCase().equals(id.toLowerCase())) {
          return i;
        }
      }
    } catch (JSONException e) {
      KMLog.LogException(TAG, "find ID() error: ", e);
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
      KMLog.LogException(TAG, "findHelp() error: ", e);
    }
    return false;
  }

  /**
   * Mirror options information that comes from keyboard cloud catalog
   * @param deviceType
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
      KMLog.LogException(TAG, "defaultOptions() error: ", e);
    }
    return options;
  }

  /**
   * Iterate through each model's folder and parse kmp.json, whether installed or not.
   */
  public static JSONArray getLexicalModels() {
    File[] packages = new File(KMManager.getLexicalModelsDir()).listFiles();
    JSONArray lexicalModelsArray = new JSONArray();
    JSONParser parser = new JSONParser();

    for (File pkg: packages) {
      for (File file: pkg.listFiles()) {
        if (file.getName().toLowerCase().endsWith(PackageProcessor.PP_DEFAULT_METADATA)) {
          try {
            String packageID = pkg.getName();
            JSONObject kmp = parser.getJSONObjectFromFile(file);
            String packageVersion = PackageProcessor.getPackageVersion(kmp);
            JSONArray kmpModels = kmp.getJSONArray("lexicalModels");

            // Determine if kmp contains welcome.htm help file
            JSONArray kmpFiles = kmp.getJSONArray("files");
            boolean containsHelp = JSONUtils.findHelp(kmpFiles);

            for (int i=0; i<kmpModels.length(); i++) {
              JSONObject kmpModelObj = kmpModels.getJSONObject(i);
              String modelName = kmpModelObj.getString(KMManager.KMKey_Name);
              String modelID = kmpModelObj.getString(KMManager.KMKey_ID);
              String modelFilename = pkg.getName() + "/" + modelID + FileUtils.LEXICALMODEL;

              // Merge languages
              JSONArray kmpLanguageArray = kmpModelObj.getJSONArray("languages");
              for (int j=0; j<kmpLanguageArray.length(); j++) {
                JSONObject languageObj = kmpLanguageArray.getJSONObject(j);
                String languageID = languageObj.getString("id");
                String languageName = languageObj.getString("name");

                JSONObject modelObj = new JSONObject();
                modelObj.put(KMManager.KMKey_PackageID, packageID);
                modelObj.put(KMManager.KMKey_ID, modelID);
                modelObj.put(KMManager.KMKey_Name, modelName);
                modelObj.put("filename", modelFilename);
                modelObj.put(KMManager.KMKey_LexicalModelVersion, packageVersion);
                if (containsHelp) {
                  File welcomeFile = new File(pkg, "welcome.htm");
                  modelObj.put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
                }

                File jsFile = new File(pkg, modelID + FileUtils.LEXICALMODEL);
                if (jsFile.exists()) {
                  SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
                  modelObj.put("lastModified", sdf.format(jsFile.lastModified()));
                } else {
                  Log.d(TAG, "getLexicalModels() can't generate modified date for " + jsFile);
                }

                JSONArray languageArray = new JSONArray();
                languageArray.put(languageObj);
                modelObj.put("languages", languageArray);

                // TODO: maybe add source, filesize

                lexicalModelsArray.put(modelObj);

                int index = findID(lexicalModelsArray, modelID);
                if (lexicalModelsArray.length() == 0 || index == -1) {
                  // Populate new entry entry into languagesArray
                  JSONArray tempModelArray = new JSONArray();
                  tempModelArray.put(modelObj);

                  JSONObject tempModelObj = new JSONObject();
                  tempModelObj.put(KMManager.KMKey_ID, languageID);
                  tempModelObj.put(KMManager.KMKey_Name, languageName);
                  tempModelObj.put("models", tempModelArray);

                  lexicalModelsArray.put(tempModelObj);
                }
              }
            }
          } catch (JSONException e) {
            KMLog.LogException(TAG, "getLexicalModels() Error parsing " + file.getName(), e);
          }
        }
      }
    }

    return lexicalModelsArray;
  }

}
