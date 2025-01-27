/**
 * Copyright (C) 2017-2020 SIL International. All rights reserved.
 */
package com.keyman.engine.packages;

import android.widget.Toast;

import androidx.annotation.NonNull;

import com.keyman.engine.KMManager;
import com.keyman.engine.JSONParser;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.ZipUtils;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * A class for use in handling Keyman's .kmp package file format, as it relates to the
 * KMEA engine.  This is primarily for installing keyboard packages.
 * Some utility methods are available for parsing the kmp.json before the .kmp file has been extracted.
 * Other utilities assume the package file has already been extracted to the file system.
 */
public class PackageProcessor {
  protected File resourceRoot = null;

  public static final String PP_DEFAULT_VERSION = "1.0";
  public static final String PP_DEFAULT_METADATA = "kmp.json";

  // Package target types
  public static final String PP_TARGET_INVALID = "invalid";
  public static final String PP_TARGET_KEYBOARDS = "keyboards";
  public static final String PP_TARGET_LEXICAL_MODELS = "lexicalModels";

  // keys in kmp.json
  public static final String PP_KEYBOARDS_KEY = "keyboards";
  public static final String PP_KEYBOARD_ID_KEY = "id";
  public static final String PP_LEXICAL_MODELS_KEY = "lexicalModels";
  public static final String PP_FILES_KEY = "files";
  public static final String PP_FILES_NAME_KEY = "name";
  public static final String PP_LANGUAGES_KEY = "languages";

  private static final String TAG = "PackageProcessor";

  public PackageProcessor(File resourceRoot) {
    this.resourceRoot = resourceRoot;
  }

  /**
   * Parse the kmp path and extract the package ID.
   * This doesn't need to unzip and parse kmp.json
   * @param path of the kmp file
   * @return String - package ID
   * @throws IllegalStateException
   * @throws IllegalArgumentException
   */
  public String getPackageID(File path) {
    // Sometimes, downloading a kmp multiple times results in a filename with a suffix:
    // " (#).kmp" or "-#.kmp" in the filename, so strip out the number
    String filename = path.getName().toLowerCase().replaceAll(
      "\\s*((\\(\\d+\\))|(-\\d+))\\.kmp$", ".kmp");

    if(resourceRoot == null) {
      throw new IllegalStateException("The PackageProcessor has not been initialized!");
    }

    if(filename.lastIndexOf('.') != -1) {
      // Android's temp downloads attach a suffix to the extension; .kmp isn't the end of the filename.
      if(!FileUtils.hasKeymanPackageExtension(filename)) {
        throw new IllegalArgumentException("Invalid file passed to the KMP unpacker!");
      }

      // Extract our package ID from .model.kmp file
      if (filename.lastIndexOf(FileUtils.MODELPACKAGE) != -1) {
        return filename.substring(0, filename.lastIndexOf(FileUtils.MODELPACKAGE));
      }

      // Extract our best-guess name for the package and construct the temporary package name.
      return filename.substring(0, filename.lastIndexOf(FileUtils.KEYMANPACKAGE));
    } else {
      throw new IllegalArgumentException("Invalid file passed to the KMP unpacker!");
    }
  }

  /**
   * A default, managed mapping for package installation, handling both temp directory
   * and perm directory locations.  No need to relocate the downloaded .kmp file itself.
   * @param path File - path to  the .kmp
   * @param temp boolean - If true, prepends ".{package ID}.temp" to the folder
   * @return File - path to extract the .kmp file
   */
  @NonNull
  protected File constructPath(File path, boolean temp) {
    String kmpBaseName = getPackageID(path);
    // Feel free to change this as desired - simply ensure it is unique enough to never be used as
    // a legitimate package name.
    String kmpFolderName = temp ? "." + kmpBaseName + ".temp" : kmpBaseName;

    return new File(resourceRoot, KMManager.KMDefault_AssetPackages + File.separator + kmpFolderName + File.separator);
  }

  /**
   * Unzips the package.kmp file to its mapped temporary directory location.
   * @param path The file path of the .kmp file, file name included.
   * @return The mapped temporary file path for the .kmp file's contents.
   * @throws IOException Exception from unzipping
   */
  public File unzipKMP(File path) throws IOException {
    File tempKeyboardPath = constructPath(path, true);
    if (!tempKeyboardPath.exists()) {
      tempKeyboardPath.mkdir();
    }
    ZipUtils.unzip(path, tempKeyboardPath);

    return tempKeyboardPath;
  }

  /**
   * Given temp path of extracted keyboard package kmp file and package ID,
   * move the tempPath to the permanent packages/ folder
   * @param tempPath Filepath of temporarily extracted .kmp file
   * @param packageID String of the package ID
   */
  public void moveTempToPackages(File tempPath, String packageID) {
    File permPath = new File(resourceRoot, KMManager.KMDefault_AssetPackages + File.separator + packageID + File.separator);
    try {
      if (permPath.exists()) {
        // Out with the old.  "In with the new" is identical to a new package installation.
        FileUtils.deleteDirectory(permPath);
      }
    } catch (IOException e) {

    }
    // No version conflict!  Proceed with the install!
    // Unfortunately, the nice recursive method provided by Apache Commons-IO's FileUtils class
    // isn't available at Android runtime.
    tempPath.renameTo(permPath);
  }

  /**
   * Given a directory location for an extracted KMP file, extracts its kmp.json information
   * into a JSON object.  Works on temporary directories and the installed package directory.
   * @param packagePath The extracted location information to retrieve information for.
   * @return A metadata JSONObject for the package version.
   */
  public JSONObject loadPackageInfo(File packagePath) {
    File infoFile = new File(packagePath, PP_DEFAULT_METADATA);

    if(infoFile.exists()) {
      JSONParser parser = new JSONParser();
      return parser.getJSONObjectFromFile(infoFile);
    } else {
      KMLog.LogError(TAG, "kmp.json does not exist");
      return null;
    }
  }

  // Call this once per each entry of the JSON `keyboards` array, then concatenate the resulting arrays for a full list.

  /**
   * Generates a list of keyboard data maps designed to mirror the `download` method output of
   * KMKeyboardDownloader as closely as practical.
   * @param jsonEntry  One entry of the master JSONArray of the top-level "keyboards" property.
   * @param packageId  Package ID
   * @param packageVersion Package version (used for lexical model version)
   * @param languageList List of preferred language IDs to associate with the entry. If null,
   *                   the first language in kmp.json is used.
   * @return A list of maps defining one keyboard-language pairing each.
   * @throws JSONException
   */
  public Map<String, String>[] processEntry(JSONObject jsonEntry, String packageId, String packageVersion, ArrayList<String> languageList) throws JSONException {
    JSONArray languages = jsonEntry.getJSONArray(PP_LANGUAGES_KEY);
    int preferredLanguageCount = (languageList != null && !languageList.isEmpty()) ? languageList.size() : 1;
    String defaultLanguageID = languages.getJSONObject(0).getString("id");
    if (languageList == null || languageList.isEmpty()) {
      languageList = new ArrayList<String>();
      languageList.add(defaultLanguageID);
    }

    String keyboardId = jsonEntry.getString(PP_KEYBOARD_ID_KEY);
    // Check that package has touch keyboards.
    if (touchKeyboardExists(packageId, keyboardId)) {
      HashMap<String, String>[] keyboards = new HashMap[preferredLanguageCount];
      boolean firstLanguageAdded = false;
      int i=0;
      for (String languageID : languageList) {
        keyboards[i] = new HashMap<>();
        keyboards[i].put(KMManager.KMKey_PackageID, packageId);
        keyboards[i].put(KMManager.KMKey_KeyboardName, jsonEntry.getString("name"));
        keyboards[i].put(KMManager.KMKey_KeyboardID, jsonEntry.getString(PP_KEYBOARD_ID_KEY));

        int languageIndex = JSONUtils.findID(languages, languageID);
        if (languageIndex == -1) {
          // languageID not found so use first language
          if (!firstLanguageAdded) {
            languageIndex = 0;
          } else {
            continue;
          }
        }
        keyboards[i].put(KMManager.KMKey_LanguageID, languages.getJSONObject(languageIndex).getString("id").toLowerCase());
        keyboards[i].put(KMManager.KMKey_LanguageName, languages.getJSONObject(languageIndex).getString("name"));

        keyboards[i].put(KMManager.KMKey_KeyboardVersion, jsonEntry.getString("version"));
        if (jsonEntry.has("displayFont")) {
          keyboards[i].put(KMManager.KMKey_Font, jsonEntry.getString("displayFont"));
        }
        if (jsonEntry.has("oskFont")) {
          keyboards[i].put(KMManager.KMKey_OskFont, jsonEntry.getString("oskFont"));
        }

        if (welcomeExists(packageId)) {
          File kmpFile = new File(packageId + ".kmp");
          File packageDir = constructPath(kmpFile, false);
          File welcomeFile = new File(packageDir, FileUtils.WELCOME_HTM);
          // Only storing relative instead of absolute paths as a convenience for unit tests.
          keyboards[i].put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
        }

        i++;
      }

      return keyboards;
    } else {
      return null;
    }
  }

  /**
   * Get the language and keyboard information for the keyboard in a keyboard package at a specified index.
   * Use placeholders for other fields like help links, and font info.
   * @param json kmp.json as a JSON object
   * @param packageID String of the keyboard package ID
   * @param index int of the keyboard entry
   * @return Keyboard info. Null if no keyboard found
   */
  public static Keyboard getKeyboard(JSONObject json, String packageID, int index) {
    try {
      if (!json.has(PP_KEYBOARDS_KEY)) {
        return null;
      }
      JSONObject keyboardObj = json.getJSONArray(PP_KEYBOARDS_KEY).getJSONObject(index);
      JSONArray languages = keyboardObj.getJSONArray(PP_LANGUAGES_KEY);
      // Just get first language
      JSONObject languageObj = languages.getJSONObject(0);
      return new Keyboard(
        packageID,
        keyboardObj.getString(PP_KEYBOARD_ID_KEY),
        keyboardObj.getString("name"),
        languageObj.getString("id"),
        languageObj.getString("name"),
        keyboardObj.getString("version"),
        null,
        null,
        true,
        null,
        null
      );
    } catch (JSONException e) {
      KMLog.LogException(TAG, "", e);
      return null;
    }
  }

  /**
   * Parse a kmp.json JSON object and return the number of touch-layout keyboards (.JS) matching
   * keyboard IDs in the "keyboards" JSONArrray.
   * Lexical model packages return 0
   * @param json kmp.json as a JSON object
   * @return int of number of matching touch-layout keyboards. 0 if not found
   */
  public static int getKeyboardCount(JSONObject json) {
    int count = 0;
    try {
      if (!json.has(PP_KEYBOARDS_KEY)) {
        return count;
      }

      // Count the number of keyboard ID's that have a matching JS file is in the kmp
      JSONArray keyboards = json.getJSONArray(PP_KEYBOARDS_KEY);
      for (int k = 0; k <  keyboards.length(); k++) {
        JSONObject keyboardObj = keyboards.getJSONObject(k);
        String keyboardID = keyboardObj.getString(PP_KEYBOARD_ID_KEY);
        String expectedKeyboardFilename = keyboardID + FileUtils.JAVASCRIPT;
        JSONArray files = json.getJSONArray(PP_FILES_KEY);
        for (int f = 0; f < files.length(); f++) {
          JSONObject file = files.getJSONObject(f);
          String filename = file.getString(PP_FILES_NAME_KEY);
          if (filename != null && filename.equals(expectedKeyboardFilename)) {
            count++;
            break;
          }
        }
      }
    } catch (JSONException e) {
      // Setting count to 0 due to invalid package
      count = 0;
      KMLog.LogException(TAG, "", e);
    }

    return count;
  }

  /**
   * Parse a kmp.json JSON object and return the language count for the specified resource and index
   * @param json kmp.json as a JSON object
   * @param key PP_KEYBOARDS_KEY or PP_LEXICAL_MODELS_KEY
   * @param index int Item number the resource array
   * @return int of number of languages. 0 if not found
   */
  public static int getLanguageCount(JSONObject json, String key, int index) {
    int count = 0;
    try {
      if ( (key.equals(PP_KEYBOARDS_KEY) && !json.has(PP_KEYBOARDS_KEY)) ||
           (key.equals(PP_LEXICAL_MODELS_KEY) && !json.has(PP_LEXICAL_MODELS_KEY)) ){
        KMLog.LogError(TAG, "kmp.json doesn't contain " + key);
        return count;
      }
      JSONArray resources = json.getJSONArray(key);
      JSONArray languages = resources.getJSONObject(index).getJSONArray(PP_LANGUAGES_KEY);
      count = languages.length();
    } catch (NullPointerException e) {
      KMLog.LogException(TAG, "getLanguageCount with null JSONObject", e);
    } catch (JSONException e) {
      KMLog.LogException(TAG, "", e);
    }

    return count;
  }

  /**
   * Parse a kmp.json JSON object for a given keyboard ID and return the keyboard version
   * @param json kmp.json as a JSON object
   * @param kbdId String of the keyboard ID
   * @return String of the keyboard version
   */
  public static String getKeyboardVersion(JSONObject json, String kbdId) {
    try {
      JSONArray keyboards = json.getJSONArray(PP_KEYBOARDS_KEY);

      for (int i = 0; i < keyboards.length(); i++) {
        JSONObject keyboard = keyboards.getJSONObject(i);

        if (keyboard.getString(PP_KEYBOARD_ID_KEY).equals(kbdId)) {
          return keyboard.getString("version");
        }
      }

      return null;
    } catch (JSONException e) {
      KMLog.LogException(TAG, "", e);
      return null;
    }
  }

  /**
   * Parse a kmp.json JSON object and return the package's name.
   * @param json kmp.json as a JSON object
   * @return String of the package name
   */
  public String getPackageName(JSONObject json) {
    try {
      return json.getJSONObject("info").getJSONObject("name").getString("description");
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
      return "";
    }
  }

  /**
   * Parse a kmp.json JSON object and return the package's version number.
   * If undefined, return default version "1.0"
   * @param json kmp.json as a JSON object.
   * @return String of the package version number
   */
  public static String getPackageVersion(JSONObject json) {
    try {
      return json.getJSONObject("info").getJSONObject("version").getString("description");
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
      return PP_DEFAULT_VERSION;
    }
  }

  /**
   * Parse a kmp.json JSON object and return the package's minimum keyboard version (system.fileVersion).
   * This is to avoid issues with app crashing if keyboard functionality is not supported.
   * If undefined, return default version "7.0".
   * @param json kmp.json as a JSON object.
   * @return String of the package's minimum keyboard version
   */
  public static String getPackageMinimumKeyboardVersion(JSONObject json) {
    try {
      return json.getJSONObject("system").getString("fileVersion");
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
      return "7.0";
    }
  }

  /**
   * Parse a kmp.json JSON object and return the package's target (keyboards vs lexical models).
   * Only one can be valid. Otherwise, return "invalid"
   * @param json kmp.json as a JSON object.
   * @return String of the package target ("keyboards", "lexicalModels", or "invalid")
   */
  public String getPackageTarget(JSONObject json) {
    if (json == null) {
      KMLog.LogError(TAG, "kmp.json is null");
      return PP_TARGET_INVALID;
    }
    try {
      if (json.has(PP_KEYBOARDS_KEY) && !json.has(PP_LEXICAL_MODELS_KEY)) {
        return PP_TARGET_KEYBOARDS;
      } else if (json.has(PP_LEXICAL_MODELS_KEY) && !json.has(PP_KEYBOARDS_KEY)) {
        return PP_TARGET_LEXICAL_MODELS;
      } else {
        return PP_TARGET_INVALID;
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
      return PP_TARGET_INVALID;
    }
  }

  /**
   * Simply extracts the package's target (keyboards vs lexical models).
   * Only one can be valid. Otherwise, return "invalid"
   * @param kmpPath File - The path to the kmp
   * @return String The target ("keyboards", "lexicalModels", or "invalid")
   */
  public String getPackageTarget(File kmpPath) {
    try {
      File tempPath = unzipKMP(kmpPath);
      String kmpFilename = kmpPath.getName();

      KMLog.LogBreadcrumb(TAG, "Determining package type for " + kmpFilename, false);
      String target = getPackageTarget(loadPackageInfo(tempPath));
      FileUtils.deleteDirectory(tempPath);
      return target;
    } catch (Exception e) {
      KMLog.LogException(TAG, "", e);
      return PP_TARGET_INVALID;
    }
  }

  /**
   * Parse a kmp.json JSON object and return boolean if the package contains welcome.htm
   * @param json kmp.json as a JSON object
   * @return boolean
   */
  public boolean hasWelcome(JSONObject json) {
    try {
      JSONArray files = json.getJSONArray("files");
      for (int i=0; i<files.length(); i++) {
        JSONObject fileInfo = files.getJSONObject(i);
        if (FileUtils.isWelcomeFile(fileInfo.getString("name"))) {
          return true;
        }
      }
    } catch (JSONException e) {
      KMLog.LogException(TAG, "", e);
      return false;
    }
    return false;
  }

  /**
   * Compares version information, ideally between the temporary extraction path of a package and its
   * desired installation path, to ensure that no accidental downgrade or side-grade overwrite occurs.
   * @param newPath The path for the (temporarily) extracted, newly downloaded version of the package.
   * @param oldPath The path to which the package should be installed.  May not actually exist yet.
   * @return Returns 1 if newer, 0 if equal, and -1 if older or invalid.  If no prior version exists, returns 1.
   * @throws IOException
   * @throws JSONException
   */
  public int comparePackageDirectories(File newPath, File oldPath) throws IOException, JSONException {
    JSONObject newInfoJSON = loadPackageInfo(newPath);
    String newVersion = getPackageVersion(newInfoJSON);

    if(oldPath.exists()) {
      JSONObject oldInfoJSON = loadPackageInfo(oldPath);
      String originalVersion = getPackageVersion(oldInfoJSON);

      if(FileUtils.compareVersions(newVersion, originalVersion) == FileUtils.VERSION_GREATER) {
        return 1;
      } else if(FileUtils.compareVersions(newVersion, originalVersion) == FileUtils.VERSION_EQUAL){
        return 0;
      } else {
        return -1;
      }
    } else {
      return 1;
    }
  }

  /**
   * Determines if installing the .kmp would result in a downgrade.
   * @param kmpPath The path to the .kmp to be installed.
   * @return <code><b>true</b></code> if installing would be a downgrade, otherwise <code><b>false</b></code>.
   * @throws IOException
   * @throws JSONException
   */
  public boolean isDowngrade(File kmpPath) throws IOException, JSONException {
    return internalCompareKMPVersion(kmpPath, false,-1);
  }

  /**
   * Determines if installing the .kmp would result in a downgrade.
   * @param kmpPath The path to the .kmp to be installed.
   * @param preExtracted Indicates use of a temporary testing 'kmp' that is pre-extracted.
   * @return <code><b>true</b></code> if installing would be a downgrade, otherwise <code><b>false</b></code>.
   * @throws IOException
   * @throws JSONException
   */
  public boolean isDowngrade(File kmpPath, boolean preExtracted) throws IOException, JSONException {
    return internalCompareKMPVersion(kmpPath, preExtracted,-1);
  }

  /**
   * Determines if installing the .kmp would result in a downgrade.  For testing only.
   * @param kmpPath The path to the .kmp to be installed.
   * @return <code><b>true</b></code> if installing would be a downgrade, otherwise <code><b>false</b></code>.
   * @throws IOException
   * @throws JSONException
   */
  public boolean isSameVersion(File kmpPath) throws IOException, JSONException {
    return internalCompareKMPVersion(kmpPath, false,0);
  }

  /**
   * Determines if installing the .kmp would result in a downgrade.  For testing only.
   * @param kmpPath The path to the .kmp to be installed.
   * @param preExtracted Indicates use of a temporary testing 'kmp' that is pre-extracted.
   * @return <code><b>true</b></code> if installing would be a downgrade, otherwise <code><b>false</b></code>.
   * @throws IOException
   * @throws JSONException
   */
  boolean isSameVersion(File kmpPath, boolean preExtracted) throws IOException, JSONException {
    return internalCompareKMPVersion(kmpPath, preExtracted,0);
  }

  boolean internalCompareKMPVersion(File kmpPath, boolean preExtracted, int compValue) throws IOException, JSONException {
    File tempPath;
    if(preExtracted) {
      tempPath = constructPath(kmpPath, true);
    } else {
      tempPath = unzipKMP(kmpPath);
    }

    int compRes = comparePackageDirectories(tempPath, constructPath(kmpPath, false));

    if(!preExtracted) {
      FileUtils.deleteDirectory(tempPath);
    }

    return compRes == compValue;
  }

  boolean touchKeyboardExists(final String packageId, final String keyboardId) {
    if (resourceRoot != null) {
      FileFilter touchKeyboardFilter = new FileFilter() {
        @Override
        public boolean accept(File pathname) {
          if (pathname.isFile() && pathname.getName().startsWith(keyboardId) && FileUtils.hasJavaScriptExtension(pathname.getName())) {
            return true;
          }
          return false;
        }
      };

      File kmpFile = new File(packageId + ".kmp");
      File packageDir = constructPath(kmpFile, false);
      File[] files = packageDir.listFiles(touchKeyboardFilter);
      if (files != null && files.length > 0) {
        return true;
      }
    }

    return false;
  }

  protected boolean welcomeExists(final String packageId) {
    if (resourceRoot != null) {
      FileFilter welcomeFilter = new FileFilter() {
        @Override
        public boolean accept(File pathname) {
          if (pathname.isFile() && FileUtils.isWelcomeFile(pathname.getName())) {
            return true;
          }
          return false;
        }
      };

      File kmpFile = new File(packageId + ".kmp");
      File packageDir = constructPath(kmpFile, false);
      File[] files = packageDir.listFiles(welcomeFilter);
      if (files != null && files.length > 0) {
        return true;
      }
    }

    return false;
  }

  /**
   * Generates a list of Keyboards from the keyboard JSONObject. baseKeyboard contains
   * keyboard information that's already installed. list is sorted by language name
   * @param keyboardJSON JSONObject - Keyboard JSONObject from kmp.json (entry for keyboardID)
   * @param baseKeyboard Keyboard - information of the keyboard that's already installed
   * @param excludeInstalledLanguages - boolean whether to exclude languages that are already installed
   * @return List<Keyboard>
   */
  private List<Keyboard> getKeyboards(JSONObject keyboardJSON, Keyboard baseKeyboard,
      boolean excludeInstalledLanguages ) {
    List<Keyboard> list = new ArrayList<Keyboard>();
    String packageID = baseKeyboard.getPackageID();
    String keyboardID = baseKeyboard.getKeyboardID();
    try {
      JSONArray languages = keyboardJSON.getJSONArray(PP_LANGUAGES_KEY);
      for (int j = 0; j < languages.length(); j++) {
        JSONObject language = languages.getJSONObject(j);
        String languageID = language.getString("id");
        String languageName = language.getString("name");
        if (!excludeInstalledLanguages ||
          !(KeyboardController.getInstance().keyboardExists(packageID, keyboardID, languageID))) {
          // Copy keyboard info and update language
          Keyboard newKeyboard = new Keyboard(baseKeyboard);
          newKeyboard.setLanguage(languageID, languageName);
          list.add(newKeyboard);
        }
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "getKeyboards() ", e);
    }
    Collections.sort(list, new LanguageNameSorter());
    return list;
  }

  /**
   * Get a list of available keyboard and language pairings that are available to add.
   * Keyboard list sorted by language name
   * (parses the kmp.json for the language list)
   * @param infoJSON - kmp.json as a JSON object
   * @param packageID - String of the package ID
   * @param keyboardID - String of the keyboard ID
   * @param tempPath - Boolean whether the kmp.json file is in a temporary location. If true,
   *                   don't use KeyboardController to determine a base keyboard.
   * @param excludeInstalledLanguages - Boolean whether to exclude
   *        installed languages from the returned list.
   * @return List of <Keyboard> based on kmp.json. If excludeInstalledLanguages is true, this list
   *         excludes languages already installed
   */
  public List<Keyboard> getKeyboardList(JSONObject infoJSON, String packageID, String keyboardID,
                                        boolean tempPath, boolean excludeInstalledLanguages) {
    List<Keyboard> list = new ArrayList<Keyboard>();
    try {
      JSONArray keyboards = infoJSON.getJSONArray(PP_KEYBOARDS_KEY);

      for (int i = 0; i < keyboards.length(); i++) {
        JSONObject keyboard = keyboards.getJSONObject(i);
        if (tempPath) {
          // temporary path for kmp.json so don't use KeyboardController to get the base keyboard.
          if (i==0) {
            // Create a "baseKeyboard" for the language picker menu. Don't need full keyboard info...
            Keyboard baseKeyboard = getKeyboard(infoJSON, packageID, 0);
            return getKeyboards(keyboard, baseKeyboard, excludeInstalledLanguages);
          }
        }
        else if (keyboardID != null && keyboardID.equals(keyboard.getString(PP_KEYBOARD_ID_KEY))) {
          // Find the keyboard already installed. We'll use it as the base
          // for creating new keyboards
          int baseKeyboardIndex =  KeyboardController.getInstance().getKeyboardIndex(packageID, keyboardID, "");
          if (baseKeyboardIndex == KeyboardController.INDEX_NOT_FOUND) {
            continue;
          }
          Keyboard baseKeyboard = KeyboardController.getInstance().getKeyboardInfo(baseKeyboardIndex);
          return getKeyboards(keyboard, baseKeyboard, excludeInstalledLanguages);
        }
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "getKeyboardList() ", e);
    }
    return list;
  }

  /**
   * Parses installed kmp.json to get a specified keyboard with an associated language. If languageID not
   * provided, return the keyboard with the first associated language.
   * This differs from getKeyboardList because this method doesn't search through
   * KeyboardController (installed keyboards)
   * @param packageID String of the package ID
   * @param keyboardID String of the keyboard ID
   * @param languageID String of the language ID
   * @return <Keyboard> object
   */
  public Keyboard getKeyboard(String packageID, String keyboardID, String languageID) {
    File packagePath = new File(resourceRoot, KMManager.KMDefault_AssetPackages + File.separator + packageID);
    Keyboard kbd = null;
    JSONObject infoJSON = loadPackageInfo(packagePath);
    String packageVersion = getPackageVersion(infoJSON);

    ArrayList<String> languageList = new ArrayList<String>();
    if (languageID != null && !languageID.isEmpty()) {
      languageList.add(languageID);
    }
    try {
      JSONArray keyboards = infoJSON.getJSONArray(PP_KEYBOARDS_KEY);

      for (int i=0; i < keyboards.length(); i++) {
        JSONObject keyboard = keyboards.getJSONObject(i);
        if (keyboardID.equals(keyboard.getString(PP_KEYBOARD_ID_KEY))) {
          Map<String, String>[] maps = processEntry(keyboard, packageID, packageVersion, languageList);
          if (maps != null) {
            // Only returning first keyboard map
            kbd = new Keyboard(
              maps[0].get(KMManager.KMKey_PackageID),
              maps[0].get(KMManager.KMKey_KeyboardID),
              maps[0].get(KMManager.KMKey_KeyboardName),
              maps[0].get(KMManager.KMKey_LanguageID),
              maps[0].get(KMManager.KMKey_LanguageName),
              maps[0].get(KMManager.KMKey_KeyboardVersion),
              maps[0].get(KMManager.KMKey_CustomHelpLink), // can be null
              "",
              false,
              maps[0].get(KMManager.KMKey_Font),
              maps[0].get(KMManager.KMKey_OskFont));
          }
        }
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "getKeyboard() ", e);
    }
    return kbd;
  }

  /**
   * The master KMP processing method; use after a .kmp download to fully install within the filesystem.
   * This will overwrite an existing package.
   * @param path Filepath of a newly downloaded .kmp file.
   * @param tempPath Filepath of temporarily extracted .kmp file
   * @param key String of jsonArray to iterate through ("keyboards" or "lexicalModels")
   * @param languageList List<String> of the preferred language ID to associate with the entry. If languageList is null or empty or
   *                   not found in kmp.json, then the first language will be added. The languageList is
   *                   only used for the first keyboard in a package.
   * @return A list of data maps of the newly installed and/or newly upgraded entries found in the package.
   * May be empty if the package file is actually an old version.
   *
   * The format for each map matches those of the current `download` method output of KMKeyboardDownloader
   * as closely as practical.
   * @throws IOException
   * @throws JSONException
   */
  public List<Map<String, String>> processKMP(File path, File tempPath, String key, ArrayList<String> languageList) throws IOException, JSONException {
    // Block reserved namespaces, like /cloud/.
    // TODO:  Consider throwing an exception instead?
    ArrayList<Map<String, String>> specs = new ArrayList<>();
    if (KMManager.isReservedNamespace(getPackageID(path))) {
      return specs;
    }
    if (path == null || tempPath == null) {
      return specs;
    }
    JSONObject newInfoJSON = loadPackageInfo(tempPath);
    if (newInfoJSON == null) {
      return specs;
    }
    String packageId = getPackageID(path);

    // For lexical model packages, lexical model version is determined by the package version
    // (Default to "1.0")
    String packageVersion = getPackageVersion(newInfoJSON);

    File permPath = constructPath(path, false);
    if (permPath.exists()) {
      // Out with the old.  "In with the new" is identical to a new package installation.
      FileUtils.deleteDirectory(permPath);
    }

    // No version conflict!  Proceed with the install!
    // Unfortunately, the nice recursive method provided by Apache Commons-IO's FileUtils class
    // isn't available at Android runtime.
    tempPath.renameTo(permPath);

//    How to retrieve other interesting bits of JSON, with pretty-printing:
//    System.out.println("System: " + json.getJSONObject("system").toString(2));
//    System.out.println("Options: " + json.getJSONObject("options").toString(2));
//    System.out.println("Info: " + json.getJSONObject("info").toString(2));
//    System.out.println("Files: " + json.getJSONArray("files").toString(2));
    if (newInfoJSON.has(key)) {
      JSONArray entries = newInfoJSON.getJSONArray(key);

      for (int i = 0; i < entries.length(); i++) {
        Map<String, String>[] maps;
        ArrayList<String> preferredLanguageList;
        JSONArray languages = entries.getJSONObject(i).getJSONArray(PP_LANGUAGES_KEY);
        if (languages == null || languages.length() == 0) {
          KMLog.LogError(TAG, packageId + " package has no languages to install");
          return specs;
        }
        if (i == 0 && key.equalsIgnoreCase(PP_KEYBOARDS_KEY) && languageList != null && !languageList.isEmpty()) {
          preferredLanguageList = languageList;
        } else {
          preferredLanguageList = new ArrayList<String>();

          if (key.equalsIgnoreCase(PP_LEXICAL_MODELS_KEY)) {
            // Use the entire language list for lexical model packages
            for (int j = 0; j < languages.length(); j++) {
              preferredLanguageList.add(languages.getJSONObject(j).getString("id"));
            }
          } else {
            // Just add the first language
            preferredLanguageList.add(languages.getJSONObject(0).getString("id"));
          }
        }

        maps = processEntry(entries.getJSONObject(i), packageId, packageVersion, preferredLanguageList);
        if (maps != null) {
          specs.addAll(Arrays.asList(maps));
        }
      }
    }  else {
      KMLog.LogError(TAG, path.getName() + " must contain \"" + key + "\"");
    }
    return specs;
  }

  public List<Map<String, String>> processKMP(File path, File tempPath, String key) throws IOException, JSONException {
    return processKMP(path, tempPath, key, null);
  }

  /**
   * Comparator to sort Keyboard list by language name
   */
  private class LanguageNameSorter implements Comparator<Keyboard> {
    @Override
    public int compare(Keyboard k1, Keyboard k2) {
      return k1.getLanguageName().compareTo(k2.getLanguageName());
    }
  }

}
