package com.tavultesoft.kmea.packages;

import androidx.annotation.NonNull;
import android.util.Log;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.JSONParser;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.ZipUtils;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Created by joshua on 12/7/2017.
 */

/**
 * A class for use in handling Keyman's .kmp package file format, as it relates to the
 * KMEA engine.  This is primarily for installing keyboard packages.
 */
public class PackageProcessor {
  protected File resourceRoot = null;

  public static final String PP_DEFAULT_VERSION = "1.0";
  public static final String PP_DEFAULT_METADATA = "kmp.json";

  public static final String PP_TARGET_INVALID = "invalid";
  public static final String PP_TARGET_KEYBOARDS = "keyboards";
  public static final String PP_TARGET_LEXICAL_MODELS = "lexicalModels";

  // keys in kmp.json
  public static final String PP_KEYBOARDS_KEY = "keyboards";
  public static final String PP_LEXICAL_MODELS_KEY = "lexicalModels";

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
    String filename = path.getName().toLowerCase();

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
      Log.e(TAG, "kmp.json does not exist");
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
   * @param languageID Preferred language ID to associate with the entry. If not found,
   *                   the first language in kmp.json is used.
   * @param isCustom Boolean if package is custom ad-hoc install
   * @return A list of maps defining one keyboard-language pairing each.
   * @throws JSONException
   */
  public Map<String, String>[] processEntry(JSONObject jsonEntry, String packageId, String packageVersion,
                                            String languageID, boolean isCustom) throws JSONException {
    JSONArray languages = jsonEntry.getJSONArray("languages");
    String isCustomStr = isCustom ? "Y" : "N";

    String keyboardId = jsonEntry.getString("id");
    if (touchKeyboardExists(packageId, keyboardId)) {
      HashMap<String, String>[] keyboards = new HashMap[1];
      int i=0;
      keyboards[i] = new HashMap<>();
      keyboards[i].put(KMManager.KMKey_PackageID, packageId);
      keyboards[i].put(KMManager.KMKey_KeyboardName, jsonEntry.getString("name"));
      keyboards[i].put(KMManager.KMKey_KeyboardID, jsonEntry.getString("id"));

      int languageIndex = JSONUtils.findID(languages, languageID);
      if (languageIndex == -1) {
        // languageID not found so use first language
        languageIndex = 0;
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

      keyboards[i].put(KMManager.KMKey_CustomKeyboard, isCustomStr);
      if (welcomeExists(packageId)) {
        File kmpFile = new File(packageId + ".kmp");
        File packageDir = constructPath(kmpFile, false);
        File welcomeFile = new File(packageDir, "welcome.htm");
        // Only storing relative instead of absolute paths as a convenience for unit tests.
        keyboards[i].put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
      }

      return keyboards;
    } else {
      return null;
    }
  }

  public static String getKeyboardVersion(JSONObject json, String kbdId) throws JSONException {
    JSONArray keyboards = json.getJSONArray("keyboards");

    for(int i=0; i < keyboards.length(); i++) {
      JSONObject keyboard = keyboards.getJSONObject(i);

      if(keyboard.getString("id").equals(kbdId)) {
        return keyboard.getString("version");
      }
    }

    return null;
  }

  /**
   * Simply extracts the package's name.
   * @param json The metadata JSONObject for the package.
   * @return The package name (via String)
   */
  public String getPackageName(JSONObject json) {
    try {
      return json.getJSONObject("info").getJSONObject("name").getString("description");
    } catch (Exception e) {
      return "";
    }
  }

  /**
   * Simply extracts the package's version number.
   * If undefined, return default version "1.0"
   * @param json The metadata JSONObject for the package.
   * @return The version number (via String)
   */
  public static String getPackageVersion(JSONObject json) {
    try {
      return json.getJSONObject("info").getJSONObject("version").getString("description");
    } catch (Exception e) {
      return PP_DEFAULT_VERSION;
    }
  }

  /**
   * Simply extracts the package's target (keyboards vs lexical models).
   * Only one can be valid. Otherwise, return "invalid"
   * @param json The metadata JSONObject for the package.
   * @return String The target ("keyboards", "lexicalModels", or "invalid")
   */
  public String getPackageTarget(JSONObject json) {
    try {
      if (json.has(PP_KEYBOARDS_KEY) && !json.has(PP_LEXICAL_MODELS_KEY)) {
        return PP_TARGET_KEYBOARDS;
      } else if (json.has(PP_LEXICAL_MODELS_KEY) && !json.has(PP_KEYBOARDS_KEY)) {
        return PP_TARGET_LEXICAL_MODELS;
      } else {
        return PP_TARGET_INVALID;
      }
    } catch (Exception e) {
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
      String target = getPackageTarget(loadPackageInfo(tempPath));
      FileUtils.deleteDirectory(tempPath);
      return target;
    } catch (Exception e) {
      return PP_TARGET_INVALID;
    }
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
   * Determines if installing the .kmp would result in a downgrade.  For testing only.
   * @param kmpPath The path to the .kmp to be installed.
   * @param preExtracted Indicates use of a temporary testing 'kmp' that is pre-extracted.
   * @return <code><b>true</b></code> if installing would be a downgrade, otherwise <code><b>false</b></code>.
   * @throws IOException
   * @throws JSONException
   */
  boolean isDowngrade(File kmpPath, boolean preExtracted) throws IOException, JSONException {
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
      if (files.length > 0) {
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
   * The master KMP processing method; use after a .kmp download to fully install within the filesystem.
   * This will overwrite an existing package.
   * @param path Filepath of a newly downloaded .kmp file.
   * @param tempPath Filepath of temporarily extracted .kmp file
   * @param key String of jsonArray to iterate through ("keyboards" or "lexicalModels")
   * @param languageID String of the preferred language ID to associate with the entry. If languageID is null or
   *                   not found in kmp.json, then the first entry will be added.
   * @param isCustom Boolean if custom ad-hoc install
   * @return A list of data maps of the newly installed and/or newly upgraded entries found in the package.
   * May be empty if the package file is actually an old version.
   * <br/><br/>
   * The format for each map matches those of the current `download` method output of KMKeyboardDownloader
   * as closely as practical.
   * @throws IOException
   * @throws JSONException
   */
  public List<Map<String, String>> processKMP(File path, File tempPath, String key,
                                              String languageID, boolean isCustom) throws IOException, JSONException {
    // Block reserved namespaces, like /cloud/.
    // TODO:  Consider throwing an exception instead?
    ArrayList<Map<String, String>> specs = new ArrayList<>();
    if (KMManager.isReservedNamespace(getPackageID(path))) {
      return specs;
    }
    JSONObject newInfoJSON = loadPackageInfo(tempPath);
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
        Map<String, String>[] maps = processEntry(entries.getJSONObject(i), packageId, packageVersion,
          languageID, isCustom);
        if (maps != null) {
          specs.addAll(Arrays.asList(maps));
        }
      }
    }  else {
      Log.e(TAG, path.getName() + " must contain \"" + key + "\"");
    }
    return specs;
  }

  public List<Map<String, String>> processKMP(File path, File tempPath, String key,
                                              boolean isCustom) throws IOException, JSONException {
    return processKMP(path, tempPath, key, null, isCustom);
  }
}
