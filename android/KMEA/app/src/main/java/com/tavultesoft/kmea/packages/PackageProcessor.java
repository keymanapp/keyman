package com.tavultesoft.kmea.packages;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.JSONParser;
import com.tavultesoft.kmea.util.ZipUtils;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Created by joshua on 12/7/2017.
 */

/**
 * A class of static methods for use in handling Keyman's .kmp file format, as it relates to the
 * KMEA engine.  Only `processKMP` and `initialize` should see regular use; the rest are helper
 * methods designed to facilitate unit testing for this class's functionality.
 */
public class PackageProcessor {
  private static File resourceRoot = null;

  public static void initialize(File resourceRoot) {
    PackageProcessor.resourceRoot = resourceRoot;
  }

  // A default, managed mapping for package installation, handling both temp directory
  // and perm directory locations.  No need to relocate the downloaded .kmp file itself.
  @NonNull
  static File constructPath(File path, boolean temp) {
    String filename = path.getName();
    String kmpBaseName;

    if(resourceRoot == null) {
      throw new IllegalStateException("The PackageProcessor has not been initialized!");
    }

    if(filename.lastIndexOf('.') != -1) {
      String ext = "";
      ext = filename.substring(filename.lastIndexOf('.'));

      if(!ext.toLowerCase().equals(".kmp")) {
        throw new IllegalArgumentException("Invalid file passed to the KMP unpacker!");
      }
    } else {
      throw new IllegalArgumentException("Invalid file passed to the KMP unpacker!");
    }

    // Extract our best-guess name for the package and construct the temporary package name.
    kmpBaseName = filename.substring(0, filename.lastIndexOf('.'));
    // Feel free to change this as desired - simply ensure it is unique enough to never be used as
    // a legitimate package name.
    String kmpFolderName = temp ? "." + kmpBaseName + ".temp" : kmpBaseName;

    return new File(resourceRoot,KMManager.KMDefault_AssetPackages + File.separator + kmpFolderName + File.separator);
  }

  /**
   * Unzips the package.kmp file to its mapped temporary directory location.
   * @param path The file path of the .kmp file, file name included.
   * @return The mapped temporary file path for the .kmp file's contents.
   * @throws IOException
   */
  static File unzipKMP(File path) throws IOException {
    File tempKeyboardPath = constructPath(path, true);
    ZipUtils.unzip(path, tempKeyboardPath);

    return tempKeyboardPath;
  }

  /**
   * Given a directory location for an extracted KMP file, extracts its kmp.json information
   * into a JSON object.  Works on temporary directories and the installed package directory.
   * @param packagePath The extracted location information to retrieve information for.
   * @return A metadata JSONObject for the package version.
   */
  static JSONObject loadPackageInfo(File packagePath) {
    File infoFile = new File(packagePath, "kmp.json");

    JSONParser parser = new JSONParser();
    return parser.getJSONObjectFromFile(infoFile);
  }

  // Call this once per each entry of the JSON `keyboards` array, then concatenate the resulting arrays for a full list.

  /**
   * Generates a list of keyboard data maps designed to mirror the `download` method output of
   * KMKeyboardDownloader as closely as practical.
   * @param jsonKeyboard  One entry of the master JSONArray of the top-level "keyboards" property.
   * @return A list of maps defining one keyboard-language pairing each.
   * @throws JSONException
   */
  public static Map<String, String>[] processKeyboardsEntry(JSONObject jsonKeyboard) throws JSONException {
    JSONArray languages = jsonKeyboard.getJSONArray("languages");

    HashMap<String, String>[] keyboards = new HashMap[languages.length()];

    for(int i=0; i < languages.length(); i++) {
      keyboards[i] = new HashMap<>();
      keyboards[i].put(KMManager.KMKey_KeyboardName, jsonKeyboard.getString("name"));
      keyboards[i].put(KMManager.KMKey_KeyboardID, jsonKeyboard.getString("id"));
      keyboards[i].put(KMManager.KMKey_LanguageID, languages.getJSONObject(i).getString("id"));
      keyboards[i].put(KMManager.KMKey_LanguageName, languages.getJSONObject(i).getString("name"));
      keyboards[i].put(KMManager.KMKey_KeyboardVersion, jsonKeyboard.getString("version"));
      keyboards[i].put(KMManager.KMKey_Font, jsonKeyboard.getString("displayFont"));
      if (jsonKeyboard.has("oskFont")) {
        keyboards[i].put(KMManager.KMKey_OskFont, jsonKeyboard.getString("oskFont"));
      }
    }

    return keyboards;
  }

  /**
   * Simply extracts the package's version number.
   * @param json The metadata JSONObject for the package.
   * @return The version number (via String)
   * @throws JSONException
   */
  public static String getVersion(JSONObject json) throws JSONException {
    return json.getJSONObject("system").getString("fileVersion");
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
  public static int comparePackageDirectories(File newPath, File oldPath) throws IOException, JSONException {
    JSONObject newInfoJSON = loadPackageInfo(newPath);
    String newVersion = getVersion(newInfoJSON);

    if(oldPath.exists()) {
      JSONObject oldInfoJSON = loadPackageInfo(oldPath);
      String originalVersion = getVersion(oldInfoJSON);

      if(KMManager.compareVersions(newVersion, originalVersion) == 1) {
        return 1;
      } else if(KMManager.compareVersions(newVersion, originalVersion) == 0){
        return 0;
      } else {
        return -1;
      }
    } else {
      return 1;
    }
  }

  /**
   * The master KMP processing method; use after a .kmp download to fully install within the filesystem.
   * @param path Filepath of a newly downloaded .kmp file.
   * @return A list of data maps of the newly installed and/or newly upgraded keyboards found in the package.
   * May be empty if the package file is actually an old version.
   * <br/><br/>
   * The format for each map matches those of the current `download` method output of KMKeyboardDownloader
   * as closely as practical.
   * @throws IOException
   * @throws JSONException
   */
  public static List<Map<String, String>> processKMP(File path) throws IOException, JSONException {
    File tempPath = unzipKMP(path);
    JSONObject newInfoJSON = loadPackageInfo(tempPath);

    File permPath = constructPath(path, false);
    if(permPath.exists()) {
      if(comparePackageDirectories(tempPath, permPath) != -1) {
        // Abort!  The current installation is newer or as up-to-date.
        FileUtils.deleteDirectory(tempPath);
        return new ArrayList<Map<String, String>>();  // It's empty; avoids dealing directly with null ptrs.
      } else {
        // Out with the old.  "In with the new" is identical to a new package installation.
        FileUtils.deleteDirectory(permPath);
      }
    }

    // No version conflict!  Proceed with the install!
    // A nice, recursive method provided by Apache Commons-IO's FileUtils class.
    FileUtils.moveDirectory(tempPath, permPath);


//    How to retrieve other interesting bits of JSON, with pretty-printing:
//    System.out.println("System: " + json.getJSONObject("system").toString(2));
//    System.out.println("Options: " + json.getJSONObject("options").toString(2));
//    System.out.println("Info: " + json.getJSONObject("info").toString(2));
//    System.out.println("Files: " + json.getJSONArray("files").toString(2));

    // newInfoJSON holds all the newly downloaded/updated keyboard data.
    JSONArray keyboards = newInfoJSON.getJSONArray("keyboards");
    ArrayList<Map<String, String>> keyboardSpecs = new ArrayList<>();

    for(int i=0; i < keyboards.length(); i++) {
      Map<String, String>[] kbds = processKeyboardsEntry(keyboards.getJSONObject(i));

      keyboardSpecs.addAll(Arrays.asList(kbds));
    }

    return keyboardSpecs;
  }
}
