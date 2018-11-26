package com.tavultesoft.kmea.packages;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
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
 * A class of static methods for use in handling Keyman's .kmp file format, as it relates to the
 * KMEA engine.  Only `processKMP` and `initialize` should see regular use; the rest are helper
 * methods designed to facilitate unit testing for this class's functionality.
 */
public class PackageProcessor {
  private static File resourceRoot = null;
  public static final String PPDefault_Version = "1.0";

  public static void initialize(File resourceRoot) {
    PackageProcessor.resourceRoot = resourceRoot;
  }

  public static String getPackageID(File path) {
    String filename = path.getName();
    String kmpBaseName;

    if(resourceRoot == null) {
      throw new IllegalStateException("The PackageProcessor has not been initialized!");
    }

    if(filename.lastIndexOf('.') != -1) {
      // Android's temp downloads attach a suffix to the extension; .kmp isn't the end of the filename.
      if(filename.lastIndexOf(".kmp") == -1) {
        throw new IllegalArgumentException("Invalid file passed to the KMP unpacker!");
      }

      // Extract our best-guess name for the package and construct the temporary package name.
      return filename.substring(0, filename.lastIndexOf(".kmp"));
    } else {
      throw new IllegalArgumentException("Invalid file passed to the KMP unpacker!");
    }
  }

  // A default, managed mapping for package installation, handling both temp directory
  // and perm directory locations.  No need to relocate the downloaded .kmp file itself.
  @NonNull
  static File constructPath(File path, boolean temp) {
    String kmpBaseName = getPackageID(path);
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
  public static File unzipKMP(File path) throws IOException {
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
  static JSONObject loadPackageInfo(File packagePath) {
    File infoFile = new File(packagePath, "kmp.json");

    if(infoFile.exists()) {
      JSONParser parser = new JSONParser();
      return parser.getJSONObjectFromFile(infoFile);
    } else {
      Log.e("PackageProcessor", infoFile.toString() + " does not exist.");
      return null;
    }
  }

  // Call this once per each entry of the JSON `keyboards` array, then concatenate the resulting arrays for a full list.

  /**
   * Generates a list of keyboard data maps designed to mirror the `download` method output of
   * KMKeyboardDownloader as closely as practical.
   * @param jsonKeyboard  One entry of the master JSONArray of the top-level "keyboards" property.
   * @return A list of maps defining one keyboard-language pairing each.
   * @throws JSONException
   */
  public static Map<String, String>[] processKeyboardsEntry(JSONObject jsonKeyboard, String packageId) throws JSONException {
    JSONArray languages = jsonKeyboard.getJSONArray("languages");


    String keyboardId = jsonKeyboard.getString("id");
    if (touchKeyboardExists(packageId, keyboardId)) {
      HashMap<String, String>[] keyboards = new HashMap[languages.length()];

      for (int i = 0; i < languages.length(); i++) {
        keyboards[i] = new HashMap<>();
        keyboards[i].put(KMManager.KMKey_PackageID, packageId);
        keyboards[i].put(KMManager.KMKey_KeyboardName, jsonKeyboard.getString("name"));
        keyboards[i].put(KMManager.KMKey_KeyboardID, jsonKeyboard.getString("id"));
        keyboards[i].put(KMManager.KMKey_LanguageID, languages.getJSONObject(i).getString("id"));
        keyboards[i].put(KMManager.KMKey_LanguageName, languages.getJSONObject(i).getString("name"));
        keyboards[i].put(KMManager.KMKey_KeyboardVersion, jsonKeyboard.getString("version"));
        if (jsonKeyboard.has("displayFont")) {
          keyboards[i].put(KMManager.KMKey_Font, jsonKeyboard.getString("displayFont"));
        }
        if (jsonKeyboard.has("oskFont")) {
          keyboards[i].put(KMManager.KMKey_OskFont, jsonKeyboard.getString("oskFont"));
        }

        // For now, all KMP distributed keyboards are custom
        keyboards[i].put(KMManager.KMKey_CustomKeyboard, "Y");
        if (welcomeExists(packageId)) {
          File kmpFile = new File(packageId + ".kmp");
          File packageDir = constructPath(kmpFile, false);
          File welcomeFile = new File(packageDir, "welcome.htm");
          // Only storing relative instead of absolute paths as a convenience for unit tests.
          keyboards[i].put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
        }
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
   * @param json The metadata JSONOBject for the package.
   * @return The package name (via String)
   * @throws JSONException
   */
  public static String getPackageName(JSONObject json) throws JSONException {
    if (json == null) {
      return null;
    } else {
      return json.getJSONObject("info").getJSONObject("name").getString("description");
    }
  }

  public static String getPackageName(File kmpPath, boolean installed) {
    try {
      if (installed) {
        return getPackageName(loadPackageInfo(constructPath(kmpPath, false)));
      } else {
        File tempPath = unzipKMP(kmpPath);
        String name = getPackageName(loadPackageInfo(tempPath));
        FileUtils.deleteDirectory(tempPath);
        return name;
      }
    } catch (Exception e) {
      // Developer will never allow package name to be undefined, but just in case, reuse package ID
      return getPackageID(kmpPath);
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
    } catch (JSONException e) {
      return PPDefault_Version;
    }
  }

  public static String getPackageVersion(File kmpPath, boolean installed) {
    try {
      if (installed) {
        return getPackageVersion(loadPackageInfo(constructPath(kmpPath, false)));
      } else {
        File tempPath = unzipKMP(kmpPath);
        String version = getPackageVersion(loadPackageInfo(tempPath));
        FileUtils.deleteDirectory(tempPath);
        return version;
      }
    } catch (Exception e) {
      return PPDefault_Version;
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
  public static int comparePackageDirectories(File newPath, File oldPath) throws IOException, JSONException {
    JSONObject newInfoJSON = loadPackageInfo(newPath);
    String newVersion = getPackageVersion(newInfoJSON);

    if(oldPath.exists()) {
      JSONObject oldInfoJSON = loadPackageInfo(oldPath);
      String originalVersion = getPackageVersion(oldInfoJSON);

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
   * Determines if installing the .kmp would result in a downgrade.
   * @param kmpPath The path to the .kmp to be installed.
   * @return <code><b>true</b></code> if installing would be a downgrade, otherwise <code><b>false</b></code>.
   * @throws IOException
   * @throws JSONException
   */
  public static boolean isDowngrade(File kmpPath) throws IOException, JSONException {
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
  static boolean isDowngrade(File kmpPath, boolean preExtracted) throws IOException, JSONException {
    return internalCompareKMPVersion(kmpPath, preExtracted,-1);
  }

  /**
   * Determines if installing the .kmp would result in a downgrade.  For testing only.
   * @param kmpPath The path to the .kmp to be installed.
   * @return <code><b>true</b></code> if installing would be a downgrade, otherwise <code><b>false</b></code>.
   * @throws IOException
   * @throws JSONException
   */
  public static boolean isSameVersion(File kmpPath) throws IOException, JSONException {
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
  static boolean isSameVersion(File kmpPath, boolean preExtracted) throws IOException, JSONException {
    return internalCompareKMPVersion(kmpPath, preExtracted,0);
  }

  static boolean internalCompareKMPVersion(File kmpPath, boolean preExtracted, int compValue) throws IOException, JSONException {
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

  static boolean touchKeyboardExists(final String packageId, final String keyboardId) {
    if (resourceRoot != null) {
      FileFilter touchKeyboardFilter = new FileFilter() {
        @Override
        public boolean accept(File pathname) {
          if (pathname.isFile() && pathname.getName().startsWith(keyboardId) && pathname.getName().endsWith(".js")) {
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

  static boolean welcomeExists(final String packageId) {
    if (resourceRoot != null) {
      FileFilter welcomeFilter = new FileFilter() {
        @Override
        public boolean accept(File pathname) {
          if (pathname.isFile() && pathname.getName().equals("welcome.htm")) {
            return true;
          }
          return false;
        }
      };

      File kmpFile = new File(packageId + ".kmp");
      File packageDir = constructPath(kmpFile, false);
      File[] files = packageDir.listFiles(welcomeFilter);
      if (files.length > 0) {
        return true;
      }
    }

    return false;
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
    return processKMP(path, false);
  }

  /**
   * The master KMP processing method; use after a .kmp download to fully install within the filesystem.
   * @param path Filepath of a newly downloaded .kmp file.
   * @param force A <code><b>true</b></code> value indicates that attempts to downgrade should proceed/be forced.
   * @return A list of data maps of the newly installed and/or newly upgraded keyboards found in the package.
   * May be empty if the package file is actually an old version.
   * <br/><br/>
   * The format for each map matches those of the current `download` method output of KMKeyboardDownloader
   * as closely as practical.
   * @throws IOException
   * @throws JSONException
   */
  public static List<Map<String, String>> processKMP(File path, boolean force) throws IOException, JSONException {
    return processKMP(path, force, false);
  }

  /**
   * The master KMP processing method; use after a .kmp download to fully install within the filesystem.
   * @param path Filepath of a newly downloaded .kmp file.
   * @param force A <code><b>true</b></code> value indicates that attempts to downgrade should proceed/be forced.
   * @param preExtracted Indicates that the specified file has already been unzipped.  Only of use for testing.
   * @return A list of data maps of the newly installed and/or newly upgraded keyboards found in the package.
   * May be empty if the package file is actually an old version.
   * <br/><br/>
   * The format for each map matches those of the current `download` method output of KMKeyboardDownloader
   * as closely as practical.
   * @throws IOException
   * @throws JSONException
   */
  static List<Map<String, String>> processKMP(File path, boolean force, boolean preExtracted) throws IOException, JSONException {
    // Block reserved namespaces, like /cloud/.
    // TODO:  Consider throwing an exception instead?
    if(KMManager.isReservedNamespace(getPackageID(path))) {
      return new ArrayList<>();
    }
    File tempPath;
    if(!preExtracted) {
      tempPath = unzipKMP(path);
    } else {
      tempPath = constructPath(path, true);
    }
    JSONObject newInfoJSON = loadPackageInfo(tempPath);
    String packageId = getPackageID(path);

    File permPath = constructPath(path, false);
    if(permPath.exists()) {
      if(!force && comparePackageDirectories(tempPath, permPath) != 1) {
        // Abort!  The current installation is newer or as up-to-date.
        FileUtils.deleteDirectory(tempPath);
        return new ArrayList<>();  // It's empty; avoids dealing directly with null ptrs.
      } else {
        // Out with the old.  "In with the new" is identical to a new package installation.
        FileUtils.deleteDirectory(permPath);
      }
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

    // newInfoJSON holds all the newly downloaded/updated keyboard data.
    JSONArray keyboards = newInfoJSON.getJSONArray("keyboards");
    ArrayList<Map<String, String>> keyboardSpecs = new ArrayList<>();

    for(int i=0; i < keyboards.length(); i++) {
      Map<String, String>[] kbds = processKeyboardsEntry(keyboards.getJSONObject(i), packageId);
      if (kbds != null) {
        keyboardSpecs.addAll(Arrays.asList(kbds));
      }
    }

    return keyboardSpecs;
  }
}
