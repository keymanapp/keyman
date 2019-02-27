package com.tavultesoft.kmea.packages;

import android.util.Log;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.ZipUtils;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * An extension of PackageProcessor
 * A class of static methods for use in handling Keyman's .kmp file format, as it relates to the
 * KMEA Engine. This is primarily for installing lexical model packages.
 */
public class LexicalModelPackageProcessor extends PackageProcessor {
  public static final String KMPPDefault_Target = "lexicalModels";

  private static final String TAG = "LMPackageProcessor";

  static File constructPath(File path, boolean temp) {
    String kmpBaseName = getPackageID(path);
    // Feel free to change this as desired - simply ensure it is unique enough to never be used as
    // a legitimate package name.
    String kmpFolderName = temp ? "." + kmpBaseName + ".temp" : kmpBaseName;

    return new File(resourceRoot, KMManager.KMDefault_LexicalModelPackages + File.separator + kmpFolderName + File.separator);
  }

  /**
   * Unzips the package.kmp file to its mapped temporary directory location.
   * @param path The file path of the .kmp file, file name included.
   * @return The mapped temporary file path for the .kmp file's contents.
   * @throws IOException
   */
  public static File unzipKMP(File path) throws IOException {
    File tempModelPath = constructPath(path, true);
    if (!tempModelPath.exists()) {
      tempModelPath.mkdir();
    }
    ZipUtils.unzip(path, tempModelPath);

    return tempModelPath;
  }

  public static Map<String, String>[] processLexicalModelsEntry(JSONObject jsonModel, String packageId) throws JSONException {
    HashMap<String, String>[] models = new HashMap[1];
    return models;
  }

  /**
   * The master KMP processing method; use after a .model.kmp download to fully install within the filesystem.
   * @param path Filepath of a newly downloaded .kmp file.
   * @return A list of data maps of the newly installed and/or newly upgraded lexical models found in the package.
   * May be empty if the package file is actually an old version.
   * <br/><br/>
   * @throws IOException
   * @throws JSONException
   */
  public static List<Map<String, String>> processKMP(File path) throws IOException, JSONException {
    return processKMP(path, false);
  }

  /**
   * The master KMP processing method; use after a .model.kmp download to fully install within the filesystem.
   * @param path Filepath of a newly downloaded .kmp file.
   * @param force A <code><b>true</b></code> value indicates that attempts to downgrade should proceed/be forced.
   * @return A list of data maps of the newly installed and/or newly upgraded lexical models found in the package.
   * May be empty if the package file is actually an old version.
   * <br/><br/>
   * @throws IOException
   * @throws JSONException
   */
  public static List<Map<String, String>> processKMP(File path, boolean force) throws IOException, JSONException {
    return processKMP(path, force, false);
  }

  /**
   * The master KMP processing method; use after a .model.kmp download to fully install within the filesystem.
   * @param path Filepath of a newly downloaded .kmp file.
   * @param force A <code><b>true</b></code> value indicates that attempts to downgrade should proceed/be forced.
   * @param preExtracted Indicates that the specified file has already been unzipped.  Only of use for testing.
   * @return A list of data maps of the newly installed and/or newly upgraded lexical models found in the package.
   * May be empty if the package file is actually an old version.
   * <br/><br/>
   * @throws IOException
   * @throws JSONException
   */
  static List<Map<String, String>> processKMP(File path, boolean force, boolean preExtracted) throws IOException, JSONException {
    // Block reserved namespaces, like /cloud/.
    // TODO:  Consider throwing an exception instead?
    ArrayList<Map<String, String>> specs = new ArrayList<>();
    if (KMManager.isReservedNamespace(getPackageID(path))) {
      return specs;
    }
    File tempPath;
    if (!preExtracted) {
      tempPath = unzipKMP(path);
    } else {
      tempPath = constructPath(path, true);
    }
    JSONObject newInfoJSON = loadPackageInfo(tempPath);
    String packageId = getPackageID(path);

    File permPath = constructPath(path, false);
    if (permPath.exists()) {
      if (!force && comparePackageDirectories(tempPath, permPath) != 1) {
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

    boolean hasKeyboards = newInfoJSON.has("keyboards");
    boolean hasLexicalModels = newInfoJSON.has("lexicalModels");
    if (hasLexicalModels && !hasKeyboards) {

    } else {
      Log.e(TAG, path.getName() + " must contain \"lexicalModels\"");
    }

    return specs;
  }

}
