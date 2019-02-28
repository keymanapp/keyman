package com.tavultesoft.kmea.packages;

import android.content.Context;
import android.util.Log;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.ZipUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * An extension of PackageProcessor
 * A class of static methods for use in handling Keyman's .kmp file format, as it relates to the
 * KMEA Engine. This is primarily for installing lexical model packages.
 */
public class LexicalModelPackageProcessor extends PackageProcessor {
  private static final String TAG = "LMPackageProcessor";

  public LexicalModelPackageProcessor(File resourceRoot) {
    super(resourceRoot);
  }

  public LexicalModelPackageProcessor(Context context) {
    super(context);
  }

  public File constructPath(File path, boolean temp) {
    String kmpBaseName = getPackageID(path);
    // Feel free to change this as desired - simply ensure it is unique enough to never be used as
    // a legitimate package name.
    String kmpFolderName = temp ? "." + kmpBaseName + ".temp" : kmpBaseName;

    if (temp) {
      return new File(resourceRoot, KMManager.KMDefault_LexicalModelPackages + File.separator + kmpFolderName + File.separator);
    }

    return new File(resourceRoot, KMManager.KMDefault_LexicalModelPackages + File.separator + kmpFolderName + File.separator);
  }

  /**
   * Unzips the package.kmp file to its mapped temporary directory location.
   * @param path The file path of the .kmp file, file name included.
   * @return The mapped temporary file path for the .kmp file's contents.
   * @throws IOException
   */
    /*
  public static File unzipKMP(File path) throws IOException {
    return PackageProcessor.unzipKMP(path);
    File tempModelPath = constructPath(path, true);
    if (!tempModelPath.exists()) {
      tempModelPath.mkdir();
    }
    ZipUtils.unzip(path, tempModelPath);

    return tempModelPath;
  }
    */

  private boolean lexicalModelExists(final String packageId, final String modelId) {
    if (resourceRoot != null) {
      FileFilter lexicalModelFilter = new FileFilter() {
        @Override
        public boolean accept(File pathname) {
          if (pathname.isFile() && pathname.getName().startsWith(modelId) && FileUtils.hasLexicalModelExtension(pathname.getName())) {
            return true;
          }
          return false;
        }
      };

      File kmpFile = new File(packageId + ".kmp");
      if (!kmpFile.exists()) {
        kmpFile = new File(packageId + ".model.kmp");
      }
      File packageDir = constructPath(kmpFile, false);
      File[] files = packageDir.listFiles(lexicalModelFilter);
      if (files.length > 0) {
        return true;
      }
    }

    return false;
  }

  public Map<String, String>[] processEntry(JSONObject jsonEntry, String packageId) throws JSONException {
    JSONArray languages = jsonEntry.getJSONArray("languages");

    String modelId = jsonEntry.getString("id");
    if (lexicalModelExists(packageId, modelId)) {
      HashMap<String, String>[] models = new HashMap[languages.length()];

      for (int i = 0; i < languages.length(); i++) {
        models[i] = new HashMap<>();
        models[i].put(KMManager.KMKey_PackageID, packageId);
        models[i].put(KMManager.KMKey_LexicalModelName, jsonEntry.getString("name"));
        models[i].put(KMManager.KMKey_LexicalModelID, jsonEntry.getString("id"));
        models[i].put(KMManager.KMKey_LexicalModelVersion, jsonEntry.getString("version"));
        models[i].put(KMManager.KMKey_LanguageID, languages.getJSONObject(i).getString("id"));
        models[i].put(KMManager.KMKey_LanguageName, languages.getJSONObject(i).getString("name"));

        if (welcomeExists(packageId)) {
          File kmpFile = new File(packageId + ".kmp");
          if (!kmpFile.exists()) {
            kmpFile = new File(packageId + ".model.kmp");
          }
          File packageDir = constructPath(kmpFile, false);
          File welcomeFile = new File(packageDir, "welcome.htm");
          // Only storing relative instead of absolute paths as a convenience for unit tests.
          models[i].put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
        }
      }
      return models;
    } else {
      return null;
    }
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
  public List<Map<String, String>> processKMP(File path) throws IOException, JSONException {
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
  public List<Map<String, String>> processKMP(File path, boolean force) throws IOException, JSONException {
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
  public List<Map<String, String>> processKMP(File path, boolean force, boolean preExtracted) throws IOException, JSONException {
    // Block reserved namespaces, like /cloud/.
    // TODO:  Consider throwing an exception instead?
    ArrayList<Map<String, String>> lexicalModelSpecs = new ArrayList<>();
    if (KMManager.isReservedNamespace(getPackageID(path))) {
      return lexicalModelSpecs;
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

    // Verify newInfoJSON has "lexicalModels" and not "keyboards"
    if (newInfoJSON.has(PackageProcessor.PP_LEXICAL_MODELS_KEY) && !newInfoJSON.has(PackageProcessor.PP_KEYBOARDS_KEY)) {
       JSONArray lexicalModels = newInfoJSON.getJSONArray(PP_LEXICAL_MODELS_KEY);

      for (int i = 0; i < lexicalModels.length(); i++) {
        Map<String, String>[] lms = processEntry(lexicalModels.getJSONObject(i), packageId);
        if (lms != null) {
          lexicalModelSpecs.addAll(Arrays.asList(lms));
        }
      }
    } else {
      Log.e(TAG, path.getName() + " must contain \"lexicalModels\"");
    }

    return lexicalModelSpecs;
  }

}
