package com.keyman.engine.packages;

import com.keyman.engine.KMManager;
import com.keyman.engine.util.FileUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileFilter;
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
  private static final String TAG = "LMPackageProcessor";

  public LexicalModelPackageProcessor(File resourceRoot) {
    super(resourceRoot);
  }

  protected File constructPath(File path, boolean temp) {
    String kmpBaseName = getPackageID(path);
    // Feel free to change this as desired - simply ensure it is unique enough to never be used as
    // a legitimate package name.
    String kmpFolderName = temp ? "." + kmpBaseName + ".temp" : kmpBaseName;

    return new File(resourceRoot, KMManager.KMDefault_LexicalModelPackages + File.separator + kmpFolderName + File.separator);
  }

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
      File packageDir = constructPath(kmpFile, false);
      File[] files = packageDir.listFiles(lexicalModelFilter);
      if (files != null && files.length > 0) {
        return true;
      }
    }

    return false;
  }

  public Map<String, String>[] processEntry(JSONObject jsonEntry, String packageId, String packageVersion, ArrayList<String> languageList) throws JSONException {
    JSONArray languages = jsonEntry.getJSONArray("languages");

    String modelId = jsonEntry.getString("id");
    if (lexicalModelExists(packageId, modelId)) {
      HashMap<String, String>[] models = new HashMap[languages.length()];

      for (int i = 0; i < languages.length(); i++) {
        models[i] = new HashMap<>();
        models[i].put(KMManager.KMKey_PackageID, packageId);
        models[i].put(KMManager.KMKey_LexicalModelName, jsonEntry.getString("name"));
        models[i].put(KMManager.KMKey_LexicalModelID, jsonEntry.getString("id"));
        // Use package version for the lexical model version
        models[i].put(KMManager.KMKey_LexicalModelVersion, packageVersion);
        models[i].put(KMManager.KMKey_LanguageID, languages.getJSONObject(i).getString("id").toLowerCase());
        models[i].put(KMManager.KMKey_LanguageName, languages.getJSONObject(i).getString("name"));

        if (welcomeExists(packageId)) {
          File kmpFile = new File(packageId + ".kmp");
          File packageDir = constructPath(kmpFile, false);
          File welcomeFile = new File(packageDir, "welcome.htm");
          // Only storing relative instead of absolute paths as a convenience for unit tests.
          models[i].put(KMManager.KMKey_CustomHelpLink, welcomeFile.getPath());
        } else {
          models[i].put(KMManager.KMKey_CustomHelpLink, "");
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
   * @param tempPath Filepath of temporarily extracted .kmp file
   * @param key String of jsonArray to iterate through ("keyboards" or "lexicalModels")
   * @return A list of data maps of the newly installed and/or newly upgraded lexical models found in the package.
   * May be empty if the package file is actually an old version.
   * <br/><br/>
   * @throws IOException
   * @throws JSONException
   */
  public List<Map<String, String>> processKMP(File path, File tempPath, String key) throws IOException, JSONException {
    return super.processKMP(path, tempPath, key, null);
  }
}
