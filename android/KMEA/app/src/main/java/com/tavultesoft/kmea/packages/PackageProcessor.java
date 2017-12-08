package com.tavultesoft.kmea.packages;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.JSONParser;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.apache.commons.io.FileUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

/**
 * Created by joshua on 12/7/2017.
 */

public class PackageProcessor {
  private static File resourceRoot = null;

  public static void initialize(File resourceRoot) {
    PackageProcessor.resourceRoot = resourceRoot;
  }

  // Credit to zapi's answer at https://stackoverflow.com/questions/3382996/how-to-unzip-files-programmatically-in-android.
  static void unzip(File zipFile, File targetDirectory) throws IOException {
    ZipInputStream zis = new ZipInputStream(
      new BufferedInputStream(new FileInputStream(zipFile)));
    try {
      ZipEntry ze;
      int count;
      byte[] buffer = new byte[8192];
      while ((ze = zis.getNextEntry()) != null) {
        File file = new File(targetDirectory, ze.getName());
        File dir = ze.isDirectory() ? file : file.getParentFile();
        if (!dir.isDirectory() && !dir.mkdirs())
          throw new FileNotFoundException("Failed to ensure directory: " +
            dir.getAbsolutePath());
        if (ze.isDirectory())
          continue;
        FileOutputStream fout = new FileOutputStream(file);
        try {
          while ((count = zis.read(buffer)) != -1)
            fout.write(buffer, 0, count);
        } finally {
          fout.close();
        }
            /* if time should be restored as well
            long time = ze.getTime();
            if (time > 0)
                file.setLastModified(time);
            */
      }
    } finally {
      zis.close();
    }
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

      if(!ext.equals(".kmp")) {
        throw new IllegalArgumentException("Invalid file passed to the KMP unpacker!");
      }
    } else {
      throw new IllegalArgumentException("Invalid file passed to the KMP unpacker!");
    }

    // Extract our best-guess name for the package and construct the temporary package name.
    kmpBaseName = filename.substring(0, filename.lastIndexOf('.'));
    String kmpFolderName = temp ? "." + kmpBaseName + ".temp" : kmpBaseName;

    return new File(resourceRoot,KMManager.KMDefault_AssetPackages + File.separator + kmpFolderName + File.separator);
  }

  static File unzipKMP(File path) throws IOException {
    File tempKeyboardPath = constructPath(path, true);
    unzip(path, tempKeyboardPath);

    return tempKeyboardPath;
  }

  static JSONObject loadPackageInfo(File packagePath) {
    File infoFile = new File(packagePath, "kmp.json");

    JSONParser parser = new JSONParser();
    return parser.getJSONObjectFromFile(infoFile);
  }

  // Call this once per each entry of the JSON `keyboards` array, then concatenate the resulting arrays for a full list.
  public static Map<String, String>[] processKeyboardsEntry(JSONObject jsonKeyboard) throws JSONException {
    JSONArray languages = jsonKeyboard.getJSONArray("languages");

    HashMap<String, String>[] keyboards = new HashMap[languages.length()];

    // This output spec is designed to mirror the `download` method output of KMKeyboardDownloader as closely as practical.
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

  public static String getVersion(JSONObject json) throws JSONException {
    return json.getJSONObject("system").getString("fileVersion");
  }

  // Returns 1 if newer, 0 if equal, and -1 if older or invalid.  If no prior version exists, returns 1.
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

  @Nullable
  public static List<Map<String, String>> processKMP(File path) throws IOException, JSONException {
    File tempPath = unzipKMP(path);
    JSONObject newInfoJSON = loadPackageInfo(tempPath);

    File permPath = constructPath(path, false);
    if(permPath.exists()) {
      if(comparePackageDirectories(tempPath, permPath) != -1) {
        // Abort!  The current installation is newer or as up-to-date.
        FileUtils.deleteDirectory(tempPath);
        return null;
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
