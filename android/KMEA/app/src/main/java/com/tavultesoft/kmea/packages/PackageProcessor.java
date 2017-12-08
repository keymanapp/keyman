package com.tavultesoft.kmea.packages;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.JSONParser;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

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

  static boolean clearDirectory(File path) {
    // Java won't allow deleting non-empty directories, so we need recursion to perform a full delete.
    if(path.isDirectory()) {
      File[] children = path.listFiles();

      for(File child:children) {
        boolean resultFlag = clearDirectory(child);
        if(!resultFlag) {
          return false;
        }
      }
    }

    return path.delete();
  }

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
    System.out.println("Attempting load of file: " + infoFile);

    JSONParser parser = new JSONParser();
    return parser.getJSONObjectFromFile(infoFile);
  }

  public static void /*eventually -> Package*/ processKMP(File path) throws IOException {
    System.out.println("File size: " + path.length());
    System.out.println("Absolute path: " + path.getAbsolutePath());
  }
}
