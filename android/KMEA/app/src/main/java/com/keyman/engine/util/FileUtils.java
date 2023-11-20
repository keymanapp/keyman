/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.keyman.engine.util;

import android.content.Context;
import android.content.res.AssetManager;
import android.util.Log;

import com.keyman.engine.KMManager;

import org.json.JSONObject;
import org.json.JSONArray;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;

/**
  * Limitations:
  * Ideally, FileUtils would extend org.apache.commons.io.FileUtils. Although that would compile,
  * the underlying dependencies on java.nio.file don't seem to be available to Android at runtime.
  * This might be usable once we can upgrade to Android Oreo
 */
public final class FileUtils {
  public static final String TAG = "FileUtils";

  public static final int DOWNLOAD_ERROR = -1;
  public static final int DOWNLOAD_SUCCESS = 1;
  public static final int DOWNLOAD_CANCELLED = 2;

  public static final int VERSION_INVALID = -2;
  public static final int VERSION_EQUAL = 0;
  public static final int VERSION_LOWER = -1;
  public static final int VERSION_GREATER = 1;

  // File extensions and file types
  public static final String JAVASCRIPT = ".js";
  public static final String LEXICALMODEL = ".model.js";
  public static final String PDF = ".pdf";
  public static final String TRUETYPEFONT = ".ttf";
  public static final String OPENTYPEFONT = ".otf";

  public static final String SVGFONT = ".svg";
  public static final String SVGVIEWBOX = ".svg#";
  public static final String WOFFFONT = ".woff";

  public static final String MODELPACKAGE = ".model.kmp";
  public static final String KEYMANPACKAGE = ".kmp";
  public static final String WELCOME_HTM = "welcome.htm";
  public static final String README_HTM = "readme.htm";

  /**
   * Utility to download a file from urlStr and store it at destinationDir/destinationFilename.
   * If the destination directory does not exist, it will be created.
   * @param context
   * @param urlStr Source URL
   * @param destinationDir Absolute path for destination. If null or empty, the app "data" will be used.
   * @param destinationFilename Destination filename. If blank, it will use the filename from the URL
   * @return ret int -1 for fail
   */
  public static int download(Context context, String urlStr, String destinationDir, String destinationFilename) {
    final int BUFFER_SIZE = 4096;
    int ret = DOWNLOAD_ERROR;
    String directoryStr = "";
    String filename = "";
    String tmpFilename = "";
    File tmpFile = null;
    File file = null;

    try {
      if (destinationDir == null || destinationDir.trim().isEmpty()) {
        directoryStr = context.getDir("data", Context.MODE_PRIVATE).toString() + File.separator;
      } else {
        directoryStr = destinationDir.trim();
      }

      if (!directoryStr.endsWith(File.separator)) {
        directoryStr = directoryStr + File.separator;
      }
      File dir = new File(directoryStr);
      if (!dir.exists()) {
        dir.mkdir();
      }

      if (Connection.initialize(urlStr)) {
        InputStream binStream = new BufferedInputStream(Connection.getInputStream(), BUFFER_SIZE);
        byte[] buff = new byte[BUFFER_SIZE];

        if (destinationFilename == null || destinationFilename.isEmpty()) {
          filename = getFilename(Connection.getFile());
          if (hasJavaScriptExtension(filename) && !filename.contains("-")) {
            filename = filename.substring(0, filename.lastIndexOf(".js")) + "-1.0.js";
          }
        } else {
          filename = destinationFilename.trim();
        }
        tmpFilename = String.format("%s.tmp", filename);
        file = new File(directoryStr, filename);
        tmpFile = new File(directoryStr, tmpFilename);
        FileOutputStream fos = new FileOutputStream(tmpFile);

        int len;
        while ((len = binStream.read(buff)) != -1) {
          fos.write(buff, 0, len);
        }

        fos.flush();
        fos.close();
        binStream.close();

        ret = DOWNLOAD_SUCCESS;
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "Download failed! Error: ", e);
      ret = -1;
    } finally {
      if (ret > 0) {
        if (tmpFile.exists() && tmpFile.length() > 0) {
          if (file.exists()) {
            file.delete();
          }
          if (!tmpFile.renameTo(file)) {
            ret = DOWNLOAD_ERROR;
          }
        } else {
          ret = DOWNLOAD_ERROR;
        }
      } else {
        if (file != null && file.exists()) {
          file.delete();
        }
        if (tmpFile != null && tmpFile.exists()) {
          tmpFile.delete();
        }
        KMLog.LogError(TAG, "Could not download filename " + destinationFilename);
      }

      Connection.disconnect();
    }

    return ret;
  }

  public static void copy(File src, File dest) throws IOException {
    InputStream in = new FileInputStream(src);
    OutputStream out = new FileOutputStream(dest);
    try {
      copy(in, out);
    } finally {

    }
  }

  public static void copy(InputStream in, OutputStream out)  throws IOException {
    try {
      // Transfer bytes from in to out
      byte[] buf = new byte[1024];
      int len;
      while ((len = in.read(buf)) >0) {
        out.write(buf, 0, len);
      }
    } finally {
      out.close();
    }
    in.close();
  }

  /**
   * Utility to copy directory from src to destination.
   * Limitation: Ignores subfolders and doesn't copy them
   * @param srcDir
   * @param destDir
   * @throws IOException
   */
  public static void copyDirectory(final File srcDir, final File destDir) throws IOException {
    if (!srcDir.exists()) {
      throw new IOException(srcDir + " does not exist");
    }
    if (destDir == null) {
      throw new IOException("Destination cannot be null");
    }
    if (!destDir.exists()) {
      destDir.mkdir();
    }
    String destStr = destDir.getPath();

    if (srcDir.isFile()) {
      copy(srcDir, new File(destDir.getPath() + File.separator + srcDir.getParent()));
    } else {
      File[] files = srcDir.listFiles();
      for(File file : files) {
        // Ignore subfolders
        if (file.isFile()) {
          String filename = file.getName();
          copy(file, new File(destStr, filename));
        }
      }
    }
  }

  /**
   * Utility to recursively delete file/directory
   * @param fileOrDirectory File
   * @throws IOException
   */
  public static void deleteDirectory (File fileOrDirectory) throws IOException {
    if (fileOrDirectory.exists()) {
      if (fileOrDirectory.isDirectory()) {
        for (File file : fileOrDirectory.listFiles()) {
          deleteDirectory(file);
        }
      }

      fileOrDirectory.delete();
    }
  }

  /**
   * Write obj Object to filepath
   * @param File path to filename to write
   * @param obj JSONObject or JSONArray to save
   * @return boolean if the save was successful
   */
  public static boolean saveList(File filepath, Object obj) {
    boolean result = false;
    final int INDENT = 2; // 2 spaces indent
    try {
      OutputStreamWriter outputStream = new OutputStreamWriter(new FileOutputStream(filepath), StandardCharsets.UTF_8);
      if (obj instanceof JSONObject) {
        outputStream.write(((JSONObject)obj).toString(INDENT));
        result = true;
      } else if (obj instanceof JSONArray) {
        outputStream.write(((JSONArray) obj).toString(INDENT));
        result = true;
      } else {
        KMLog.LogError(TAG, "saveList failed. arr not JSONObject or JSONArray");
      }
      outputStream.flush();
      outputStream.close();
    } catch (Exception e) {
      KMLog.LogException(TAG, "saveList failed to save " + filepath.getName() + ". Error: ", e);
      result = false;
    }

    return result;
  }

  /**
   * Read the contents of asset file as a string
   * Reference:  https://stackoverflow.com/questions/16110002/read-assets-file-as-string
   * @param context
   * @param path - path of file relative to assets folder
   * @return String
   */
  public static String readContents(Context context, String path) {
    StringBuilder sb = new StringBuilder();
    String str = "";
    AssetManager assetManager = context.getAssets();
    try {
      InputStream inputStream = assetManager.open(path);
      if (inputStream == null) {
        KMLog.LogInfo(TAG, "Unable to read contents of asset: " + path);
        return str;
      }
      BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8));
      while ((str = reader.readLine()) != null) {
        sb.append(str);
      }
      reader.close();
    } catch (Exception e) {
      KMLog.LogException(TAG, "Error reading asset file", e);
      return str;
    }
    return sb.toString();
  }

  /**
   * Utility to parse a URL and extract the filename
   * @param urlStr String
   * @return filename String
   */
  public static String getFilename(String urlStr) {
    String filename = "unknown";
    if (urlStr != null && !urlStr.isEmpty()) {
      if(urlStr.indexOf(File.separator)>=0)
        filename = urlStr.substring(urlStr.lastIndexOf(File.separator) + 1);
      else
        filename = urlStr.substring(urlStr.lastIndexOf('/') + 1);
    }
    return filename;
  }

  /**
   * Utility to compare two version strings
   * @param v1 String
   * @param v2 String
   * @return int
   *   -2 if v1 or v2 is invalid
   *    0 if v1 = v2
   *   -1 if v1 < v2
   *    1 if v1 > v2
   */
  public static int compareVersions(String v1, String v2) {
    // returns;

    if (v1 == null || v2 == null) {
      return VERSION_INVALID;
    }

    if (v1.isEmpty() || v2.isEmpty()) {
      return VERSION_INVALID;
    }

    String[] v1Values = v1.split("\\.");
    String[] v2Values = v2.split("\\.");

    int len = (v1Values.length >= v2Values.length ? v1Values.length : v2Values.length);
    for (int i = 0; i < len; i++) {
      String vStr1 = "0";
      if (i < v1Values.length) {
        vStr1 = v1Values[i];
      }

      String vStr2 = "0";
      if (i < v2Values.length) {
        vStr2 = v2Values[i];
      }

      Integer vInt1 = parseInteger(vStr1);
      Integer vInt2 = parseInteger(vStr2);
      int iV1, iV2, iV1_, iV2_;

      if (vInt1 != null) {
        iV1 = vInt1.intValue();
        iV1_ = 0;
      } else {
        iV1 = 0;
        iV1_ = 0;
      }

      if (vInt2 != null) {
        iV2 = vInt2.intValue();
        iV2_ = 0;
      } else {
        iV2 = 0;
        iV2_ = 0;
      }

      if (vInt1 == null) {
        if (i != (v1Values.length - 1)) {
          return VERSION_INVALID;
        }

        if (vStr1.toLowerCase().endsWith("b")) {
          Integer vInt1_ = parseInteger(vStr1.substring(0, vStr1.length() - 1));
          if (vInt1_ == null) {
            return VERSION_INVALID;
          }

          iV1 = vInt1_.intValue();
          iV1_ = -100;
        } else if (vStr1.toLowerCase().endsWith("a")) {
          Integer vInt1_ = parseInteger(vStr1.substring(0, vStr1.length() - 1));
          if (vInt1_ == null) {
            return VERSION_INVALID;
          }

          iV1 = vInt1_.intValue();
          iV1_ = -200;
        } else {
          return VERSION_INVALID;
        }
      }

      if (vInt2 == null) {
        if (i != (v2Values.length - 1)) {
          return VERSION_INVALID;
        }

        if (vStr2.toLowerCase().endsWith("b")) {
          Integer vInt2_ = parseInteger(vStr2.substring(0, vStr2.length() - 1));
          if (vInt2_ == null) {
            return VERSION_INVALID;
          }

          iV2 = vInt2_.intValue();
          iV2_ = -100;
        } else if (vStr2.toLowerCase().endsWith("a")) {
          Integer vInt2_ = parseInteger(vStr2.substring(0, vStr2.length() - 1));
          if (vInt2_ == null) {
            return VERSION_INVALID;
          }

          iV2 = vInt2_.intValue();
          iV2_ = -200;
        } else {
          return VERSION_INVALID;
        }
      }

      if (iV1 == iV2) {
        if (iV1_ == iV2_) {
          continue;
        }
        if (iV1_ < iV2_) {
          return VERSION_LOWER;
        }
        if (iV1_ > iV2_) {
          return VERSION_GREATER;
        }
      } else if (iV1 < iV2) {
        return VERSION_LOWER;
      } else if (iV1 > iV2) {
        return VERSION_GREATER;
      }
    }

    return VERSION_EQUAL;
  }

  private static Integer parseInteger(String s) {
    Integer retVal = null;
    try {
      int i = Integer.parseInt(s);
      retVal = new Integer(i);
    } catch (Exception e) {
      KMLog.LogException(TAG, "parseInteger Error: ", e);
      retVal = null;
    }

    return retVal;
  }

  /**
   * Utility if a given file is a font. We prefer TTF or OTF, but old Android devices use SVG or WOFF.
   * @param filename
   * @return boolean
   */
  public static boolean hasFontExtension(String filename) {
    String f = filename.toLowerCase();
    return f.endsWith(TRUETYPEFONT) || f.endsWith(OPENTYPEFONT) ||
      f.endsWith(WOFFFONT) || f.endsWith(SVGFONT);
  }

  public static boolean hasSVGViewBox(String filename) {
    String f = filename.toLowerCase();
    return f.contains(SVGVIEWBOX);
  }

  public static boolean hasJavaScriptExtension(String filename) {
    String f = filename.toLowerCase();
    return f.endsWith(JAVASCRIPT);
  }

  public static boolean hasLexicalModelExtension(String filename) {
    String f = filename.toLowerCase();
    return f.endsWith(LEXICALMODEL);
  }

  public static boolean hasLexicalModelPackageExtension(String filename) {
    String f = filename.toLowerCase();
    return f.endsWith(MODELPACKAGE);
  }

  public static boolean hasKeymanPackageExtension(String filename) {
    String f = filename.toLowerCase();
    return f.endsWith(KEYMANPACKAGE);
  }

  public static boolean hasPDFExtension(String filename) {
    String f = filename.toLowerCase();
    return f.endsWith(PDF);
  }

  public static boolean isTTFFont(String filename) {
    String f = filename.toLowerCase();
    return f.endsWith(TRUETYPEFONT);
  }

  public static boolean isWelcomeFile(String filename) {
    String f = getFilename(filename);
    return f.equalsIgnoreCase(WELCOME_HTM);
  }

  public static boolean isReadmeFile(String filename) {
    String f = getFilename(filename);
    return f.equalsIgnoreCase(README_HTM);
  }

  /**
   * Utility to extract the .svg# filename.
   * @param filename String
   * @return String. Empty string if filename is not .svg#
   */
  public static String getSVGFilename(String filename) {
    if (!hasSVGViewBox(filename)) {
      return "";
    }

    String f = filename.toLowerCase();
    int i = f.indexOf(SVGVIEWBOX) + SVGVIEWBOX.length();
    return filename.substring(0, i);
  }
}
