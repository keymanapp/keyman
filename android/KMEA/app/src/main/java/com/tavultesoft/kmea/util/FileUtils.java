package com.tavultesoft.kmea.util;

import android.content.Context;
import android.os.Build;
import android.util.Log;

import com.tavultesoft.kmea.KMManager;

import org.json.JSONArray;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.nio.charset.StandardCharsets;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

  public static final int VERSION_INVALID = -2;
  public static final int VERSION_EQUAL = 0;
  public static final int VERSION_LOWER = -1;
  public static final int VERSION_GREATER = 1;

  // File extensions and file types
  public static final String JAVASCRIPT = ".js";
  public static final String LEXICALMODEL = ".model.js";
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
      ret = -1;
      Log.e("FileUtils", "Download failed! Error: " + e);
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
        Log.e("FileUtils", "Could not download filename " + filename);
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

  public static boolean saveList(Context context, String listName, JSONArray arr) {
    boolean result;
    final int INDENT = 2; // 2 spaces indent
    try {
      File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), listName);
      OutputStreamWriter outputStream = new OutputStreamWriter(new FileOutputStream(file), StandardCharsets.UTF_8);
      outputStream.write(arr.toString(INDENT));
      outputStream.flush();
      outputStream.close();
      result = true;
    } catch (Exception e) {
      Log.e(TAG, "Failed to save " + listName + ". Error: " + e);
      result = false;
    }

    return result;
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
   * Utility to parse a URL and determine if it's a valid keyman:<method>
   * Currently, only "keyman" scheme with "download" path and query is supported.
   * Legacy keyman:// protocol is deprecated and not supported.
   * @param u String of the URL
   * @return boolean true if URL is a supported Keyman link
   */
  public static boolean isKeymanLink(String u) {
    boolean ret = false;
    if (u == null) {
      return ret;
    }
    String lowerU = u.toLowerCase();
    Pattern pattern = Pattern.compile("^keyman:(\\w+)\\?(.+)");
    Matcher matcher = pattern.matcher(lowerU);
    // Check URL starts with "keyman"
    if (matcher.matches() && (matcher.group(1) != null)) {
      // For now, only handle "download"
      switch (matcher.group(1).toLowerCase()) {
        case "download":
          if (matcher.group(2) != null) {
            // Contains query
            ret = true;
          }
          break;
        default:
          ret = false;
      }
    }
    return ret;
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
      Log.e("FileUtils", "parseInteger Error: " + e);
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
