package com.tavultesoft.kmea.util;

import android.content.Context;
import android.os.Build;
import android.util.Log;

import com.tavultesoft.kmea.KMManager;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

/**
  * Limitations:
  * Ideally, FileUtils would extend org.apache.commons.io.FileUtils. Although that would compile,
  * the underlying dependencies on java.nio.file don't seem to be available to Android at runtime.
  * This might be usable once we can upgrade to Android Oreo
 */
public final class FileUtils {

  public static final int DOWNLOAD_ERROR = -1;
  public static final int DOWNLOAD_SUCCESS = 1;

  // File extensions and file types
  public static final String JAVASCRIPT = ".js";
  public static final String TRUETYPEFONT = ".ttf";
  public static final String OPENTYPEFONT = ".otf";

  public static final String SVGFONT = ".svg";
  public static final String SVGVIEWBOX = ".svg#";
  public static final String WOFFFONT = ".woff";

  public static final String KEYBOARDPACKAGE = ".kmp";
  public static final String WELCOME_HTM = "welcome.htm";

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
        if (file.exists()) {
          file.delete();
        }
        if (tmpFile.exists()) {
          tmpFile.delete();
        }
        Log.e("FileUtils", "Could not download filename " + file.toString());
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
   * Utility to parse a URL and extract the filename
   * @param urlStr String
   * @return filename String
   */
  public static String getFilename(String urlStr) {
    String filename = "unknown";
    if (urlStr != null && !urlStr.isEmpty()) {
      filename = urlStr.substring(urlStr.lastIndexOf('/') + 1);
    }
    return filename;
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

  public static boolean hasKeyboardPackageExtension(String filename) {
    String f = filename.toLowerCase();
    return f.endsWith(KEYBOARDPACKAGE);
  }

  public static boolean isWelcomeFile(String filename) {
    String f = getFilename(filename);
    return f.equalsIgnoreCase(WELCOME_HTM);
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
