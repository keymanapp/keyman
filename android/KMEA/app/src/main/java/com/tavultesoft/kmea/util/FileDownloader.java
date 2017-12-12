package com.tavultesoft.kmea.util;

import android.content.Context;
import android.util.Log;

import com.tavultesoft.kmea.KMManager;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.InputStream;

public final class FileDownloader {

  /**
   * Utility to download a file from urlStr and store it at directory/filename.
   * If the directory does not exist, it will be created.
   * @param context
   * @param urlStr Source
   * @param directory Destination path.
   * @param filename Destination filename. If blank, it will use the filename from the URL
   * @return
   */
  public static int download(Context context, String urlStr, String directory, String filename) {
    final int BUFFER_SIZE = 4096;
    int ret = -1;
    String fileName = "";
    String tmpFileName = "";
    File tmpFile = null;
    File file = null;

    try {
      if (directory == null) {
        directory = "";
      }
      directory = directory.trim();

      String dirPath;
      if (directory.length() != 0) {
        directory = directory + "/";
        dirPath = context.getDir("data", Context.MODE_PRIVATE) + "/" + directory;
      } else {
        dirPath = context.getDir("data", Context.MODE_PRIVATE).toString();
      }
      File dir = new File(dirPath);
      if (!dir.exists()) {
        dir.mkdir();
      }

      if (Connection.initialize(urlStr)) {
        InputStream binStream = new BufferedInputStream(Connection.getInputStream(), BUFFER_SIZE);
        byte[] buff = new byte[BUFFER_SIZE];

        filename = filename.trim();
        if (filename == null || filename.isEmpty()) {
          fileName = Connection.getFile().substring(Connection.getFile().lastIndexOf('/') + 1);
          if (fileName.lastIndexOf(".js") > 0 && !fileName.contains("-")) {
            fileName = fileName.substring(0, filename.lastIndexOf(".js")) + "-1.0.js";
          }
        } else {
          fileName = filename;
        }
        tmpFileName = String.format("%s.tmp", fileName);
        file = new File(dirPath, fileName);
        tmpFile = new File(dirPath, tmpFileName);
        FileOutputStream fos = new FileOutputStream(tmpFile);

        int len;
        while ((len = binStream.read(buff)) != -1) {
          fos.write(buff, 0, len);
        }

        fos.flush();
        fos.close();
        binStream.close();

        ret = 1;
      }
    } catch (Exception e) {
      ret = -1;
      Log.e("FileDownloader", "Download failed! Error: " + e);
    } finally {
      if (ret > 0) {
        if (tmpFile.exists() && tmpFile.length() > 0) {
          if (file.exists()) {
            file.delete();
          }
          if (!tmpFile.renameTo(file)) {
            ret = -1;
          } else if (KMManager.isDebugMode()) {
            Log.d("FileDownloader", "Download finished for filename " + file.toString());
          }
        } else {
          ret = -1;
        }
      } else {
        if (file.exists()) {
          file.delete();
        }
        if (tmpFile.exists()) {
          tmpFile.delete();
        }
        if (KMManager.isDebugMode()) {
          Log.d("FileDownloader", "Could not download filename " + file.toString());
        }
      }

      Connection.disconnect();
    }

    return ret;
  }
}
