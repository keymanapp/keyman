/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.keyman.engine.util;

import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.provider.OpenableColumns;
import android.widget.Toast;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;

/**
 * Utilities for handling a file downloaded via DownloadManager. Currently, this only handles URI
 * schemes of content:// and file://.
 */
public final class DownloadFileUtils {
  private static final String TAG = "DownloadFileUtils";

  /**
   * Small class for returning information about a file downloaded via DownloadManager.
   */
  public static final class Info {
    private final boolean isKMP;
    private final String filename;   // Usable filename (instead of a document number passed from DownloadManager)
    private final File file;         // File handle to the cached file

    public Info(boolean isKMP, String filename, File file) {
      this.isKMP = isKMP;
      this.filename = filename;
      this.file = file;
    }

    public boolean isKMP() {
      return isKMP;
    }

    public String getFilename() {
      return filename;
    }

    public File getFile() {
      return file;
    }
  }

  /**
   * Utility to copy a downloaded file from DownloadManager into cache.
   * This makes it easier for cloud callbacks to handle (converting a document/number into
   * expected filenames.
   * @param context
   * @param {Uri} data
   * @return Info
   */
  public static Info cacheDownloadFile(Context context, Uri data) {
    boolean isKMP = false;
    String filename = "";
    File cachedFile = null;
    InputStream inputFile = null;
    try {
      switch (data.getScheme().toLowerCase()) {
        case "content":
          // DownloadManager passes a path "/document/number" so we need to extract the .kmp filename
          Cursor cursor = context.getContentResolver().query(data, null, null, null, null);
          cursor.moveToFirst();
          int nameIndex = cursor.getColumnIndex(OpenableColumns.DISPLAY_NAME);
          filename = cursor.getString(nameIndex);
          isKMP = FileUtils.hasKeymanPackageExtension(filename);
          inputFile = context.getContentResolver().openInputStream(data);
          break;

        case "file":
          File kmpFile = new File(data.getPath());
          filename = kmpFile.getName();
          isKMP = FileUtils.hasKeymanPackageExtension(data.toString());
          inputFile = new FileInputStream(kmpFile);
          break;
      }

      if (inputFile != null && filename != null) {
        cachedFile = new File(context.getCacheDir().toString(), filename);
        if (cachedFile.exists()) {
          cachedFile.delete();
        }

        FileUtils.copy(inputFile, new FileOutputStream(cachedFile));
      }
    } catch (Exception e) {
      String message = "Access denied to " + filename +
        ".\nCheck Android Settings --> Apps --> Keyman to grant storage permissions";
      KMLog.LogException(TAG, "Unable to copy " + filename + " to app cache ", e);
    }

    Info info = new Info(isKMP, filename, cachedFile);
    return info;
  }

}
