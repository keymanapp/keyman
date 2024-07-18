/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.keyman.engine.util;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public class ZipUtils {
  private final static String TAG = "ZipUtils";

  // Credit to zapi's answer at https://stackoverflow.com/questions/3382996/how-to-unzip-files-programmatically-in-android.
  public static void unzip(File zipFile, File targetDirectory) throws IOException {
    ZipInputStream zis = new ZipInputStream(
      new BufferedInputStream(new FileInputStream(zipFile)));
    try {
      ZipEntry ze;
      int count;
      byte[] buffer = new byte[8192];
      while ((ze = zis.getNextEntry()) != null) {
        File file = new File(targetDirectory, ze.getName());

        // Check for zip path traversal vulnerability
        // https://support.google.com/faqs/answer/9294009
        String canonicalPath = file.getCanonicalPath();
        if (!canonicalPath.startsWith(targetDirectory.getCanonicalPath())) {
          // Security exception
          throw new SecurityException("Zip traversal error");
        }

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
}
