/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmea.util;

import java.io.File;
import java.io.InputStream;

/**
 * Small class for returning information about a file downloaded via DownloadManager.
 * The file is copied to cache
 */
public final class DownloadManagerFileInfo {
  private final boolean isKMP;
  private final String filename;    // Internal filename (may be a document number)
  private final File file;         // File handle to the cached file

  public DownloadManagerFileInfo(boolean isKMP, String filename, File file) {
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

  public File getFile() { return file; }
}
