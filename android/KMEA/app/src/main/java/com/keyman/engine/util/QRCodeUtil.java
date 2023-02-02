/**
 * Copyright (C) 2022 SIL International. All rights reserved.
 */

package com.keyman.engine.util;

import android.graphics.Bitmap;

import net.glxn.qrgen.android.QRCode;

/**
 * Utility to generate QR Code from a URL string
 */
public final class QRCodeUtil {
  public static final int DEFAULT_HEIGHT = 800;
  public static final int DEFAULT_WIDTH = 800;
  public static final String QR_CODE_URL_FORMATSTR = "https://keyman.com/go/keyboard/%s/share";

  /**
   * Generate QR Code as a Bitmap
   * @param url String
   * @return Bitmap of the QR Code
   */
  public static Bitmap toBitmap(String url) {
    Bitmap result = QRCode.from(url).withSize(DEFAULT_WIDTH, DEFAULT_HEIGHT).bitmap();
    return result;
  }
}