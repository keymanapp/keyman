package com.tavultesoft.kmea.util;

import android.content.Context;
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

  public static boolean libraryExists(Context context) {
    boolean result = false;
    try {
      Class.forName("net.glxn.qrgen.android.QRCode");
      return true;
    } catch (ClassNotFoundException e) {
      // Intentionally not sending to Sentry because 3rd party apps may not include this library
      return false;
    }
  }
}