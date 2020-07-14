/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmea.util;

import android.os.Build;

import com.tavultesoft.kmea.BuildConfig;
import com.tavultesoft.kmea.KMManager;

import java.util.regex.Matcher;
import java.util.regex.Pattern;


public final class KMPLink {

  public static final String KMP_PRODUCTION_HOST = "https://keyman.com";
  public static final String KMP_STAGING_HOST = "https://staging-keyman-com.azurewebsites.net";

  private static final String KMP_INSTALL_KEYBOARDS_PATTERN_FORMATSTR = "^(%s|%s)(/keyboards/install/)(\\w+)(\\?bcp47=)?(.+)?";
  private static final String installPatternFormatStr = String.format(KMP_INSTALL_KEYBOARDS_PATTERN_FORMATSTR,
    KMP_PRODUCTION_HOST,
    KMP_STAGING_HOST);
  private static final Pattern installPattern = Pattern.compile(installPatternFormatStr);

  // Keyman 14.0+ keyboard download links from Keyman server
  private static final String KMP_DOWNLOAD_KEYBOARDS_PATTERN_FORMATSTR = "^(%s|%s)(/go/package/download/)(\\w+)(\\?platform=android&tier=(alpha|beta|stable))(&bcp47=)?(.+)?";
  private static final String downloadPatternFormatStr = String.format(KMP_DOWNLOAD_KEYBOARDS_PATTERN_FORMATSTR,
    KMP_PRODUCTION_HOST,
    KMP_STAGING_HOST);
  private static final Pattern downloadPattern = Pattern.compile(downloadPatternFormatStr);

  // Keyman 13.0 keyboard download links from Keyman server
  // TODO: Remove this in 14.0 Beta  when keyboard search updated on live site
  private static final String KMP_DOWNLOAD_KEYBOARDS_13_PATTERN_FORMATSTR = "(%s)(/keyboard/download\\?id=)(\\w+)(&platform=android&mode=standalone)(.+)?";
  private static final String download13PatternFromatStr = String.format(KMP_DOWNLOAD_KEYBOARDS_13_PATTERN_FORMATSTR,
    KMP_PRODUCTION_HOST);
  private static final Pattern download13Pattern = Pattern.compile(download13PatternFromatStr);

  // Keyman 14.0+ generated URL for keyboard download links
  private static final String KMP_DOWNLOAD_KEYBOARDS_FORMATSTR = "%s/go/package/download/%s?platform=android&tier=%s%s";
  private static final String KMP_DOWNLOAD_KEYBOARDS_LANGUAGE_FORMATSTR = "&bcp47=%s";

  /**
   * Check if a URL is a valid Keyman keyboard download link with a packageID
   * @param url String of the URL to parse
   * @return boolean
   */
  public static boolean isKeymanInstallLink(String url) {
    boolean status = false;
    if (url == null || url.isEmpty()) {
      return status;
    }
    Matcher matcher = installPattern.matcher(url);
    if (matcher.matches() && matcher.group(3) != null) {
      status = true;
    }

    return status;
  }

  public static boolean isKeymanDownloadLink(String url) {
    boolean status = false;
    if (url == null || url.isEmpty()) {
      return status;
    }
    Matcher matcher = downloadPattern.matcher(url);
    Matcher matcher13 = download13Pattern.matcher(url);
    if (matcher.matches() || matcher13.matches()) {
      status = true;
    }

    return status;
  }
  /**
   * Parses a URL and generates the Keyman keyboard download link. Parameters are:
   * host (required)
   * packageID (required)
   * tier (optional)
   * languageID (optional)
   * @param url String of the URL to parse
   * @return String of the  Keyman keyboard download link
   */
  public static String getKeyboardDownloadLink(String url) {
    String downloadURL = null;

    if (url == null || url.isEmpty()) {
      return downloadURL;
    }

    Matcher matcher = installPattern.matcher(url);
    // Validate deep link with package ID and optional bcp47 tag
    if (matcher.matches() && matcher.group(3) != null) {
      String host = matcher.group(1);
      String packageID = matcher.group(3);
      String tier = KMManager.getTier(BuildConfig.VERSION_NAME).toString().toLowerCase();
      String languageStr = (matcher.group(5) != null) ? String.format(KMP_DOWNLOAD_KEYBOARDS_LANGUAGE_FORMATSTR,
        matcher.group(5)) : "";
      downloadURL = String.format(KMP_DOWNLOAD_KEYBOARDS_FORMATSTR,
        host,
        packageID,
        tier,
        languageStr);
    }

    return downloadURL;
  }
}