/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.keyman.engine.util;

import android.net.Uri;
import android.os.Build;

import com.keyman.engine.BuildConfig;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KMManager.Tier;
import com.keyman.engine.util.KMString;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Utilities for parsing keyboard install and keyboard download URLs
 */
public final class KMPLink {

  public static final String KMP_PRODUCTION_HOST = "keyman.com";
  public static final String KMP_STAGING_HOST =  "keyman-staging.com";

  private static final String KMP_INSTALL_KEYBOARDS_PATTERN_FORMATSTR = "^http(s)?://(%s|%s)/keyboards/install/([^\\?/]+)(\\?(.+))?$";
  private static final String installPatternFormatStr = KMString.format(KMP_INSTALL_KEYBOARDS_PATTERN_FORMATSTR,
    KMP_PRODUCTION_HOST,
    KMP_STAGING_HOST);
  private static final Pattern installPattern = Pattern.compile(installPatternFormatStr);

  // Keyman 14.0+ keyboard download links from Keyman server
  private static final String KMP_DOWNLOAD_KEYBOARDS_PATTERN_FORMATSTR = "^https://(%s|%s)(/go/package/download/)(\\w+)(\\?platform=android&tier=(alpha|beta|stable))(&bcp47=)?(.+)?";
  private static final String downloadPatternFormatStr = KMString.format(KMP_DOWNLOAD_KEYBOARDS_PATTERN_FORMATSTR,
    KMP_PRODUCTION_HOST,
    KMP_STAGING_HOST);
  private static final Pattern downloadPattern = Pattern.compile(downloadPatternFormatStr);

  private static final String LEGACY_DOWNLOAD_KEYBOARDS_PATTERN_FORMATSTR = "^keyman://localhost/open\\?(.+)?$";
  private static final Pattern legacyDownloadPattern = Pattern.compile(LEGACY_DOWNLOAD_KEYBOARDS_PATTERN_FORMATSTR);

  // Keyman 14.0+ generated URL for keyboard download links
  private static final String KMP_DOWNLOAD_KEYBOARDS_FORMATSTR = "https://%s/go/package/download/%s";

  /**
   * Check if a URL is a valid Keyman keyboard install link with a packageID
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

  /**
   * Check if a URL is a valid Keyman keyboard download KMP link
   * @param url String of the URL to parse
   * @return boolean
   */
  public static boolean isKeymanDownloadLink(String url) {
    boolean status = false;
    if (url == null || url.isEmpty()) {
      return status;
    }
    Matcher matcher = downloadPattern.matcher(url);
    if (matcher.matches()) {
      status = true;
    }

    return status;
  }

  /**
   * Check if a URL is a valid legacy Keyman keyboard download link (uses keyman://)
   * @param url String of the URL to parse
   * @return boolean
   */
  public static boolean isLegacyKeymanDownloadLink(String url) {
    boolean status = false;
    if (url == null || url.isEmpty()) {
      return status;
    }
    Matcher legacyMatcher = legacyDownloadPattern.matcher(url);
    if (legacyMatcher.matches() && legacyMatcher.group(1) != null) {
      Uri data = Uri.parse(url);
      String keyboard = data.getQueryParameter("keyboard");
      String language = data.getQueryParameter("language");
      if (keyboard != null && !keyboard.isEmpty()) {
        status = true;
      }
    }

    return status;
  }

  /**
   * Get the keyman.com host (production vs staging) based on the tier
   * @return String of keyman.com host
   */
  public static String getHost() {
    switch (KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME)) {
      case ALPHA:
      case BETA:
        return KMP_STAGING_HOST;
      default:
        return KMP_PRODUCTION_HOST;
    }
  }

  /**
   * Parses a URL and generates the Keyman keyboard download link. URL contains:
   * host (required)
   * packageID (required)
   * tier (optional)
   * BCP 47 languageID (optional)
   * @param url String of the URL to parse
   * @return Uri of the  Keyman keyboard download link
   */
  public static Uri getKeyboardDownloadLink(String url) {
    Uri uri = null;

    if (url == null || url.isEmpty()) {
      return uri;
    }

    Matcher matcher = installPattern.matcher(url);
    // Validate deep link with package ID and optional bcp47 tag
    if (matcher.matches() && matcher.group(3) != null) {
      String host = matcher.group(2);
      String packageID = matcher.group(3);
      String tier = KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME).toString().toLowerCase();
      Uri installUri = Uri.parse(url);
      String languageID = installUri.getQueryParameter(KMKeyboardDownloaderActivity.KMKey_BCP47);

      String downloadURL = KMString.format(KMP_DOWNLOAD_KEYBOARDS_FORMATSTR,
        host,
        packageID);
      uri = Uri.parse(downloadURL)
        .buildUpon()
        .appendQueryParameter(KMKeyboardDownloaderActivity.KMKey_Platform, "android")
        .appendQueryParameter(KMKeyboardDownloaderActivity.KMKey_Tier, tier)
        .build();
      if (languageID != null) {
        uri = uri.buildUpon()
          .appendQueryParameter(KMKeyboardDownloaderActivity.KMKey_BCP47, languageID)
          .build();
      }
    }

    return uri;
  }

  /**
   * Parses a URL and generates the Keyman keyboard download link. URL contains:
   * keyboard (required)
   * language (optional)
   * @param url String of the URL to parse
   * @return Uri of the  Keyman keyboard download link
   */
  public static Uri getLegacyKeyboardDownloadLink(String url) {
    Uri uri = null;

    if (url == null || url.isEmpty()) {
      return uri;
    }

    Matcher matcher = legacyDownloadPattern.matcher(url);
    // Validate deep link with package ID and optional bcp47 tag
    if (matcher.matches() && matcher.group(1) != null) {
      Uri installUri = Uri.parse(url);
      String tier = KMManager.getTier(BuildConfig.KEYMAN_ENGINE_VERSION_NAME).toString().toLowerCase();
      String host = (tier.equals("alpha") || tier.equals("beta")) ? KMP_STAGING_HOST : KMP_PRODUCTION_HOST;
      String keyboardID = installUri.getQueryParameter("keyboard");
      String languageID = installUri.getQueryParameter("language");

      String downloadURL = KMString.format(KMP_DOWNLOAD_KEYBOARDS_FORMATSTR,
        host,
        keyboardID); // Using keyboardID instead of packageID
      uri = Uri.parse(downloadURL)
        .buildUpon()
        .appendQueryParameter(KMKeyboardDownloaderActivity.KMKey_Platform, "android")
        .appendQueryParameter(KMKeyboardDownloaderActivity.KMKey_Tier, tier)
        .build();
      if (languageID != null) {
        uri = uri.buildUpon()
          .appendQueryParameter(KMKeyboardDownloaderActivity.KMKey_BCP47, languageID)
          .build();
      }
    }

    return uri;
  }
}
