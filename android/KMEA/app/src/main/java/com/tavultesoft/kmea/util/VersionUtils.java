/**
 * Copyright (C) 2022 SIL International. All rights reserved.
 */
package com.tavultesoft.kmea.util;

import com.tavultesoft.kmea.BuildConfig;

/**
 * Utilities to determine if a build is local or a test (PR) build.
 */
public final class VersionUtils {
  public static final String TAG = "VersionUtils";

  public static boolean isLocalBuild() {
    return BuildConfig.VERSION_ENVIRONMENT.equalsIgnoreCase("local");
  }

  public static boolean isTestBuild() {
    return BuildConfig.KEYMAN_ENGINE_VERSION_NAME.matches("^.*(-test-\\d+)$");
  }
}
