/**
 * Copyright (C) 2022 SIL International. All rights reserved.
 */
package com.keyman.engine.util;

import com.keyman.engine.BuildConfig;

/**
 * Utilities to determine if a build is local or a test (PR) build.
 */
public final class VersionUtils {
  public static final String TAG = "VersionUtils";

  public static boolean isLocalBuild() {
    return BuildConfig.VERSION_ENVIRONMENT.equalsIgnoreCase("local");
  }

  public static boolean isTestBuild() {
    return BuildConfig.VERSION_ENVIRONMENT.equalsIgnoreCase("test");
  }

  // Utility for local and PR test builds - e.g. force check of keyboard updates
  public static boolean isLocalOrTestBuild() {
    return isLocalBuild() || isTestBuild();
  }
}
