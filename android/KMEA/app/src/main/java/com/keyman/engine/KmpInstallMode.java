package com.keyman.engine;

public enum KmpInstallMode {
  Silent,       // Show no UI while installing a package. Errors may be shown as toasts
  WelcomeOnly,  // Show only the welcome screen after installing a package; language will
                // be automatically selected if not provided as an input parameter
  Full;         // Show readme, language picker (if applicable), and welcome after install.

  public static KmpInstallMode fromString(String mode) {
    if(mode == null) return Full;
    switch(mode.toLowerCase()) {
      case "silent": return Silent;
      case "welcomeonly": return WelcomeOnly;
      case "full": return Full;
    }
    return Full;
  }
}

