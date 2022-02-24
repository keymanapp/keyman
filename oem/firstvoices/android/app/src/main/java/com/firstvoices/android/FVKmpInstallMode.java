package com.firstvoices.android;

public enum FVKmpInstallMode {
  Silent,       // Show no UI while installing a package. Errors may be shown as toasts
  WelcomeOnly,  // Show only the welcome screen after installing a package; language will
  // be automatically selected if not provided as an input parameter
  Full          // Show readme, language picker (if applicable), and welcome after install.
}
