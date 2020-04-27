package com.tavultesoft.kmea.data;

import android.os.Bundle;

public interface LanguageResource {
  String getResourceId();
  String getResourceName();
  String getLanguageID();

  // Deprecated in Keyman 14.0 by getLanguageID();
  String getLanguageCode();
  String getLanguageName();
  String getVersion();
  String getPackageID();

  // Deprecated in Keyman 14.0 by getPackageID()
  String getPackage();
  String getCustomHelpLink();

  Bundle buildDownloadBundle();
}
