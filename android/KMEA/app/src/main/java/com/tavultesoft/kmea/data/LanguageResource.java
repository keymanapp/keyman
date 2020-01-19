package com.tavultesoft.kmea.data;

import android.os.Bundle;

public interface LanguageResource {
  String getResourceId();
  String getResourceName();
  String getLanguageCode();
  String getLanguageName();
  String getVersion();
  String getPackage();
  String getCustomHelpLink();

  Bundle buildDownloadBundle();
}
