package com.tavultesoft.kmea.data;

import android.os.Bundle;
import android.util.Log;

import java.io.Serializable;

public abstract class LanguageResource implements Serializable {
  protected String packageID;
  protected String resourceID;
  protected String resourceName;
  protected String languageID;
  protected String languageName;
  protected String version;
  protected String helpLink;

  public String getResourceID() { return resourceID; }

  public String getResourceName() { return resourceName; }

  public String getLanguageID() { return languageID; }

  // Deprecated in Keyman 14.0 by getLanguageID();
  public String getLanguageCode() { return languageID; }

  public String getLanguageName() { return languageName; }

  public String getVersion() { return version; }

  public String getPackageID() { return packageID; }

  // Deprecated in Keyman 14.0 by getPackageID()
  public String getPackage() { return packageID; }

  public String getHelpLink() { return helpLink; }

  public int hashCode() {
    String id = getResourceID();
    String lgCode = getLanguageID();
    if (id == null || lgCode == null) {
      Log.e("LanguageResource", "Invalid hashCode");
    }
    return id.hashCode() * lgCode.hashCode();
  }

  public abstract Bundle buildDownloadBundle();

  //public abstract boolean equals();
}
