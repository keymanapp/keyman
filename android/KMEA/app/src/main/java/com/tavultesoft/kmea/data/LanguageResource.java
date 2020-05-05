package com.tavultesoft.kmea.data;

import android.os.Bundle;
import android.util.Log;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;

public abstract class LanguageResource implements Serializable {
  protected String packageID;
  protected String resourceID;
  protected String resourceName;
  protected String languageID;
  protected String languageName;
  protected String version;
  protected String helpLink;

  // JSON keys
  public static String LR_PACKAGE_ID_KEY = "packageID";
  public static String LR_RESOURCE_ID_KEY = "resourceID";
  public static String LR_RESOURCE_NAME_KEY = "resourceName";
  public static String LR_LANGUAGE_ID_KEY = "languageID";
  public static String LR_LANGUAGE_NAME_KEY = "languageName";
  public static String LR_VERSION_KEY = "version";
  public static String LR_HELP_LINK_KEY = "helpLink";

  private static final String TAG = "LanguageResource";

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

  public JSONObject toJSON() {
    JSONObject o = new JSONObject();
    try {
      o.put(LR_PACKAGE_ID_KEY, this.packageID);
      o.put(LR_RESOURCE_ID_KEY, this.resourceID);
      o.put(LR_RESOURCE_NAME_KEY, this.resourceName);
      o.put(LR_LANGUAGE_ID_KEY, this.languageID);
      o.put(LR_LANGUAGE_NAME_KEY, this.languageName);
      o.put(LR_VERSION_KEY, this.version);
      o.put(LR_HELP_LINK_KEY, this.helpLink);
    } catch (JSONException e) {
      Log.e(TAG, "toJSON() exception: " + e);
    }

    return o;
  }
}
