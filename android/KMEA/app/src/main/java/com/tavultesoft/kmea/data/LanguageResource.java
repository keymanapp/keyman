/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmea.data;

import android.os.Bundle;
import android.util.Log;

import com.tavultesoft.kmea.KMManager;

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
  private static String LR_PACKAGE_ID_KEY = "packageID";
  private static String LR_RESOURCE_ID_KEY = "resourceID";
  private static String LR_RESOURCE_NAME_KEY = "resourceName";
  private static String LR_LANGUAGE_ID_KEY = "languageID";
  private static String LR_LANGUAGE_NAME_KEY = "languageName";
  private static String LR_VERSION_KEY = "version";
  private static String LR_HELP_LINK_KEY = "helpLink";

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

  public String getKey() {
    return String.format("%s_%s", languageID, resourceID);
  }

  public abstract Bundle buildDownloadBundle();

  public LanguageResource() {
    // Noop
  }

  /**
   * Constructor using properties
   * @param packageID
   * @param resourceID
   * @param resourceName
   * @param languageID
   * @param languageName
   * @param version
   * @param helpLink
   */
  public LanguageResource(String packageID, String resourceID, String resourceName,
                          String languageID, String languageName, String version,
                          String helpLink) {
    this.packageID = (packageID != null) ? packageID : KMManager.KMDefault_UndefinedPackageID;
    this.resourceID = resourceID;
    this.resourceName = resourceName;
    this.languageID = languageID.toLowerCase();
    // If language name not provided, fallback to re-use language ID
    this.languageName = (languageName != null && !languageName.isEmpty()) ? languageName : this.languageID;
    this.version = version;
    this.helpLink = helpLink;
  }

  protected void fromJSON(JSONObject installedObj) {
    try {
      this.packageID = installedObj.getString(LanguageResource.LR_PACKAGE_ID_KEY);
      this.resourceID = installedObj.getString(LanguageResource.LR_RESOURCE_ID_KEY);
      this.resourceName = installedObj.getString(LanguageResource.LR_RESOURCE_NAME_KEY);
      this.languageID = installedObj.getString(LanguageResource.LR_LANGUAGE_ID_KEY);
      this.languageName = installedObj.getString(LanguageResource.LR_LANGUAGE_NAME_KEY);
      this.version = installedObj.getString(LanguageResource.LR_VERSION_KEY);
      this.helpLink = installedObj.getString(LanguageResource.LR_HELP_LINK_KEY);
    } catch (JSONException e) {
      Log.e(TAG, "fromJSON() exception: " + e);
    }
  }

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
