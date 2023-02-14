/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.keyman.engine.data;

import android.net.Uri;
import android.os.Bundle;

import com.keyman.engine.KMManager;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMString;

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
  protected String kmp; // link to latest kmp from the cloud vs what is currently installed

  // JSON keys
  private static String LR_PACKAGE_ID_KEY = "packageID";
  private static String LR_RESOURCE_ID_KEY = "resourceID";
  private static String LR_RESOURCE_NAME_KEY = "resourceName";
  private static String LR_LANGUAGE_ID_KEY = "languageID";
  private static String LR_LANGUAGE_NAME_KEY = "languageName";
  private static String LR_VERSION_KEY = "version";
  private static String LR_HELP_LINK_KEY = "helpLink";
  private static String LR_KMP_KEY = "kmp";

  private static final String TAG = "LanguageResource";

  public String getResourceID() { return resourceID; }

  public String getResourceName() { return resourceName; }

  public String getLanguageID() { return languageID; }

  // Deprecated in Keyman 14.0 by getLanguageID();
  public String getLanguageCode() { return languageID; }

  public String getLanguageName() { return languageName; }

  public void setLanguage(String languageID, String languageName) {
    this.languageID = languageID;
    this.languageName = languageName;
  }

  public String getVersion() { return version; }
  public void setVersion(String version) {
    this.version = version;
  }

  public String getPackageID() { return packageID; }

  // Deprecated in Keyman 14.0 by getPackageID()
  public String getPackage() { return packageID; }

  public String getHelpLink() { return helpLink; }

  public String getUpdateKMP() { return kmp; }
  public void setUpdateKMP(String kmp) { this.kmp = kmp; }

  /**
   * Helper method if the language resource has an updated kmp package available to download from the cloud
   * @return boolean true if an updated kmp package is available
   */
  public boolean hasUpdateAvailable() {
    boolean updateAvailable = false;
    if (kmp != null && !kmp.isEmpty()) {
      updateAvailable = true;
    }

    return updateAvailable;
  }

  public int hashCode() {
    String id = getResourceID();
    String lgCode = getLanguageID();
    if (id == null || lgCode == null) {
      KMLog.LogError("LanguageResource", "Invalid hashCode");
    }
    return id.hashCode() * lgCode.hashCode();
  }

  public String getKey() {
    return KMString.format("%s_%s", languageID, resourceID);
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
   * @param kmp
   */
  public LanguageResource(String packageID, String resourceID, String resourceName,
                          String languageID, String languageName, String version,
                          String helpLink, String kmp) {
    this.packageID = (packageID != null) ? packageID : KMManager.KMDefault_UndefinedPackageID;
    this.resourceID = resourceID;
    this.resourceName = resourceName;
    this.languageID = languageID.toLowerCase();
    // If language name not provided, fallback to re-use language ID
    this.languageName = (languageName != null && !languageName.isEmpty()) ? languageName : this.languageID;
    this.version = version;
    this.helpLink = helpLink;
    this.kmp = kmp;
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
      if (installedObj.has(LanguageResource.LR_KMP_KEY)) {
        this.kmp = installedObj.getString(LanguageResource.LR_KMP_KEY);
      } else {
        this.kmp = "";
      }
    } catch (JSONException e) {
      KMLog.LogException(TAG, "fromJSON() exception: ", e);
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
      o.put(LR_KMP_KEY, this.kmp);
    } catch (JSONException e) {
      KMLog.LogException(TAG, "toJSON() exception: ", e);
    }

    return o;
  }
}
