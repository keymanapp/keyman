package com.tavultesoft.kmea.data;

import android.os.Bundle;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;

import java.io.Serializable;
import java.util.Map;

public class LexicalModel implements Serializable, LanguageResource {
  public final Map<String, String> map;

  /* TODO:  (v13 refactor)
   * Drop the HashMap and instead directly represent the following as object properties:
   *
   *   hashMap.put(KMManager.KMKey_PackageID, packageID);
   *   hashMap.put(KMManager.KMKey_LanguageID, languageID);
   *   hashMap.put(KMManager.KMKey_LexicalModelID, modelID);
   *   hashMap.put(KMManager.KMKey_LexicalModelName, modelName);
   *   hashMap.put(KMManager.KMKey_LanguageName, langName);
   *   hashMap.put(KMManager.KMKey_LexicalModelVersion, modelVersion);
   *   hashMap.put(KMManager.KMKey_CustomModel, isCustom);
   */

  public LexicalModel(Map<String, String> modelData) {
    this.map = modelData;
  }

  public String getResourceId() {
    return this.map.get(KMManager.KMKey_LexicalModelID);
  }

  @Override
  public String getLanguageCode() {
    return this.map.get(KMManager.KMKey_LanguageID);
  }

  public String getLanguageName() {
    return this.map.get(KMManager.KMKey_LanguageName);
  }

  public String getResourceName() {
    return this.map.get(KMManager.KMKey_LexicalModelName);
  }

  public String getVersion() {
    return this.map.get(KMManager.KMKey_LexicalModelVersion);
  }

  public String getPackage() {
    return this.map.get(KMManager.KMKey_PackageID);
  }

  public Bundle buildDownloadBundle() {
    Bundle bundle = new Bundle();

    // Make sure we have an actual download URL.  If not, we can't build a proper download bundle -
    // the downloader conditions on this URL's existence in 12.0!
    String modelURL = map.get(KMManager.KMKey_LexicalModelPackageFilename);
    if(modelURL == null) {
      return null;
    } else if (modelURL.equals("")) {
      return null;
    }

    String customHelpLink = map.get(KMManager.KMKey_CustomHelpLink);

    bundle.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, getPackage());
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_ID, getResourceId());
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, getLanguageCode());
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_NAME, getResourceName());
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, getLanguageName());
    bundle.putBoolean(KMKeyboardDownloaderActivity.ARG_IS_CUSTOM, false);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_URL, modelURL);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_CUSTOM_HELP_LINK, customHelpLink);

    return bundle;
  }

  public boolean equals(Object obj) {
    if(obj instanceof LexicalModel) {
      boolean lgCodeMatch = ((LexicalModel) obj).getLanguageCode().equals(this.getLanguageCode());
      boolean idMatch = ((LexicalModel) obj).getResourceId().equals(this.getResourceId());

      return lgCodeMatch && idMatch;
    }

    return false;
  }

  @Override
  public int hashCode() {
    return getResourceId().hashCode() * getLanguageCode().hashCode();
  }
}
