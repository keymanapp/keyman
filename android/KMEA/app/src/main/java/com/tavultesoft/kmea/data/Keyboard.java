package com.tavultesoft.kmea.data;

import android.os.Bundle;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;

import java.io.Serializable;
import java.util.Map;

public class Keyboard implements Serializable, LanguageResource {
  public final Map<String, String> map;

  /* TODO:  (v13 refactor)
   * Drop the HashMap and instead directly represent the following as object properties:
   *
   *  String kbId = kbInfo.get(KMManager.KMKey_KeyboardID);
   *  String langId = kbInfo.get(KMManager.KMKey_LanguageID);
   *  String kbName = kbInfo.get(KMManager.KMKey_KeyboardName);
   *  String langName = kbInfo.get(KMManager.KMKey_LanguageName);
   *  String kFont = kbInfo.get(KMManager.KMKey_Font);
   *  String kOskFont = kbInfo.get(KMManager.KMKey_OskFont);
   */

  public Keyboard(Map<String, String> kbdData) {
    this.map = kbdData;
  }

  public String getResourceId() {
    return this.map.get(KMManager.KMKey_KeyboardID);
  }

  @Override
  public String getLanguageCode() {
    return this.map.get(KMManager.KMKey_LanguageID);
  }

  public String getLanguageName() {
    return this.map.get(KMManager.KMKey_LanguageName);
  }

  public String getResourceName() {
    return this.map.get(KMManager.KMKey_KeyboardName);
  }

  public String getVersion() {
    return this.map.get(KMManager.KMKey_KeyboardVersion);
  }

  public String getPackage() {
    return this.map.get(KMManager.KMKey_PackageID);
  }

  public Bundle buildDownloadBundle() {
    Bundle bundle = new Bundle();
    bundle.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, getPackage());
    bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_ID, getResourceId());
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, getLanguageCode());
    bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_NAME, getResourceName());
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, getLanguageName());

    String isCustom = map.get(KMManager.KMKey_CustomKeyboard);
    if(isCustom == null) {
      isCustom = "N";
    }

    bundle.putBoolean(KMKeyboardDownloaderActivity.ARG_IS_CUSTOM, isCustom.equals("Y"));

    return bundle;
  }

  public boolean equals(Object obj) {
    if(obj instanceof Keyboard) {
      boolean lgCodeMatch = ((Keyboard) obj).getLanguageCode().equals(this.getLanguageCode());
      boolean idMatch = ((Keyboard) obj).getResourceId().equals(this.getResourceId());

      return lgCodeMatch && idMatch;
    }

    return false;
  }

  @Override
  public int hashCode() {
    String id = getResourceId();
    String lgCode = getLanguageCode();
    return id.hashCode() * lgCode.hashCode();
  }
}
