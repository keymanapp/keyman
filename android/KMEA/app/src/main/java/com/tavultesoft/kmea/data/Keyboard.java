package com.tavultesoft.kmea.data;

import com.tavultesoft.kmea.KMManager;

import java.io.Serializable;
import java.util.Map;

public class Keyboard implements Serializable, LanguageCoded {
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

  public String getId() {
    return this.map.get(KMManager.KMKey_KeyboardID);
  }

  @Override
  public String getLanguageCode() {
    return this.map.get(KMManager.KMKey_LanguageID);
  }

  public String getLanguageName() {
    return this.map.get(KMManager.KMKey_LanguageName);
  }
}
