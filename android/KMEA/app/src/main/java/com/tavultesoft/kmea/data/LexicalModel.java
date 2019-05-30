package com.tavultesoft.kmea.data;

import java.util.Map;

public class LexicalModel {
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
}
