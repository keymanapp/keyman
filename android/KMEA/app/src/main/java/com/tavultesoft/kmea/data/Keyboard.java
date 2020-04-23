package com.tavultesoft.kmea.data;

import android.os.Bundle;
import android.util.Log;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.util.FileUtils;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;
import java.util.Map;

public class Keyboard implements Serializable, LanguageResource {
  private static final String TAG = "Keyboard";
  private static final String HELP_URL_FORMATSTR = "https://help.keyman.com/keyboard/%s/%s";

  private String packageID;
  private String keyboardID;
  private String keyboardName;
  private String languageID;
  private String languageName;
  private boolean isCustomKeyboard;
  private boolean isNewKeyboard;
  private String font;
  private String oskFont;
  private String helpLink;
  private String version;

  public Keyboard(JSONObject languageJSON, JSONObject keyboardJSON) {
    try {
      this.packageID = keyboardJSON.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);

      this.keyboardID = keyboardJSON.getString(KMManager.KMKey_ID);

      this.keyboardName = keyboardJSON.getString(KMManager.KMKey_Name);

      // language ID and language name from languageJSON
      this.languageID = languageJSON.getString(KMManager.KMKey_ID).toLowerCase();
      this.languageName = languageJSON.getString(KMManager.KMKey_Name);

      this.isCustomKeyboard = keyboardJSON.has(KMManager.KMKey_CustomKeyboard) &&
        keyboardJSON.get(KMManager.KMKey_CustomKeyboard).equals("Y");

      this.isNewKeyboard = keyboardJSON.has(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD) &&
        keyboardJSON.get(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD).equals(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD);

      this.font = keyboardJSON.optString(KMManager.KMKey_Font, "");

      this.oskFont = keyboardJSON.optString(KMManager.KMKey_OskFont, "");

      this.version = keyboardJSON.optString(KMManager.KMKey_KeyboardVersion, "1.0");

      this.helpLink = keyboardJSON.optString(KMManager.KMKey_CustomHelpLink,
        String.format(HELP_URL_FORMATSTR, this.keyboardID, this.version));
    } catch (JSONException e) {
      Log.e(TAG, "Keyboard exception parsing JSON: " + e);
    }
  }

  public Keyboard(String packageID, String keyboardID, String keyboardName, String languageID, String languageName,
                  boolean isCustomKeyboard, boolean isNewKeyboard,
                  String font, String oskFont, String version, String helpLink) {

    this.packageID = (packageID != null) ? packageID : KMManager.KMDefault_UndefinedPackageID;
    this.keyboardID = keyboardID;
    this.keyboardName = keyboardName;
    this.languageID = languageID.toLowerCase();
    this.languageName = languageName;
    this.isCustomKeyboard = isCustomKeyboard;
    this.isNewKeyboard = isNewKeyboard;
    this.font = (font != null) ? font : "";
    this.oskFont = (oskFont != null) ? oskFont : "";
    this.version = (version != null) ? version : "1.0";
    this.helpLink = (FileUtils.isWelcomeFile(helpLink)) ? helpLink :
      String.format(HELP_URL_FORMATSTR, this.keyboardID, this.version);
  }

  public boolean isCustomKeyboard() { return isCustomKeyboard; }

  public boolean isNewKeyboard() { return isNewKeyboard; }

  public String getResourceId() { return keyboardID; }

  public String getFont() { return font; }

  public String getOSKFont() { return oskFont; }

  @Override
  public String getLanguageCode() { return languageID; }

  public String getLanguageName() { return languageName; }

  public String getResourceName() { return keyboardName; }

  public String getCustomHelpLink() { return helpLink; }

  public String getVersion() { return version; }

  public String getPackage() { return packageID; }

  public Bundle buildDownloadBundle() {
    Bundle bundle = new Bundle();

    bundle.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, packageID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_ID, keyboardID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, languageID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_NAME, keyboardName);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, languageName);

    bundle.putBoolean(KMKeyboardDownloaderActivity.ARG_IS_CUSTOM, isCustomKeyboard);

    bundle.putString(KMKeyboardDownloaderActivity.ARG_CUSTOM_HELP_LINK, helpLink);

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
