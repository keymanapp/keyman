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

public class Keyboard extends LanguageResource implements Serializable {
  private static final String TAG = "Keyboard";
  private static final String HELP_URL_FORMATSTR = "https://help.keyman.com/keyboard/%s/%s";

  private boolean isNewKeyboard;
  private String font;
  private String oskFont;

  public Keyboard(JSONObject languageJSON, JSONObject keyboardJSON) {
    try {
      this.packageID = keyboardJSON.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);

      this.resourceID = keyboardJSON.getString(KMManager.KMKey_ID);

      this.resourceName = keyboardJSON.getString(KMManager.KMKey_Name);

      // language ID and language name from languageJSON
      this.languageID = languageJSON.getString(KMManager.KMKey_ID).toLowerCase();
      this.languageName = languageJSON.getString(KMManager.KMKey_Name);

      this.isNewKeyboard = keyboardJSON.has(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD) &&
        keyboardJSON.get(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD).equals(KeyboardPickerActivity.KMKEY_INTERNAL_NEW_KEYBOARD);

      this.font = keyboardJSON.optString(KMManager.KMKey_Font, "");

      this.oskFont = keyboardJSON.optString(KMManager.KMKey_OskFont, "");

      this.version = keyboardJSON.optString(KMManager.KMKey_KeyboardVersion, "1.0");

      this.helpLink = keyboardJSON.optString(KMManager.KMKey_CustomHelpLink,
        String.format(HELP_URL_FORMATSTR, this.resourceID, this.version));
    } catch (JSONException e) {
      Log.e(TAG, "Keyboard exception parsing JSON: " + e);
    }
  }

  public Keyboard(String packageID, String keyboardID, String keyboardName, String languageID, String languageName,
                  String version, String helpLink,
                  boolean isNewKeyboard, String font, String oskFont) {

    this.packageID = (packageID != null) ? packageID : KMManager.KMDefault_UndefinedPackageID;
    this.resourceID = keyboardID;
    this.resourceName = keyboardName;
    this.languageID = languageID.toLowerCase();
    this.languageName = languageName;
    this.version = (version != null) ? version : "1.0";
    this.helpLink = (FileUtils.isWelcomeFile(helpLink)) ? helpLink :
      String.format(HELP_URL_FORMATSTR, this.resourceID, this.version);

    this.isNewKeyboard = isNewKeyboard;
    this.font = (font != null) ? font : "";
    this.oskFont = (oskFont != null) ? oskFont : "";
  }

  public String getKeyboardID() { return getResourceID(); }
  public String getKeyboardName() { return getResourceName(); }

  public boolean isNewKeyboard() { return isNewKeyboard; }

  public String getFont() { return font; }

  public String getOSKFont() { return oskFont; }

  public Bundle buildDownloadBundle() {
    Bundle bundle = new Bundle();

    bundle.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, packageID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_ID, resourceID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, languageID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_KB_NAME, resourceName);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, languageName);

    bundle.putString(KMKeyboardDownloaderActivity.ARG_CUSTOM_HELP_LINK, helpLink);

    return bundle;
  }

  public boolean equals(Object obj) {
    if(obj instanceof Keyboard) {
      boolean lgCodeMatch = ((Keyboard) obj).getLanguageID().equals(this.getLanguageID());
      boolean idMatch = ((Keyboard) obj).getKeyboardID().equals(this.getKeyboardID());

      return lgCodeMatch && idMatch;
    }

    return false;
  }

}
