/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.tavultesoft.kmea.data;

import android.content.Context;
import android.os.Bundle;

import androidx.annotation.NonNull;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.KeyboardPickerActivity;
import com.tavultesoft.kmea.util.BCP47;
import com.tavultesoft.kmea.util.FileUtils;
import com.tavultesoft.kmea.util.KMLog;
import com.tavultesoft.kmea.util.KMString;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.Serializable;

public class Keyboard extends LanguageResource implements Serializable {
  private static final String TAG = "Keyboard";
  public static final String HELP_URL_HOST = "https://help.keyman.com/";
  private static final String HELP_URL_FORMATSTR = "%s/keyboard/%s/%s";

  private boolean isNewKeyboard;
  private String font;
  private String oskFont;

  // JSON keys
  private static String KB_NEW_KEYBOARD_KEY = "isNewKeyboard";
  private static String KB_FONT_KEY = "font";
  private static String KB_OSK_FONT_KEY = "oskFont";

  private static Keyboard FALLBACK_KEYBOARD;

  /**
   * Constructor using JSON Objects from installed keyboards list
   * @param installedObj
   */
  public Keyboard(JSONObject installedObj) {
    this.fromJSON(installedObj);
  }

  /**
   * Constructor using JSON Objects from keyboard cloud catalog
   * @param languageJSON
   * @param keyboardJSON
   */
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

      this.version = keyboardJSON.optString(KMManager.KMKey_KeyboardVersion, null);

      this.helpLink = keyboardJSON.optString(KMManager.KMKey_CustomHelpLink,
        KMString.format(HELP_URL_FORMATSTR, HELP_URL_HOST, this.resourceID, this.version));
    } catch (JSONException e) {
      KMLog.LogException(TAG, "Keyboard exception parsing JSON: ", e);
    }
  }

  public Keyboard(String packageID, String keyboardID, String keyboardName,
                  String languageID, String languageName, String version,
                  String helpLink, String kmp,
                  boolean isNewKeyboard, String font, String oskFont) {
    super(packageID, keyboardID, keyboardName, languageID, languageName, version,
      (FileUtils.isWelcomeFile(helpLink)) ? helpLink :
        KMString.format(HELP_URL_FORMATSTR, HELP_URL_HOST, keyboardID, version),
      kmp);

    this.isNewKeyboard = isNewKeyboard;
    this.font = (font != null) ? font : "";
    this.oskFont = (oskFont != null) ? oskFont : "";
  }

  public Keyboard(Keyboard k) {
    super(k.getPackageID(), k.getKeyboardID(), k.getKeyboardName(),
      k.getLanguageID(), k.getLanguageName(), k.getVersion(),
      k.getHelpLink(), k.getUpdateKMP());
    this.isNewKeyboard = false;
    this.font = k.getFont();
    this.oskFont = k.getOSKFont();
  }

  public String getKeyboardID() { return getResourceID(); }
  public String getKeyboardName() { return getResourceName(); }

  public boolean getNewKeyboard() { return isNewKeyboard; }
  public void setNewKeyboard(boolean isNewKeyboard) { this.isNewKeyboard = isNewKeyboard; }

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
    bundle.putString(KMKeyboardDownloaderActivity.ARG_KMP_LINK, kmp);

    return bundle;
  }

  public boolean equals(Object obj) {
    if(obj instanceof Keyboard) {
      boolean lgCodeMatch = BCP47.languageEquals(((Keyboard) obj).getLanguageID(), this.getLanguageID());
      boolean idMatch = ((Keyboard) obj).getKeyboardID().equals(this.getKeyboardID());

      return lgCodeMatch && idMatch;
    }

    return false;
  }

  protected void fromJSON(JSONObject installedObj) {
    super.fromJSON(installedObj);
    try {
      this.isNewKeyboard = installedObj.getBoolean(KB_NEW_KEYBOARD_KEY);
      this.font = installedObj.getString(KB_FONT_KEY);
      this.oskFont = installedObj.getString(KB_OSK_FONT_KEY);
    } catch (JSONException e) {
      KMLog.LogException(TAG, "fromJSON exception: ", e);
    }
  }

  public JSONObject toJSON() {
    JSONObject o = super.toJSON();
    if (o != null) {
      try {
        o.put(KB_NEW_KEYBOARD_KEY, this.isNewKeyboard);
        o.put(KB_FONT_KEY, this.font);
        o.put(KB_OSK_FONT_KEY, this.oskFont);
      } catch (JSONException e) {
        KMLog.LogException(TAG, "toJSON exception: ", e);
      }
    }
    return o;
  }

  /**
   * Get the fallback keyboard. If never specified, use sil_euro_latin
   * @param context Context
   * @return Keyboard - the fallback keyboard
   */
  public static Keyboard getDefaultKeyboard(@NonNull Context context) {
    if (context == null) {
      KMLog.LogError(TAG, "getDefaultKeyboard with null context");
    }
    if (FALLBACK_KEYBOARD == null) {
      String version = KMManager.getLatestKeyboardFileVersion(
        context, KMManager.KMDefault_PackageID, KMManager.KMDefault_KeyboardID);

      // If local help file doesn't exist, it will default to help.keyman.com link
      File helpFile = new File(KMManager.getPackagesDir(),
        KMManager.KMDefault_PackageID + File.separator + FileUtils.WELCOME_HTM);
      String helpFileStr = helpFile.exists() ? helpFile.toString() : null;

      FALLBACK_KEYBOARD = new Keyboard(
        KMManager.KMDefault_PackageID,
        KMManager.KMDefault_KeyboardID,
        KMManager.KMDefault_KeyboardName,
        KMManager.KMDefault_LanguageID,
        KMManager.KMDefault_LanguageName,
        version,
        helpFileStr,
        "",
        false,
        KMManager.KMDefault_KeyboardFont,
        KMManager.KMDefault_KeyboardFont);
    }
    return FALLBACK_KEYBOARD;
  }

  /**
   * Set the fallback keyboard. If keyboard is null, the fallback keyboard will
   * revert to sil_euro_latin
   * @param k Keyboard to set as the fallback keyboard
   */
  public static void setDefaultKeyboard(Keyboard k) {
    // If k is null, getDefaultKeyboard() will regenerate fallback keyboard sil_euro_latin
    FALLBACK_KEYBOARD = k;
  }
}
