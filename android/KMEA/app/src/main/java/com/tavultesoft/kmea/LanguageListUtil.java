/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.json.JSONArray;
import org.json.JSONObject;
import com.tavultesoft.kmea.cloud.CloudDataJsonUtil;
import com.tavultesoft.kmea.util.MapCompat;

import androidx.annotation.NonNull;
import android.content.Context;
import android.content.Intent;
import android.util.Log;

/**
 * "Add new keyboard"
 * Gets the list of installable languages from Keyman cloud and allows user to download a keyboard.
 * After downloading, the new keyboard is selected.
 *
 * The language list is saved to a cache file "jsonKeyboardsCache.dat".
 */
public final class LanguageListUtil {

  private Context context;

  private static final String TAG = "LanguageListUtility";

  // These two JSON objects and their getters are still used by legacy metadata functions.
  private static JSONArray languages = null;

  protected static JSONArray languages() {
    return languages;
  }

  private static JSONObject options = null;

  protected static JSONObject options() {
    return options;
  }

  // Also these.
  private static HashMap<String, HashMap<String, String>> keyboardsInfo = null;
  private static HashMap<String, String> keyboardModifiedDates = null;

  // Still used by KMManager and/or KMKeyboard.
  protected static HashMap<String, HashMap<String, String>> getKeyboardsInfo(Context context) {
    if (keyboardsInfo != null) {
      return keyboardsInfo;
    } else {
      try {
        JSONObject jsonObj = CloudDataJsonUtil.getCachedJSONObject(
          CloudDataJsonUtil.getKeyboardCacheFile(context));
        if (jsonObj == null) {
          return null;
        }

        languages = jsonObj.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Languages).getJSONArray(KMKeyboardDownloaderActivity.KMKey_Languages);
        options = jsonObj.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Options);
        keyboardsInfo = new HashMap<String, HashMap<String, String>>();
        keyboardModifiedDates = new HashMap<String, String>();

        int langLength = languages.length();
        for (int i = 0; i < langLength; i++) {
          JSONObject language = languages.getJSONObject(i);
          String kbKey = "";
          String pkgID = "";
          String kbID = "";
          String langID = language.getString(KMManager.KMKey_ID);
          String kbName = "";
          String langName = language.getString(KMManager.KMKey_Name);
          String kbVersion = "1.0";
          String kbFont = "";
          JSONArray langKeyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);
          JSONObject keyboard = null;

          int kbLength = langKeyboards.length();
          if (kbLength == 1) {
            keyboard = langKeyboards.getJSONObject(0);
            pkgID = keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
            kbID = keyboard.getString(KMManager.KMKey_ID);
            kbName = keyboard.getString(KMManager.KMKey_Name);
            kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
            kbFont = keyboard.optString(KMManager.KMKey_Font, "");

            kbKey = String.format("%s_%s", langID, kbID);
            HashMap<String, String> hashMap = new HashMap<String, String>();
            hashMap.put(KMManager.KMKey_PackageID, pkgID);
            hashMap.put(KMManager.KMKey_KeyboardName, kbName);
            hashMap.put(KMManager.KMKey_LanguageName, langName);
            hashMap.put(KMManager.KMKey_KeyboardVersion, kbVersion);
            hashMap.put(KMManager.KMKey_Font, kbFont);
            keyboardsInfo.put(kbKey, hashMap);

            if (keyboardModifiedDates.get(kbID) == null) {
              keyboardModifiedDates.put(kbID, keyboard.getString(KMManager.KMKey_KeyboardModified));
            }
          } else {
            for (int j = 0; j < kbLength; j++) {
              keyboard = langKeyboards.getJSONObject(j);
              pkgID = keyboard.optString(KMManager.KMKey_PackageID, KMManager.KMDefault_UndefinedPackageID);
              kbID = keyboard.getString(KMManager.KMKey_ID);
              kbName = keyboard.getString(KMManager.KMKey_Name);
              kbVersion = keyboard.optString(KMManager.KMKey_KeyboardVersion, "1.0");
              kbFont = keyboard.optString(KMManager.KMKey_Font, "");

              kbKey = String.format("%s_%s", langID, kbID);
              HashMap<String, String> hashMap = new HashMap<String, String>();
              hashMap.put(KMManager.KMKey_PackageID, pkgID);
              hashMap.put(KMManager.KMKey_KeyboardName, kbName);
              hashMap.put(KMManager.KMKey_LanguageName, langName);
              hashMap.put(KMManager.KMKey_KeyboardVersion, kbVersion);
              hashMap.put(KMManager.KMKey_Font, kbFont);
              keyboardsInfo.put(kbKey, hashMap);

              if (keyboardModifiedDates.get(kbID) == null) {
                keyboardModifiedDates.put(kbID, keyboard.getString(KMManager.KMKey_KeyboardModified));
              }
            }
          }
        }

        return keyboardsInfo;
      } catch (Exception e) {
        Log.e(TAG, "getKeyboardsInfo() error: " + e);
        return null;
      }
    }
  }
}