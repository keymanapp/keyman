/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */
package com.keyman.engine.data;

import android.content.Context;
import android.net.Uri;
import android.os.Bundle;

import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.util.BCP47;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.KMString;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class LexicalModel extends LanguageResource implements Serializable {
  private static final String TAG = "lexicalModel";

  /**
   * Constructor using JSON Object from installed lexical models list
   * @param installedObj
   */
  public LexicalModel(JSONObject installedObj) {
    this.fromJSON(installedObj);
  }

  /**
   * Construct a list of LexicalModel using JSON Object (from either installed KMP or lexical model cloud catalog).
   * Adds a model for each language found
   * @param lexicalModelJSON
   * @param fromKMP boolean - if true, only the first language in an array is processed
   */
  public static List<LexicalModel> LexicalModelList(JSONObject lexicalModelJSON, boolean fromKMP) {
    List<LexicalModel> lexicalModelList = new ArrayList<>();
    try {
      String kmp = lexicalModelJSON.optString("packageFilename", "");

      String packageID = "";
      if (lexicalModelJSON.has(KMManager.KMKey_PackageID)) {
        packageID = lexicalModelJSON.getString(KMManager.KMKey_PackageID);
      } else if (kmp != null) {
        Uri uri = Uri.parse(kmp);
        // Extract package ID from uri
        packageID = uri.getLastPathSegment();
      } else {
        // Invalid Package ID
        KMLog.LogExceptionWithData(TAG, "Invalid package ID with lexicalModelJSON",
          "lexicalModelJSON", lexicalModelJSON.toString(4), null);
      }

      String resourceID = lexicalModelJSON.getString(KMManager.KMKey_ID);

      String resourceName = lexicalModelJSON.getString(KMManager.KMKey_Name);

      // Cloud data may not contain lexical model version, so fallback to (package) version
      String version = lexicalModelJSON.optString(KMManager.KMKey_Version, "1.0");
      version = lexicalModelJSON.optString(KMManager.KMKey_LexicalModelVersion, version);

      String helpLink = lexicalModelJSON.optString(KMManager.KMKey_CustomHelpLink, "");

      // language ID and language name from lexicalModelJSON. Iterate through language array
      String languageID = "", languageName = "";
      Object obj = lexicalModelJSON.getJSONArray("languages");
      // If processing JSONObject from kmp file, only handle the first language.
      int itemsToProcess = (fromKMP) ? 1 : ((JSONArray)obj).length();
      for (int i=0; i<itemsToProcess; i++) {
        if (((JSONArray) obj).get(i) instanceof String) {
          // language name not provided so re-use language ID
          languageID = lexicalModelJSON.getJSONArray("languages").getString(i).toLowerCase();
          languageName = languageID;
        } else if (((JSONArray) obj).get(i) instanceof JSONObject) {
          JSONObject languageObj = lexicalModelJSON.getJSONArray("languages").getJSONObject(i);
          languageID = languageObj.getString(KMManager.KMKey_ID).toLowerCase();
          languageName = languageObj.getString(KMManager.KMKey_Name);
        }

        lexicalModelList.add(new LexicalModel(packageID, resourceID, resourceName,
          languageID, languageName, version, helpLink, kmp));
      }
    } catch (JSONException e) {
      KMLog.LogException(TAG, "Lexical model exception parsing JSON: ", e);
    }
    return lexicalModelList;
  }

  public LexicalModel(String packageID, String lexicalModelID, String lexicalModelName,
                      String languageID, String languageName,  String version,
                      String helpLink, String kmp) {
    super(packageID, lexicalModelID, lexicalModelName, languageID, languageName,
        version, helpLink, kmp);
  }

  @Override
  public String getKey() {
    return KMString.format("%s_%s_%s", packageID, languageID, resourceID);
  }

  public String getLexicalModelID() { return getResourceID(); }
  public String getLexicalModelName() { return getResourceName(); }

  public Bundle buildDownloadBundle() {
    Bundle bundle = new Bundle();

    // Make sure we have an actual download URL.  If not, we can't build a proper download bundle -
    // the downloader conditions on this URL's existence in 12.0!
    if(kmp == null) {
      return null;
    } else if (kmp.equals("")) {
      return null;
    }

    bundle.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, packageID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_ID, resourceID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, languageID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_NAME, resourceName);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, languageName);

    bundle.putString(KMKeyboardDownloaderActivity.ARG_CUSTOM_HELP_LINK, helpLink);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_KMP_LINK, kmp);

    return bundle;
  }

  public boolean equals(Object obj) {
    if(obj instanceof LexicalModel) {
      boolean lgCodeMatch = BCP47.languageEquals(((LexicalModel) obj).getLanguageID(), this.getLanguageID());
      boolean idMatch = ((LexicalModel) obj).getLexicalModelID().equals(this.getLexicalModelID());

      return lgCodeMatch && idMatch;
    }

    return false;
  }

  protected void fromJSON(JSONObject installedObj) {
    super.fromJSON(installedObj);
  }

  public JSONObject toJSON() {
    return super.toJSON();
  }

  // default nrc.en.mtnt English dictionary
  public static final LexicalModel getDefaultLexicalModel(Context context) {
    String version = KMManager.getLexicalModelPackageVersion(
      context, KMManager.KMDefault_DictionaryPackageID);

    return new LexicalModel(
      KMManager.KMDefault_DictionaryPackageID,
      KMManager.KMDefault_DictionaryModelID,
      KMManager.KMDefault_DictionaryModelName,
      KMManager.KMDefault_LanguageID,
      KMManager.KMDefault_LanguageName,
      version,
      "", // help link
      ""); // kmp link
  }
}
