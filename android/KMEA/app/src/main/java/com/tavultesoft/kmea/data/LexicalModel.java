package com.tavultesoft.kmea.data;

import android.os.Bundle;
import android.util.Log;

import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.util.FileUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;

public class LexicalModel extends LanguageResource implements Serializable {
  private static final String TAG = "lexicalModel";

  // Only used to build download bundle from cloud
  private String modelURL;

  // JSON key
  public static String LM_MODEL_URL_KEY = "modelURL";

  /**
   * Constructor using JSON Object from installed lexical models list
   * @param installedObj
   */
  public LexicalModel(JSONObject installedObj) {
    try {
      this.packageID = installedObj.getString(LanguageResource.LR_PACKAGE_ID_KEY);
      this.resourceID = installedObj.getString(LanguageResource.LR_RESOURCE_ID_KEY);
      this.resourceName = installedObj.getString(LanguageResource.LR_RESOURCE_NAME_KEY);
      this.languageID = installedObj.getString(LanguageResource.LR_LANGUAGE_ID_KEY);
      this.languageName = installedObj.getString(LanguageResource.LR_LANGUAGE_NAME_KEY);
      this.version = installedObj.getString(LanguageResource.LR_VERSION_KEY);
      this.helpLink = installedObj.getString(LanguageResource.LR_HELP_LINK_KEY);

      this.modelURL = installedObj.getString(LM_MODEL_URL_KEY);
    } catch (JSONException e) {
      Log.e(TAG, "JSON exception: " + e);
    }
  }

  /**
   * Constructor using JSON Object from lexical model cloud catalog
   * @param lexicalModelJSON
   * @param fromCloud boolean - only really used to make a unique prototype
   */
  public LexicalModel(JSONObject lexicalModelJSON, boolean fromCloud) {
    try {
      this.modelURL = lexicalModelJSON.optString("packageFilename", "");

      if (lexicalModelJSON.has(KMManager.KMKey_PackageID)) {
        this.packageID = lexicalModelJSON.getString(KMManager.KMKey_PackageID);
      } else if (this.modelURL != null && FileUtils.hasLexicalModelPackageExtension(this.modelURL)) {
        // Extract package ID from packageFilename
        String filename = FileUtils.getFilename(this.modelURL);
        // Truncate .model.kmp file extension
        this.packageID = filename.replace(FileUtils.MODELPACKAGE, "");
      } else {
        // Invalid Package ID
        Log.e(TAG, "Invalid package ID");
      }

      this.resourceID = lexicalModelJSON.getString(KMManager.KMKey_ID);

      this.resourceName = lexicalModelJSON.getString(KMManager.KMKey_Name);

      // language ID and language name from lexicalModelJSON
      Object obj = lexicalModelJSON.getJSONArray("languages");
      if (((JSONArray) obj).get(0) instanceof String) {
        // language name not provided so re-use language ID
        this.languageID = lexicalModelJSON.getJSONArray("languages").getString(0).toLowerCase();
        this.languageName = languageID;
      } else if (((JSONArray) obj).get(0) instanceof JSONObject) {
        JSONObject languageObj = lexicalModelJSON.getJSONArray("languages").getJSONObject(0);
        this.languageID = languageObj.getString(KMManager.KMKey_ID).toLowerCase();
        this.languageName = languageObj.getString(KMManager.KMKey_Name);
      }

      // Cloud data may not contain lexical model version, so fallback to (package) version
      String version = lexicalModelJSON.optString(KMManager.KMKey_Version, "1.0");
      this.version = lexicalModelJSON.optString(KMManager.KMKey_LexicalModelVersion, version);

      this.helpLink = ""; // TOODO: Handle help links
      this.modelURL = modelURL;
    } catch (JSONException e) {
      Log.e(TAG, "Lexical model exception parsing JSON: " + e);
    }
  }

  public LexicalModel(String packageID, String lexicalModelID, String lexicalModelName, String languageID, String languageName,
                      String version, String helpLink,
                      String modelURL) {

    this.packageID = (packageID != null) ? packageID : KMManager.KMDefault_UndefinedPackageID;
    this.resourceID = lexicalModelID;
    this.resourceName = lexicalModelName;
    this.languageID = languageID.toLowerCase();
    // If language name not provided, fallback to re-use language ID
    this.languageName = (languageName != null && !languageName.isEmpty()) ? languageName : this.languageID;

    this.version = (version != null) ? version : "1.0";
    this.helpLink = ""; // TODO: Handle help links
    this.modelURL = modelURL;
  }

  public String getLexicalModelID() { return getResourceID(); }
  public String getLexicalModelName() { return getResourceName(); }

  public Bundle buildDownloadBundle() {
    Bundle bundle = new Bundle();

    // Make sure we have an actual download URL.  If not, we can't build a proper download bundle -
    // the downloader conditions on this URL's existence in 12.0!
    if(modelURL == null) {
      return null;
    } else if (modelURL.equals("")) {
      return null;
    }

    bundle.putString(KMKeyboardDownloaderActivity.ARG_PKG_ID, packageID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_ID, resourceID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_ID, languageID);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_NAME, resourceName);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_LANG_NAME, languageName);
    bundle.putString(KMKeyboardDownloaderActivity.ARG_MODEL_URL, modelURL);

    bundle.putString(KMKeyboardDownloaderActivity.ARG_CUSTOM_HELP_LINK, helpLink);

    return bundle;
  }

  public boolean equals(Object obj) {
    if(obj instanceof LexicalModel) {
      boolean lgCodeMatch = ((LexicalModel) obj).getLanguageID().equals(this.getLanguageID());
      boolean idMatch = ((LexicalModel) obj).getLexicalModelID().equals(this.getLexicalModelID());

      return lgCodeMatch && idMatch;
    }

    return false;
  }

  public JSONObject toJSON() {
    JSONObject o = super.toJSON();
    if (o != null) {
      try {
        o.put(LM_MODEL_URL_KEY, this.modelURL);
      } catch (JSONException e) {
        Log.e(TAG, "toJSON exception: " + e);
      }
    }

    return o;
  }

  // default nrc.en.mtnt English dictionary
  public static final LexicalModel DEFAULT_LEXICAL_MODEL = new LexicalModel(
    KMManager.KMDefault_DictionaryPackageID,
    KMManager.KMDefault_DictionaryModelID,
    KMManager.KMDefault_DictionaryModelName,
    KMManager.KMDefault_LanguageID,
    KMManager.KMDefault_LanguageName,
    KMManager.KMDefault_DictionaryVersion,
    "",
    "");
}
