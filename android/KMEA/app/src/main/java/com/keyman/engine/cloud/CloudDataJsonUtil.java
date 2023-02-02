package com.keyman.engine.cloud;

import android.content.Context;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;

import com.keyman.engine.JSONParser;
import com.keyman.engine.KMKeyboardDownloaderActivity;
import com.keyman.engine.KMManager;
import com.keyman.engine.KeyboardPickerActivity;
import com.keyman.engine.R;
import com.keyman.engine.cloud.CloudApiTypes;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.data.KeyboardController;
import com.keyman.engine.data.LexicalModel;
import com.keyman.engine.util.FileUtils;
import com.keyman.engine.util.KMLog;
import com.keyman.engine.util.MapCompat;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

public class CloudDataJsonUtil {

  private static final String TAG = "CloudDataJsonUtil";

  // Deprecated
  public static final String JSON_Keyboards_Cache_Filename = "jsonKeyboardsCache.dat";

  public static final String JSON_Lexical_Models_Cache_Filename = "jsonLexicalModelsCache.dat";
  public static final String JSON_Resources_Cache_Filename = "jsonResourcesCache.json";

  // keys for api.keyman.com/package-version
  private static final String CDKey_Keyboards = "keyboards";
  private static final String CDKey_Models = "models";
  private static final String CDKey_KMP = "kmp";
  private static final String CDKey_Version = "version";
  private static final String CDKey_Error = "error";

  private CloudDataJsonUtil()
  {
    //no instances
  }

  public static HashMap<String,String> createKeyboardInfoMap(String aPackageId,String aLanguageId, String aLanguageName, String aKeyboardId,
                                                   String aKeyboardName, String aKeyboardVersion,
                                                   String aFont, String aOskFont, String aCustomHelpLink)
  {
    HashMap<String, String> keyboardInfo = new HashMap<String, String>();
    keyboardInfo.put(KMManager.KMKey_PackageID, aPackageId);
    keyboardInfo.put(KMManager.KMKey_KeyboardID, aKeyboardId);
    keyboardInfo.put(KMManager.KMKey_LanguageID, aLanguageId.toLowerCase());
    keyboardInfo.put(KMManager.KMKey_KeyboardName, aKeyboardName);
    keyboardInfo.put(KMManager.KMKey_LanguageName, aLanguageName);
    keyboardInfo.put(KMManager.KMKey_KeyboardVersion, aKeyboardVersion);
    keyboardInfo.put(KMManager.KMKey_Font, aFont);
    if (aOskFont != null) {
      keyboardInfo.put(KMManager.KMKey_OskFont, aOskFont);
    }
    if (aCustomHelpLink != null) {
      keyboardInfo.put(KMManager.KMKey_CustomHelpLink, aCustomHelpLink);
    }

    return keyboardInfo;
  }

  public static List<Keyboard> processKeyboardJSON(JSONObject query, boolean fromKMP) {
    List<Keyboard> keyboardsList = new ArrayList<>();
    if (query == null || query.length() == 0) {
      return keyboardsList;
    }

    try {
      // Thank you, Cloud API format
      JSONArray languages = query.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Languages).getJSONArray(KMKeyboardDownloaderActivity.KMKey_Languages);
      for (int i = 0; i < languages.length(); i++) {
        JSONObject languageJSON = languages.getJSONObject(i);
        JSONArray langKeyboards = languageJSON.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);

        // Can't foreach a JSONArray
        int kbLength = langKeyboards.length();
        for (int j = 0; j < kbLength; j++) {
          JSONObject keyboardJSON = langKeyboards.getJSONObject(j);
          keyboardsList.add(new Keyboard(languageJSON, keyboardJSON));
        }
      }
    } catch (JSONException | NullPointerException e) {
      KMLog.LogException(TAG, "JSONParse Error: ", e);
      return new ArrayList<Keyboard>();  // Is this ideal?
    }

    return keyboardsList;
  }

  /**
   * Parse a JSONArray of models to create a list of LexicalModel
   * @param models
   * @param fromKMP - boolean. If true, only  the first language in an array is processed
   * @return List of LexicalModel
   */
  public static List<LexicalModel> processLexicalModelJSON(JSONArray models, boolean fromKMP) {
    List<LexicalModel> modelList = new ArrayList<>(models.length());

    try {
      // Parse each model JSON Object.
      int modelsLength = models.length();
      for (int i = 0; i < modelsLength; i++) {
        JSONObject modelJSON = models.getJSONObject(i);
        modelList.addAll(LexicalModel.LexicalModelList(modelJSON, fromKMP));
      }
    } catch (JSONException | NullPointerException e) {
      KMLog.LogException(TAG, "JSONParse Error: ", e);
      return new ArrayList<LexicalModel>();  // Is this ideal?
    }

    return modelList;
  }

  /**
   * Process the "keyboards" JSON Object to determine keyboard updates
   * @param aContext Context
   * @param pkgData JSONObject from the package-version API
   * @param updateBundles - List of keyboard bundle updates
   */
  public static void processKeyboardPackageUpdateJSON(Context aContext, JSONObject pkgData, List<Bundle> updateBundles) {
    boolean saveKeyboardList = false;
    // Parse for the keyboard package updates
    if (pkgData.has(CDKey_Keyboards)) {
      try {
        JSONObject cloudKeyboardPackages = pkgData.getJSONObject(CDKey_Keyboards);
        Iterator<String> keyboardIDs = cloudKeyboardPackages.keys();
        while (keyboardIDs.hasNext()) {
          String keyboardID = keyboardIDs.next();
          JSONObject cloudKeyboardObj = cloudKeyboardPackages.getJSONObject(keyboardID);
          if (!cloudKeyboardObj.has(CDKey_Error)) {
            String cloudVersion = cloudKeyboardObj.getString(CDKey_Version);
            String cloudKMP = cloudKeyboardObj.getString(CDKey_KMP);
            // Valid keyboard package exists. See if keyboard list needs to be updated
            for (int i = 0; i < KeyboardController.getInstance().get().size(); i++) {
              Keyboard kbd = KeyboardController.getInstance().getKeyboardInfo(i);
              String version = kbd.getVersion();
              if (keyboardID.equalsIgnoreCase(kbd.getKeyboardID()) &&
                  FileUtils.compareVersions(cloudVersion, version) == FileUtils.VERSION_GREATER) {
                // Cloud catalog has newer KMP version available
                String updateKMP = kbd.getUpdateKMP();
                if (updateKMP != null) {
                  if (cloudLinkIsNewer(updateKMP, cloudKMP)) {
                    // Update keyboard info with the latest KMP link after appending languageID
                    String languageID = kbd.getLanguageID();
                    kbd.setUpdateKMP(String.format("%s&bcp47=%s", cloudKMP, languageID));
                    KeyboardController.getInstance().add(kbd);
                    saveKeyboardList = true;
                  }

                  if (!updateKMP.isEmpty()) {
                    // Update bundle list for update notifications
                    Bundle bundle = new Bundle(kbd.buildDownloadBundle());
                    updateBundles.add(bundle);
                  }
                }
              }
            }
          }
        }
      } catch (JSONException | NullPointerException e) {
        KMLog.LogException(TAG, "processPackageUpdateJSON Error process keyboards: ", e);
      }
    }

    if (saveKeyboardList) {
      KeyboardController.getInstance().save(aContext);
    }
  }

  public static void processLexicalModelPackageUpdateJSON(Context aContext, JSONObject pkgData, List<Bundle> updateBundles) {
    boolean saveModelsList = false;
    // Parse for lexical model package updates
    if (pkgData.has(CDKey_Models)) {
      try {
        JSONObject cloudModelPackages = pkgData.getJSONObject(CDKey_Models);
        Iterator<String> lexicalModelIDs = cloudModelPackages.keys();
        while (lexicalModelIDs.hasNext()) {
          String lexicalModelID = lexicalModelIDs.next();
          JSONObject cloudModelObj = cloudModelPackages.getJSONObject(lexicalModelID);
          if (!cloudModelObj.has(CDKey_Error)) {
            String cloudVersion = cloudModelObj.getString(CDKey_Version);
            String cloudKMP = cloudModelObj.getString(CDKey_KMP);
            // Valid lexical model package exists. See if lexical model list needs to be updated
            // Valid keyboard package exists. See if keyboard list needs to be updated
            int index = KeyboardPickerActivity.getLexicalModelIndex(aContext, lexicalModelID);
            if (index != -1) {
              HashMap<String, String> lmInfo = KeyboardPickerActivity.getLexicalModelInfo(aContext, index);
              String version = lmInfo.get(KMManager.KMKey_Version);
              if (lexicalModelID.equalsIgnoreCase(lmInfo.get(KMManager.KMKey_LexicalModelID)) &&
                  (FileUtils.compareVersions(cloudVersion, version) == FileUtils.VERSION_GREATER) &&
                  (!MapCompat.getOrDefault(lmInfo, KMManager.KMKey_KMPLink, "").equalsIgnoreCase(cloudKMP))) {
                // Update keyboard with the latest KMP link
                lmInfo.put(KMManager.KMKey_KMPLink, cloudKMP);
                KeyboardPickerActivity.addLexicalModel(aContext, lmInfo);

                // Update bundle list
                LexicalModel lm = new LexicalModel(
                  lmInfo.get(KMManager.KMKey_PackageID),
                  lmInfo.get(KMManager.KMKey_LexicalModelID),
                  lmInfo.get(KMManager.KMKey_LexicalModelName),
                  lmInfo.get(KMManager.KMKey_LanguageID),
                  lmInfo.get(KMManager.KMKey_LanguageName),
                  lmInfo.get(KMManager.KMKey_Version),
                  lmInfo.get(KMManager.KMKey_CustomHelpLink),
                  lmInfo.get(KMManager.KMKey_KMPLink));
                Bundle bundle = new Bundle(lm.buildDownloadBundle());
                updateBundles.add(bundle);

                saveModelsList = true;
              }
            }
          }
        }
      } catch (JSONException | NullPointerException e) {
        KMLog.LogException(TAG, "processPackageUpdateJSON Error processing models: ", e);
      }
    }
  }

  public static JSONArray getCachedJSONArray(File file) {
    JSONArray lmData = null;
    try {
      // Read from cache file
      if (file.exists()) {
        ObjectInputStream objInput = new ObjectInputStream(new FileInputStream(file));
        lmData = new JSONArray(objInput.readObject().toString());
        objInput.close();
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "getCachedJSONArray failed to read from cache file. Error: ", e);
      lmData = null;
    }

    return lmData;
  }

  public static JSONObject getCachedJSONObject(File file) {
    JSONObject kbData = null;
    try {
      // Read from cache file
      if (file.exists()) {
        StringBuilder sb = new StringBuilder(0);
        String line;
        BufferedReader objInput = new BufferedReader(new InputStreamReader(new FileInputStream(file), "UTF-8"));
        while ((line = objInput.readLine()) != null) {
          sb.append(line);
        }
        kbData = new JSONObject(sb.toString());
        objInput.close();
      }
    } catch (Exception e) {
      KMLog.LogException(TAG, "getCachedJSONObject failed to read from cache file. Error: ", e);
      kbData = null;
    }

    return kbData;
  }

  /**
   * Save the JSON catalog data that's available from the cloud.
   * The catalog is saved to a unique file.  Separate files should
   * be used for each API call, such as for keyboards vs lexical models.
   * @param json - Array of JSON objects containing API return info
   */
  public static void saveJSONArrayToCache(File file, JSONArray json) {
    ObjectOutput objOutput;
    try {
      // Save to cache file
      objOutput = new ObjectOutputStream(new FileOutputStream(file));
      objOutput.writeObject(json.toString());
      objOutput.close();
    } catch (Exception e) {
      KMLog.LogException(TAG, "Failed to save to cache file. Error: ", e);
    }
  }

  // Deprecated
  public static File getKeyboardCacheFile(Context context) {
    final String jsonCacheFilename = JSON_Keyboards_Cache_Filename;
    return new File(context.getCacheDir(), jsonCacheFilename);
  }

  public static File getLexicalModelCacheFile(Context context) {
    final String jsonLexicalCacheFilename = JSON_Lexical_Models_Cache_Filename;
    return new File(context.getCacheDir(), jsonLexicalCacheFilename);
  }

  public static File getResourcesCacheFile(Context context) {
    final String jsonCacheFilename = JSON_Resources_Cache_Filename;
    return new File(context.getCacheDir(), jsonCacheFilename);
  }

  /**
   * retrieve a json object from a downloaded file.
   * @param aDownload the download
   * @return the result
   */
  public static CloudApiTypes.CloudApiReturns retrieveJsonFromDownload(
    Context context, CloudApiTypes.SingleCloudDownload aDownload)
  {
      JSONParser jsonParser = new JSONParser();
      JSONArray dataArray = null;
      JSONObject dataObject = null;

      File destinationFile = aDownload.cacheAndOpenDestinationFile(context);
      if (destinationFile != null && destinationFile.length() > 0) {
        try {

          if (aDownload.getCloudParams().type == CloudApiTypes.JSONType.Array) {
            dataArray = jsonParser.getJSONObjectFromFile(destinationFile,JSONArray.class);
          } else {
            dataObject = jsonParser.getJSONObjectFromFile(destinationFile,JSONObject.class);
          }
        } catch (Exception e) {
          KMLog.LogException(TAG, "", e);
        } finally {
          destinationFile.delete();
        }
      } else {
        // Offline trouble!  That said, we can't get anything, so we simply shouldn't add anything.
      }

      if (aDownload.getCloudParams().type == CloudApiTypes.JSONType.Array)
      {
        if(dataArray!=null)
         return new CloudApiTypes.CloudApiReturns(aDownload.getCloudParams().target, dataArray);  // Null if offline.
        return null;
      }
      if(dataObject!=null)
          return new CloudApiTypes.CloudApiReturns(aDownload.getCloudParams().target, dataObject); // Null if offline.


    return null;
  }

  /**
   * Check if cloudKMP update link is a newer version.
   * @param updateKMP - Previously stored kmp link. Could be empty if an update wasn't available before
   * @param cloudKMP - New kmp link from the cloud query
   * @return boolean if the kmp paths match and cloud kmp version is newer
   */
  public static boolean cloudLinkIsNewer(String updateKMP, String cloudKMP) {
    if (cloudKMP == null || cloudKMP.isEmpty()) {
      if (!KMManager.isTestMode()) {
        KMLog.LogError(TAG, "cloudKMP is null or empty");
      }
      return false;
    }

    try {
      Uri cloudLink = Uri.parse(cloudKMP);

      if (updateKMP == null || updateKMP.isEmpty()) {
        return true;
      }

      Uri localLink = Uri.parse(updateKMP);

      boolean pathsMatch = localLink.getLastPathSegment().equalsIgnoreCase(cloudLink.getLastPathSegment());
      boolean cloudVersionNewer = FileUtils.compareVersions(
        localLink.getQueryParameter(CDKey_Version), cloudLink.getQueryParameter(CDKey_Version)) == FileUtils.VERSION_LOWER;

      return pathsMatch && cloudVersionNewer;
    } catch (Exception e) {
      KMLog.LogException(TAG, "Failed to compare kmp links " + updateKMP + " and " + cloudKMP, e);
    }
    return false;
  }
}
