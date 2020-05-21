package com.tavultesoft.kmea.cloud;

import android.content.Context;
import android.os.Build;
import android.util.Log;

import com.tavultesoft.kmea.JSONParser;
import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.R;
import com.tavultesoft.kmea.cloud.CloudApiTypes;
import com.tavultesoft.kmea.data.Keyboard;
import com.tavultesoft.kmea.data.KeyboardController;
import com.tavultesoft.kmea.data.LexicalModel;
import com.tavultesoft.kmea.util.FileUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
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
      if (fromKMP) {
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
      } else {
        // Thank you, Cloud API format.
        JSONObject keyboardsJSON = query.getJSONObject(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);
        String keyboardID = keyboardsJSON.keys().next();
        JSONObject keyboardJSON = keyboardsJSON.getJSONObject(keyboardID);
        String version = keyboardJSON.getString(KMManager.KMKey_Version);
        String kmp = keyboardJSON.getString("kmp");
        String packageID = FileUtils.getFilename(kmp).substring(0, kmp.indexOf(FileUtils.KEYMANPACKAGE));

        //keyboardsList.add(new Keyboard(packageID,))


      }
    } catch (JSONException | NullPointerException e) {
      Log.e(TAG, "JSONParse Error: " + e);
      return new ArrayList<Keyboard>();  // Is this ideal?
    }

    return keyboardsList;
  }

  public static List<LexicalModel> processLexicalModelJSON(JSONArray models) {
    List<LexicalModel> modelList = new ArrayList<>(models.length());

    try {
      // Parse each model JSON Object.
      int modelsLength = models.length();
      for (int i = 0; i < modelsLength; i++) {
        JSONObject modelJSON = models.getJSONObject(i);
        modelList.add(new LexicalModel(modelJSON, true));
      }
    } catch (JSONException | NullPointerException e) {
      Log.e(TAG, "JSONParse Error: " + e);
      return new ArrayList<LexicalModel>();  // Is this ideal?
    }

    return modelList;
  }

  public static void processKeyboardPackageUpdateJSON(Context aContext, JSONObject pkgData) {
    boolean saveKeyboardList = false;
    // Parse for the keyboard package updates
    if (pkgData.has("keyboards")) {
      try {
        JSONObject cloudKeyboardPackages = pkgData.getJSONObject("keyboards");
        Iterator<String> keyboardIDs = cloudKeyboardPackages.keys();
        while (keyboardIDs.hasNext()) {
          String keyboardID = keyboardIDs.next();
          JSONObject cloudKeyboardObj = cloudKeyboardPackages.getJSONObject(keyboardID);
          if (!cloudKeyboardObj.has("error")) {
            String cloudVersion = cloudKeyboardObj.getString("version");
            String cloudKMP = cloudKeyboardObj.getString("kmp");
            // Valid keyboard package exists. See if keyboard list needs to be updated
            for (int i = 0; i < KeyboardController.getInstance().get().size(); i++) {
              Keyboard kbd = KeyboardController.getInstance().getKeyboardInfo(i);
              String version = kbd.getVersion();
              if (keyboardID.equalsIgnoreCase(kbd.getKeyboardID()) &&
                FileUtils.compareVersions(cloudVersion, version) == FileUtils.VERSION_GREATER) {
                // Update keyboard with the KMP link
                kbd.setKMP(cloudKMP);
                KeyboardController.getInstance().add(kbd);
                saveKeyboardList = true;
              }
            }
          }
        }
      } catch (JSONException | NullPointerException e) {
        Log.e(TAG, "processPackageUpdateJSON Error process keyboards: " + e);
      }
    }

    if (saveKeyboardList) {
      KeyboardController.getInstance().save(aContext);
    }
  }

  public static void processLexicalModelPackageUpdateJSON(Context aContext, JSONObject pkgData) {
    // Parse for lexical model package updates
    if (pkgData.has("models")) {
      try {
        JSONArray modelPackages = pkgData.getJSONArray("models");
      } catch (JSONException | NullPointerException e) {
        Log.e(TAG, "processPackageUpdateJSON Error processing models: " + e);
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
      Log.e(TAG, "Failed to read from cache file. Error: " + e);
      lmData = null;
    }

    return lmData;
  }

  public static JSONObject getCachedJSONObject(File file) {
    JSONObject kbData = null;
    try {
      // Read from cache file
      if (file.exists()) {
        ObjectInputStream objInput = new ObjectInputStream(new FileInputStream(file));
        kbData = new JSONObject(objInput.readObject().toString());
        objInput.close();
      }
    } catch (Exception e) {
      Log.e(TAG, "Failed to read from cache file. Error: " + e);
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
      Log.e(TAG, "Failed to save to cache file. Error: " + e);
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
  public static CloudApiTypes.CloudApiReturns retrieveJsonFromDownload(CloudApiTypes.SingleCloudDownload aDownload)
  {
      JSONParser jsonParser = new JSONParser();
      JSONArray dataArray = null;
      JSONObject dataObject = null;

      if (aDownload.getDestinationFile() != null && aDownload.getDestinationFile().length() > 0) {
        try {

          if (aDownload.getCloudParams().type == CloudApiTypes.JSONType.Array) {
            dataArray = jsonParser.getJSONObjectFromFile(aDownload.getDestinationFile(),JSONArray.class);
          } else {
            dataObject = jsonParser.getJSONObjectFromFile(aDownload.getDestinationFile(),JSONObject.class);
          }
        } catch (Exception e) {
          Log.d(TAG, e.getMessage(),e);
        } finally {
          aDownload.getDestinationFile().delete();
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

  // If a font JSONObject contains multiple font font files, only keep the .ttf source
  public static void updateFontSourceToTTFFont(JSONObject jsonFont) {
    boolean updateJsonFont = false;
    try {
      JSONArray fontSource = jsonFont.optJSONArray(KMManager.KMKey_FontSource);
      if ((fontSource != null) && hasTTFFont(fontSource)) {
        for (int i = fontSource.length() - 1; i >= 0; i--) {
          String s = fontSource.getString(i);
          if (!FileUtils.isTTFFont(s)) {
            updateJsonFont = true;
            // remove() was added in API 19
            // https://developer.android.com/reference/org/json/JSONArray#remove(int)
            if (Build.VERSION.SDK_INT > 19) {
              fontSource.remove(i);
            } else {
              fontSource = removeJsonObjectAtIndex(fontSource, i);
            }
          }
        }

        if (updateJsonFont) {
          JSONArray copy = fontSource;
          jsonFont.remove(KMManager.KMKey_FontSource);
          jsonFont.put(KMManager.KMKey_FontSource, copy);
        }
      }
    } catch (JSONException e) {
      Log.e(TAG, "findTTF exception" + e);
    }
  }

  // Parse the fontSource JSONArray to see if it contains a .ttf font
  private static boolean hasTTFFont(JSONArray fontSource) {
    try {
      for (int i = 0; i < fontSource.length(); i++) {
        String s = fontSource.getString(i);
        if (FileUtils.isTTFFont(s)) {
          return true;
        }
      }
      return false;
    } catch (JSONException e) {
      Log.e(TAG, "hasTTFFont exception" + e);
      return false;
    }
  }

  // From https://stackoverflow.com/questions/27427999/remove-jsonobeject-before-android-api-lvl-19
  private static JSONArray removeJsonObjectAtIndex(JSONArray source, int index) throws JSONException {
    if (index < 0 || index > source.length() - 1) {
      throw new IndexOutOfBoundsException();
    }

    final JSONArray copy = new JSONArray();
    for (int i=0, count = source.length(); i<count; i++) {
      if (i != index) {
        copy.put(source.get(i));
      }
    }
    return copy;
  }

  public static ArrayList<String> fontUrls(JSONObject jsonFont, String baseUri, boolean isOskFont) {
    if (jsonFont == null)
      return null;

    ArrayList<String> urls = new ArrayList<String>();
    JSONArray fontSource = jsonFont.optJSONArray(KMManager.KMKey_FontSource);
    if (fontSource != null) {
      int fcCount = fontSource.length();
      for (int i = 0; i < fcCount; i++) {
        String fontSourceString;
        try {
          fontSourceString = fontSource.getString(i);

          if (FileUtils.hasFontExtension(fontSourceString)) {
            urls.add(baseUri + fontSourceString);
          } else if (isOskFont && FileUtils.hasSVGViewBox(fontSourceString)) {
            String fontFilename = FileUtils.getSVGFilename(fontSourceString);
            urls.add(baseUri + fontFilename);
          }
        } catch (JSONException e) {
          return null;
        }
      }
    } else {
      String fontSourceString;
      try {
        fontSourceString = jsonFont.getString(KMManager.KMKey_FontSource);
        if (FileUtils.hasFontExtension(fontSourceString)) {
          urls.add(baseUri + fontSourceString);
        } else if (isOskFont && FileUtils.hasSVGViewBox(fontSourceString)) {
          String fontFilename = FileUtils.getSVGFilename(fontSourceString);
          urls.add(baseUri + fontFilename);
        }
      } catch (JSONException e) {
        return null;
      }
    }

    return urls;
  }

  public static JSONObject findMatchingKeyboardByID(JSONArray aKeyboards, String aKbId)
      throws  IllegalStateException, JSONException
  {
    if (aKeyboards == null) {
      throw new IllegalStateException("Keyboard array is empty");
    }

    JSONObject keyboard = null;
    // In case keyboards array contains multiple keyboards, get the one with matching keyboard ID
    for (int index = 0; index < aKeyboards.length(); index++) {
      keyboard = aKeyboards.getJSONObject(index);
      if (keyboard != null && (aKbId.equals(keyboard.getString(KMManager.KMKey_ID)))) {
        break;
      }
    }
    if (keyboard == null) {
      throw new IllegalStateException("could not find matching keyboard");
    }

    return keyboard;
  }

  /**
   * select device type for api queries.
   * @param aContext a context
   * @return the result
   */
  public static String getDeviceTypeForCloudQuery(Context aContext)
  {
    String deviceType = aContext.getString(R.string.device_type);
    if (deviceType.equals("AndroidTablet")) {
      deviceType = "androidtablet";
    } else {
      deviceType = "androidphone";
    }
    return deviceType;
  }

}
