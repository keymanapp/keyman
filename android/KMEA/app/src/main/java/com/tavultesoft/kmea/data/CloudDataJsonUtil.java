package com.tavultesoft.kmea.data;

import android.content.Context;
import android.os.Build;
import android.util.Log;

import com.tavultesoft.kmea.JSONParser;
import com.tavultesoft.kmea.KMKeyboardDownloaderActivity;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.R;
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
import java.util.List;

public class CloudDataJsonUtil {

  private static final String TAG = "CloudDataJsonUtil";
  private CloudDataJsonUtil()
  {
    //no instances
  }

  public static HashMap<String,String> createKeyboardInfoMap(String aPackageId,String aLanguageId, String aLanguageName, String aKeyboardId,
                                                   String aKeyboardName, String aKeyboardVersion, String anIsCustomKeyBoard,
                                                   String aFont, String aOskFont)
  {
    HashMap<String, String> keyboardInfo = new HashMap<String, String>();
    if(aPackageId!=null)
      keyboardInfo.put(KMManager.KMKey_PackageID, aPackageId);
    keyboardInfo.put(KMManager.KMKey_KeyboardID, aKeyboardId);
    keyboardInfo.put(KMManager.KMKey_LanguageID, aLanguageId);
    keyboardInfo.put(KMManager.KMKey_KeyboardName, aKeyboardName);
    keyboardInfo.put(KMManager.KMKey_LanguageName, aLanguageName);
    keyboardInfo.put(KMManager.KMKey_KeyboardVersion, aKeyboardVersion);
    keyboardInfo.put(KMManager.KMKey_CustomKeyboard, anIsCustomKeyBoard);
    keyboardInfo.put(KMManager.KMKey_Font, aFont);
    if (aOskFont != null)
      keyboardInfo.put(KMManager.KMKey_OskFont, aOskFont);

    return keyboardInfo;
  }

  static List<Keyboard> processKeyboardJSON(JSONObject query, boolean fromKMP) {
    List<Keyboard> keyboardsList = new ArrayList<>();
    //keyboardModifiedDates = new HashMap<String, String>();

    String isCustom = fromKMP ? "Y" : "N";

    try {
      // Thank you, Cloud API format.
      JSONArray languages = query.getJSONObject(KMKeyboardDownloaderActivity.KMKey_Languages).getJSONArray(KMKeyboardDownloaderActivity.KMKey_Languages);
      for (int i = 0; i < languages.length(); i++) {
        JSONObject language = languages.getJSONObject(i);

        String langID = language.getString(KMManager.KMKey_ID);
        String langName = language.getString(KMManager.KMKey_Name);

        JSONArray langKeyboards = language.getJSONArray(KMKeyboardDownloaderActivity.KMKey_LanguageKeyboards);

        int kbLength = langKeyboards.length();
        for (int j = 0; j < kbLength; j++) {
          JSONObject keyboardJSON = langKeyboards.getJSONObject(j);
          String kbID = keyboardJSON.getString(KMManager.KMKey_ID);
          String kbName = keyboardJSON.getString(KMManager.KMKey_Name);
          String kbVersion = keyboardJSON.optString(KMManager.KMKey_KeyboardVersion, "1.0");
          String kbFont = keyboardJSON.optString(KMManager.KMKey_Font, "");

          //String kbKey = String.format("%s_%s", langID, kbID);
          HashMap<String, String> hashMap = createKeyboardInfoMap(null,langID,langName,kbID,kbName,kbVersion,isCustom,kbFont,null);


//          if (keyboardModifiedDates.get(kbID) == null) {
//            keyboardModifiedDates.put(kbID, keyboardJSON.getString(KMManager.KMKey_KeyboardModified));
//          }

          keyboardsList.add(new Keyboard(hashMap));
        }
      }
    } catch (JSONException | NullPointerException e) {
      Log.e(TAG, "JSONParse Error: " + e);
      return new ArrayList<>();  // Is this ideal?
    }

    return keyboardsList;
  }

  static List<LexicalModel> processLexicalModelJSON(JSONArray models) {
    List<LexicalModel> modelList = new ArrayList<>(models.length());

    try {
      // Parse each model JSON Object.
      int modelsLength = models.length();
      for (int i = 0; i < modelsLength; i++) {
        JSONObject model = models.getJSONObject(i);
        String packageID = "", modelURL = "";
        if (model.has(KMManager.KMKey_PackageID)) {
          packageID = model.getString(KMManager.KMKey_PackageID);
        } else {
          // Determine package ID from packageFilename
          modelURL = model.optString("packageFilename", "");
          packageID = FileUtils.getFilename(modelURL);
          packageID = packageID.replace(FileUtils.MODELPACKAGE, "");
        }

        // api.keyman.com query returns an array of language IDs Strings while
        // kmp.json "languages" is an array of JSONObject
        String languageID = "", langName = "";
        Object obj = model.getJSONArray("languages");
        if (((JSONArray) obj).get(0) instanceof String) {
          // language name not provided, so re-use language ID
          languageID = model.getJSONArray("languages").getString(0);
          langName = languageID;
        } else if (((JSONArray) obj).get(0) instanceof JSONObject) {
          JSONObject languageObj = model.getJSONArray("languages").getJSONObject(0);
          languageID = languageObj.getString("id");
          langName = languageObj.getString("name");
        }

        String modelID = model.getString(KMManager.KMKey_ID);
        String modelName = model.getString(KMManager.KMKey_Name);
        String modelVersion = model.getString(KMManager.KMKey_LexicalModelVersion);

        String isCustom = model.optString(KMManager.KMKey_CustomModel, "N");
        
        String icon = "0";

        HashMap<String, String> hashMap = new HashMap<String, String>();
        hashMap.put(KMManager.KMKey_PackageID, packageID);
        hashMap.put(KMManager.KMKey_LanguageID, languageID);
        hashMap.put(KMManager.KMKey_LexicalModelID, modelID);
        hashMap.put(KMManager.KMKey_LexicalModelName, modelName);
        hashMap.put(KMManager.KMKey_LanguageName, langName);
        hashMap.put(KMManager.KMKey_LexicalModelVersion, modelVersion);
        hashMap.put(KMManager.KMKey_CustomModel, isCustom);
        hashMap.put(KMManager.KMKey_LexicalModelPackageFilename, modelURL);
        hashMap.put("isEnabled", "true");
        hashMap.put(KMManager.KMKey_Icon, String.valueOf(R.drawable.ic_arrow_forward));

        modelList.add(new LexicalModel(hashMap));
      }
    } catch (JSONException | NullPointerException e) {
      Log.e(TAG, "JSONParse Error: " + e);
      return new ArrayList<>();  // Is this ideal?
    }

    return modelList;
  }

   static JSONArray getCachedJSONArray(File file) {
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
  protected static void saveJSONArrayToCache(File file, JSONArray json) {
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

  /**
   * Save the JSON catalog data that's available from the cloud.
   * The catalog is saved to a unique file.  Separate files should
   * be used for each API call, such as for keyboards vs lexical models.
   * @param json - JSON object containing API return info
   */
  protected static void saveJSONObjectToCache(File file, JSONObject json) {
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

  public static File getKeyboardCacheFile(Context context) {
    final String jsonCacheFilename = "jsonKeyboardsCache.dat";
    return new File(context.getCacheDir(), jsonCacheFilename);
  }

  public static File getLexicalModelCacheFile(Context context) {
    final String jsonLexicalCacheFilename = "jsonLexicalModelsCache.dat";
    return new File(context.getCacheDir(), jsonLexicalCacheFilename);
  }

  /**
   * retrieve a json object from a downloaded file.
   * @param aDownload the download
   * @return the result
   */
  public static CloudApiTypes.CloudApiReturns retrieveJsonFromDownload( CloudApiTypes.SingleCloudDownload aDownload)
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
          Log.d(TAG, e.getMessage());
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
