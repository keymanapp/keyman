package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.AssetManager;
import android.net.Uri;
import android.util.Log;
import android.widget.Toast;
import com.keyman.engine.JSONParser;
import com.keyman.engine.KMManager;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.KMLog;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;

final class FVShared {
    private static FVShared instance = null;
    private boolean isInitialized = false;

    // File containing keyboard+region info for each keyboard
    private static final String FVKeyboards_JSON = "keyboards.json";
    private static final String FVLoadedKeyboardList = "loaded_keyboards.dat";

    // Keys from earlier versions of app, used only in the upgrade process
    private static final String FVUpgrade_KeyboardList = "keyboard_list.dat";
    private static final String FVUpgrade_KeyboardListHashKey = "FVKeyboardListHash";
    private static final String FVUpgrade_Preferences = "FVPreferences";
    private static final String FVUpgrade_KeyboardFilenameKey = "FVKeyboardFilename";
    private static final String FVUpgrade_KeyboardCheckStateKey = "FVKeyboardCheckState";

    private Context context;
    private FVRegionList regionList;
    private FVLoadedKeyboardList loadedKeyboards;

    private static final String FVKeyboardHelpLink = "https://help.keyman.com/keyboard/";

    public static final String FVDefault_PackageID = "fv_all";
    public static final String TAG = "FVShared";

    /// Describes a keyboard used in FirstVoices Keyboards
    static class FVKeyboard {
        final String id, name, legacyId, version, lgId, lgName;
        FVKeyboard(String id, String name, String legacyId, String version, String lgId, String lgName) {
            this.id = id;
            this.name = name;
            this.legacyId = legacyId;
            this.version = version;
            this.lgId = lgId;
            this.lgName = lgName;
        }
    }

    static class FVKeyboardList extends ArrayList<FVKeyboard> {
        FVKeyboardList() {
            super();
        }
    }

    /// Keyboards are grouped by region in the keyboards.csv data
    /// The order of the keyboards in the csv is the final order
    /// shown everywhere in the app
    static class FVRegion {
        final String name;
        final FVKeyboardList keyboards;
        FVRegion(String name) {
            this.name = name;
            this.keyboards = new FVKeyboardList();
        }
    }

    static class FVRegionList extends ArrayList<FVRegion> {
        FVRegionList() {
            super();
        }

        FVRegion findRegion(String name) {
            for(FVRegion r : this) {
                if(r.name.equals(name)) {
                    return r;
                }
            }
            return null;
        }
    }

    static class FVLoadedKeyboardList extends ArrayList<String> {
        FVLoadedKeyboardList() {
            super();
        }
    }

    public synchronized void initialize(Context context) {
        if (isInitialized) {
          Log.w(TAG, "initialize called multiple times");
          return;
        }

        this.context = context.getApplicationContext();
        String keyboardsJSONPath = getPackagesDir() + FVDefault_PackageID + File.separator + FVKeyboards_JSON;
        this.regionList = loadRegionList(keyboardsJSONPath);
        this.loadedKeyboards = loadLoadedKeyboardList();

        isInitialized = true;
    }

  /**
   * @return get or create shared singleton instance.
   */
  public static FVShared getInstance() {
    if (instance == null) {
      instance = new FVShared();
    }
    return instance;
  }

  /**
   * Parse keyboards.json file to associate region info for each keyboard
   * @param keyboardsJSONPath - path to the keyboards.json file containing region info for each keyboard
   * @return FVRegionList
   */
  private FVRegionList loadRegionList(String keyboardsJSONPath) {
      FVRegionList list = new FVRegionList();
      JSONParser parser = new JSONParser();
      File jsonFile = new File(keyboardsJSONPath);
      if (!jsonFile.exists()) {
          // Fatal error
          throw new Error("keyboards.json file doesn't exist");
      }
      try {
          // At this point in initialization, fv_all.kmp is now extracted, so
          // populate keyboard info from keyboards.json
          JSONArray keyboardsArray = parser.getJSONObjectFromFile(jsonFile, JSONArray.class);

          if (keyboardsArray == null) {
              KMLog.LogError(TAG, "Unable to parse keyboards.json");
              return list;
          }

          for (int i=0; i<keyboardsArray.length(); i++) {
              JSONObject keyboardObj = keyboardsArray.getJSONObject(i);
              if (keyboardObj == null) {
                  KMLog.LogError(TAG, "keyboard Object in keyboards.json is null");
                  continue;
              }
              JSONArray languageArray = keyboardObj.getJSONArray("languages");
              if (languageArray == null) {
                  KMLog.LogError(TAG, "languages Array in keyboards.json is null");
                  continue;
              }
              String regionName = keyboardObj.getString("region");
              FVRegion region = list.findRegion(regionName);
              if(region == null) {
                  region = new FVRegion(regionName);
                  list.add(region);
              }

              String kbID = keyboardObj.getString("id");
              String kbName = keyboardObj.getString("name");
              JSONObject languageObj = languageArray.getJSONObject(0);
              String lgID = languageObj.getString("id").toLowerCase(); // Normalize language Id
              String lgName = languageObj.getString("name");

              // Override European keyboard info
              if (kbID.equalsIgnoreCase("sil_euro_latin")) {
                kbName = "English";
                lgID = "en";
                lgName = "English";
              } else if (kbID.equalsIgnoreCase("basic_kbdcan")) {
                kbName = "Français";
                lgID = "fr-ca";
                lgName = "French";
              }

              FVKeyboard kbd = new FVKeyboard(
                  kbID,
                  kbName,
                  kbID, // unused legacy ID
                  keyboardObj.getString("version"),
                  lgID,
                  lgName);

              region.keyboards.add(kbd);
          }
      } catch (Exception e) {
          String errorMsg = "loadRegionList had malformed keyboard list";
          KMLog.LogException(TAG, errorMsg, e);
          // Crash the app
          throw new Error(errorMsg);
      }

      // Sort by region name
      Collections.sort(list, (r1, r2) -> r1.name.compareTo(r2.name));

      for (int i=0; i<list.size(); i++) {
        // Sort by keyboard name in each region
        Collections.sort(list.get(i).keyboards, (k1, k2) -> k1.name.compareTo(k2.name));
      }
      return list;
  }

    private FVLoadedKeyboardList loadLoadedKeyboardList() {
        FVLoadedKeyboardList data = new FVLoadedKeyboardList();
        File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), FVLoadedKeyboardList);
        if (file.exists()) {
            try {
                ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
                data = (FVLoadedKeyboardList) inputStream.readObject();
                inputStream.close();
            }
            catch (Exception e) {
                Log.e("loadData", "Error: " + e);
            }
        }
        return data;
    }

    private void saveLoadedKeyboardList() {
        try {
            File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), FVLoadedKeyboardList);
            ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(file));
            outputStream.writeObject(loadedKeyboards);
            outputStream.flush();
            outputStream.close();
        }
        catch (Exception e) {
            Log.e("saveLoadedKeyboardList", "Error: " + e);
        }

        updateActiveKeyboardsList();
    }

    // Saves the migrated keyboard list for upgradeTo14()
    private void saveUpdateTo14List(ArrayList<HashMap<String, String>>list) {
        try {
            File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), KMManager.KMFilename_KeyboardsList);
            ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(file));
            outputStream.writeObject(list);
            outputStream.flush();
            outputStream.close();
        }
        catch (Exception e) {
            KMLog.LogException("saveUpdateTo14List", "Error saving migrated keyboard list", e);
        }
    }

    FVRegionList getRegionList() {
        return regionList;
    }

    int activeKeyboardCount() {
        return loadedKeyboards.size();
    }

    int activeKeyboardCount(FVRegion region) {
        int count = 0;
        for (FVKeyboard keyboard : region.keyboards) {
            if (loadedKeyboards.contains(keyboard.id)) count++;
        }

        return count;
    }

    boolean checkState(String id) {
        return loadedKeyboards.contains(id);
    }

    void setCheckState(String id, boolean isChecked) {
        // Remove the keyboard from the list first
        loadedKeyboards.remove(id);

        if(isChecked) {
            loadedKeyboards.add(id);
        }

        saveLoadedKeyboardList();
    }

    void helpAction(Context localContext, String id) {
        String helpUrl = String.format("%s%s", FVKeyboardHelpLink, id);
        Intent i = new Intent(Intent.ACTION_VIEW);
        i.setData(Uri.parse(helpUrl));
        if (i.resolveActivity(localContext.getPackageManager()) != null) {
          localContext.startActivity(i);
        } else {
            Toast.makeText(localContext, localContext.getString(R.string.unable_to_open_browser), Toast.LENGTH_SHORT).show();
        }
    }

    private void updateActiveKeyboardsList() {
      // Clear existing active keyboards list

      List<Keyboard> activeKbList = KMManager.getKeyboardsList(context);
      if (activeKbList != null) {
        int len = activeKbList.size();
        for (int i = len - 1; i >= 0; i--)
          KMManager.removeKeyboard(context, i);
      }

      File resourceRoot = new File(getResourceRoot());
      PackageProcessor kmpProcessor = new PackageProcessor(resourceRoot);

      // Recreate active keyboards list
      for (FVRegion region : regionList) {
        for (FVKeyboard keyboard : region.keyboards) {
          if (loadedKeyboards.contains(keyboard.id)) {
            // Parse kmp.json for the keyboard info
            Keyboard kbd = kmpProcessor.getKeyboard(
              FVDefault_PackageID,
              keyboard.id,
              null); // get first associated language ID
            if (kbd != null) {
              kbd.setDisplayName(keyboard.name);
              // TODO: Override fonts to NotoSansCanadianAboriginal.ttf
              KMManager.addKeyboard(context, kbd);
            }
          }
        }
      }

      activeKbList = KMManager.getKeyboardsList(context);
      if ((activeKbList != null) && (activeKbList.size() > 0)) {
        if (KMManager.getCurrentKeyboardIndex(context) < 0) {
          KMManager.setKeyboard(context, 0);
        }
      } else {
        // Add a default keyboard if none are available
        Keyboard kbInfo = KMManager.getDefaultKeyboard(context);
        KMManager.addKeyboard(context, kbInfo);
      }
    }

    private String getResourceRoot() {
        return context.getDir("data", 0).toString() + File.separator;
    }

    private String getPackagesDir() {
        return getResourceRoot() + "packages" + File.separator;
    }

    private void createDir(File dir) throws IOException {
        if (dir.exists()) {
            if (!dir.isDirectory()) {
                throw new IOException("Cannot create directory, a file is in the way");
            }
        } else {
            if(!dir.mkdirs() || !dir.isDirectory()) {
                throw new IOException("Unable to create directory");
            }
        }
    }

    private void copyAssets(String assetSource, String dest) throws IOException {
        AssetManager assetManager = context.getAssets();
        String[] items = assetManager.list(assetSource);
        if(items == null) {
            // Nothing found to copy
            return;
        }

        File dest_dir = new File(dest);
        createDir(dest_dir);

        for(String item : items) {
            String assetItem = assetSource + File.separator + item;
            String destItem = dest + File.separator + item;
            String[] subItems = assetManager.list(assetItem);
            if(subItems == null || subItems.length == 0) {
                // Is a file or empty folder, try and copy it
                InputStream in = assetManager.open(assetItem);
                OutputStream out = new FileOutputStream(destItem);

                byte[] buf = new byte[1024];
                int len;
                while ((len = in.read(buf)) > 0)
                    out.write(buf, 0, len);
                in.close();
                out.close();
            } else {
                // Is a folder,
                copyAssets(assetItem, dest + "/" + item);
            }
        }
    }

    void preloadPackages() {
        try {
            copyAssets("packages", getPackagesDir());
        }
        catch (Exception e) {
            Log.e("preloadPackages", "Error: " + e);
            // We'll squawk in this situation
        }
    }

    /// Upgrades from an earlier version, converting data structures
    /// The data structures in earlier versions were a bit messy
    @SuppressWarnings("unchecked")
    void upgradeTo12() {
        File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), FVUpgrade_KeyboardList);
        if(!file.exists()) {
            // New install, or already upgraded to 12.0
            return;
        }

        // Remove legacy preferences that are no longer required.
        SharedPreferences prefs = context.getSharedPreferences(FVUpgrade_Preferences, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = prefs.edit();
        editor.remove(FVUpgrade_KeyboardListHashKey);
        editor.apply();

        ArrayList<ArrayList<HashMap<String, String>>> keyboardList;

        try {
            ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
            keyboardList = (ArrayList<ArrayList<HashMap<String, String>>>) inputStream.readObject();
            inputStream.close();
        }
        catch (Exception e) {
            // Unable to load the data from earlier version. Log the issue, then we will
            // just delete the file and give up on it. The user will have to reconfigure
            // their keyboards but that's not the end of the world.
            Log.e("upgradeTo12", "Error: " + e);
            if(!file.delete()) {
                Log.w("upgradeTo12", "Could not remove legacy data file "+FVUpgrade_KeyboardList);
            }
            return;
        }

        // We've loaded the data, so delete the legacy file.
        if(!file.delete()) {
            Log.w("upgradeTo12", "Could not remove legacy data file "+FVUpgrade_KeyboardList);
        }

        // Iterate through the keyboards and prepare the new active keyboard list
        loadedKeyboards.clear();

        int len1 = keyboardList.size();
        for (int i = 0; i < len1; i+=2) {
            ArrayList<HashMap<String, String>>  kbSubList = keyboardList.get(i+1);
            int len2 = kbSubList.size();
            for (int j = 0; j < len2; j++) {
                HashMap<String, String> kbDict = kbSubList.get(j);
                String checkState = kbDict.get(FVUpgrade_KeyboardCheckStateKey);
                String kbFilename = kbDict.get(FVUpgrade_KeyboardFilenameKey);

                if (checkState != null && kbFilename != null && checkState.equals("Y")) {
                    // The keyboard is loaded, so find the equivalent entry in the
                    // legacyId column
                    boolean found = false;
                    for (FVRegion region : regionList) {
                        for (FVKeyboard keyboard : region.keyboards) {
                            if (keyboard.legacyId.equalsIgnoreCase(kbFilename)) {
                                loadedKeyboards.add(keyboard.id);
                                found = true;
                                break;
                            }
                        }
                        if (found) break;
                    }
                }
            }
        }

        saveLoadedKeyboardList();
    }

    /// Upgrades the keyboard list Keyman Engine for Android uses from an earlier version to 14.0
    // because the FirstVoices keyboard packageIDs changed to "fv_all".
    // Note: KMManager.initialize will eventually migrate KMFilename_KeyboardsList to a JSON file
    @SuppressWarnings("unchecked")
    void upgradeTo14() {
        String TAG = "upgradeTo14";
        File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), KMManager.KMFilename_KeyboardsList);
        if(!file.exists()) {
            // New install, or already upgraded to 14.0
            return;
        }

        ArrayList<HashMap<String, String>> keyboardList;

        try {
            ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
            keyboardList = (ArrayList<HashMap<String, String>>) inputStream.readObject();
            inputStream.close();

            // Migrate the package IDs and include OSK font
            if (keyboardList != null) {
                for(int i=0; i<keyboardList.size(); i++) {
                    HashMap<String, String> kbdMap = keyboardList.get(i);
                    kbdMap.put(KMManager.KMKey_PackageID, FVDefault_PackageID);
                    if (kbdMap.containsKey(KMManager.KMKey_Font)) {
                        kbdMap.put(KMManager.KMKey_OskFont, kbdMap.get(KMManager.KMKey_Font));
                    }
                }
            }
        }
        catch (Exception e) {
            // Unable to load and migrate the data from earlier version. Log the issue, then we will
            // just delete the file and give up on it. The user will have to reconfigure
            // their keyboards but that's not the end of the world.
            KMLog.LogException(TAG, "Unable to migrate keyboard list", e);
            if(!file.delete()) {
                Log.w(TAG, "Could not remove legacy data file "+ KMManager.KMFilename_KeyboardsList);
            }
            return;
        }

        // We've migrated the data so overwrite the legacy file
        if (keyboardList != null) {
            saveUpdateTo14List(keyboardList);
        }
    }
}
