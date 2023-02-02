package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.AssetManager;
import android.net.Uri;
import android.util.Log;
import android.widget.Toast;
import com.keyman.engine.KMManager;
import com.keyman.engine.data.Keyboard;
import com.keyman.engine.packages.PackageProcessor;
import com.keyman.engine.util.KMLog;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

final class FVShared {
    private static FVShared instance = null;
    private boolean isInitialized = false;

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
        this.regionList = loadRegionList();
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

  private FVRegionList loadRegionList() {
        FVRegionList list = new FVRegionList();
        try {
            // At this point in initialization, fv_all.kmp hasn't been extracted, so
            // we get all the keyboard info from keyboards.csv
            InputStream inputStream = context.getAssets().open("keyboards.csv");
            BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));

            reader.readLine(); // skip header row
            String line = reader.readLine();

            while (line != null) {
                while (line.contains(",,"))
                    line = line.replace(",,", ", ,");

                String[] values = line.split(",");
                if (values != null && values.length > 0) {
                    // Read in column info
                    String kbId = values[1];
                    String kbName = values[2];
                    String regionName = values[3];
                    String legacyId = values[4];
                    String version = values[5];
                    String lgId = values[6].toLowerCase(); // Normalize language ID
                    String lgName = values[7];

                    FVRegion region = list.findRegion(regionName);
                    if(region == null) {
                        region = new FVRegion(regionName);
                        list.add(region);
                    }

                    FVKeyboard keyboard = new FVKeyboard(kbId, kbName, legacyId, version, lgId, lgName);

                    region.keyboards.add(keyboard);
                }

                line = reader.readLine();
            }

            inputStream.close();
        } catch (Exception e) {
            Log.e("createKeyboardList", "Error: " + e);
            // We'll return a malformed list for now in this situation
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
