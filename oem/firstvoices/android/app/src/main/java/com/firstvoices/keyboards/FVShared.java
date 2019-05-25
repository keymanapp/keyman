package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.res.AssetManager;
import android.net.Uri;
import android.util.Log;
import com.tavultesoft.kmea.KMManager;
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
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.HashMap;

public class FVShared {
    private static FVRegionList keyboardList = null;
    private static FVLoadedKeyboardList loadedKeyboards;

    private static final String FVLoadedKeyboardList = "loaded_keyboards.dat";
    private static final String FVPreferences = "FVPreferences";
    public static final String FVRegionNameKey = "FVRegionName";
    public static final String FVKeyboardNameKey = "FVKeyboardName";
    public static final String FVKeyboardLanguageCodeKey = "FVKeyboardLanguageCode";
    public static final String FVKeyboardFilenameKey = "FVKeyboardFilename";
    public static final String FVKeyboardCheckStateKey = "FVKeyboardCheckState";
    public static final String FVKeyboardHelpLink = "http://help.keyman.com/keyboard/";

    public static class FVKeyboard {
        public final String id, name, legacyId;
        public FVKeyboard(String id, String name, String legacyId) {
            this.id = id;
            this.name = name;
            this.legacyId = legacyId;
        }
    }

    public static class FVKeyboardList extends ArrayList<FVKeyboard> {
        public FVKeyboardList() {
            super();
        }
    }

    public static class FVRegion {
        public final String name;
        public FVKeyboardList keyboards;
        public FVRegion(String name) {
            this.name = name;
            this.keyboards = new FVKeyboardList();
        }
    }

    public static class FVRegionList extends ArrayList<FVRegion> {
        public FVRegionList() {
            super();
        }

        public FVRegion findRegion(String name) {
            for(FVRegion r : this) {
                if(r.name.equals(name)) {
                    return r;
                }
            }
            return null;
        }
    }

    public static class FVLoadedKeyboardList extends ArrayList<String> {
        public FVLoadedKeyboardList() {
            super();
        }
    }

    public static void verifyLoaded(Context context) {
        if (keyboardList == null) {
            keyboardList = createKeyboardList(context);
        }

        if (loadedKeyboards == null) {
            File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), FVLoadedKeyboardList);
            if (file.exists()) {
                try {
                    ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
                    loadedKeyboards = (FVLoadedKeyboardList) inputStream.readObject();
                    inputStream.close();
                }
                catch (Exception e) {
                    Log.e("getKeyboardList", "Error: " + e);
                    loadedKeyboards = new FVLoadedKeyboardList();
                }
            }
            else {
                loadedKeyboards = new FVLoadedKeyboardList();
            }
        }
    }

    public static FVRegionList getKeyboardList(Context context) {
        verifyLoaded(context);
        return keyboardList;
    }

    public static FVLoadedKeyboardList getLoadedKeyboardList(Context context) {
        verifyLoaded(context);
        return loadedKeyboards;
    }

    public static boolean saveLoadedKeyboardList(Context context) {
        boolean result;
        try {
            File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), FVLoadedKeyboardList);
            ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(file));
            outputStream.writeObject(loadedKeyboards);
            outputStream.flush();
            outputStream.close();
            result = true;
        }
        catch (Exception e) {
            Log.e("saveLoadedKeyboardList", "Error: " + e);
            result = false;
        }

        updateActiveKeyboardsList(context);

        return result;
    }

    public static int keyboardCount(Context context, int regionIndex) {
        FVRegionList regions = getKeyboardList(context);
        return regions.get(regionIndex).keyboards.size();
    }

    public static int activeKeyboardCount(Context context) {
        FVLoadedKeyboardList k = getLoadedKeyboardList(context);
        return k.size();
    }

    public static int activeKeyboardCount(Context context, FVRegion region) {
        if (keyboardList == null)
            keyboardList = getKeyboardList(context);

        int count = 0;
        for (FVKeyboard keyboard : region.keyboards) {
            if (loadedKeyboards.contains(keyboard.id)) count++;
        }

        return count;
    }

    public static boolean checkState(Context context, String id) {
        verifyLoaded(context);

        //String id = keyboardList.get(regionIndex).keyboards.get(keyboardIndex).id;
        return loadedKeyboards.contains(id);
    }

    public static boolean setCheckState(Context context, String id, boolean isChecked) {
        verifyLoaded(context);

        // Remove any duplicated entries, yes this may be overkill
        while(loadedKeyboards.remove(id)) {}

        if(isChecked) {
            loadedKeyboards.add(id);
        }

        saveLoadedKeyboardList(context);
        return true;
    }

    public static void helpAction(Context context, String id) {
        verifyLoaded(context);

        String helpUrl = String.format("%s%s", FVKeyboardHelpLink, id);
        Intent i = new Intent(Intent.ACTION_VIEW);
        i.setData(Uri.parse(helpUrl));
        context.startActivity(i);
    }

    private static FVRegionList createKeyboardList(Context context) {
        FVRegionList list = new FVRegionList();
        try {
            InputStream inputStream = context.getAssets().open("keyboards.csv");
            BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));

            reader.readLine(); // skip header row
            String line = reader.readLine();

            while (line != null) {
                while (line != null && line.contains(",,"))
                    line = line.replace(",,", ", ,");

                String[] values = line.split(",");
                if (values.length > 0) {
                    // Columns: shortname,id,name,region,legacyId
                    String kbId = values[1];
                    String kbName = values[2];
                    String regionName = values[3];
                    String legacyId = values[4];

                    FVRegion region = list.findRegion(regionName);
                    if(region == null) {
                        region = new FVRegion(regionName);
                        list.add(region);
                    }

                    FVKeyboard keyboard = new FVKeyboard(kbId, kbName, legacyId);

                    region.keyboards.add(keyboard);
                }

                line = reader.readLine();
            }

            inputStream.close();
        }
        catch (Exception e) {
            Log.e("createKeyboardList", "Error: " + e);
            // We'll return a malformed list for now in this situation
        }

        return list;
    }

    private static void updateActiveKeyboardsList(Context context) {
        verifyLoaded(context);

        // Clear existing active keyboards list

        ArrayList<HashMap<String, String>> activeKbList = KMManager.getKeyboardsList(context);
        if (activeKbList != null) {
            int len = activeKbList.size();
            for (int i = len-1; i >= 0; i--)
                KMManager.removeKeyboard(context, i);
        }

        // Recreate active keyboards list
        for(FVRegion region : keyboardList) {
            for(FVKeyboard keyboard : region.keyboards) {
                if(loadedKeyboards.contains(keyboard.id)) {
                    // Load the .keyboard_info file and find its first language code

                    //KeyboardInfo keyboardInfo;
                    //DO LOAD THE FILE

                    HashMap<String, String> kbInfo = new HashMap<String, String>();
                    kbInfo.put(KMManager.KMKey_PackageID, keyboard.id); //TODO: we want to share keyboard build scripts between ios and android; can we do this?
                    kbInfo.put(KMManager.KMKey_KeyboardID, keyboard.id);
                    kbInfo.put(KMManager.KMKey_LanguageID, "en"); //TODO: use language code from kmp.json
                    kbInfo.put(KMManager.KMKey_KeyboardName, keyboard.name);
                    kbInfo.put(KMManager.KMKey_LanguageName, keyboard.name);
                    kbInfo.put(KMManager.KMKey_KeyboardVersion, "1.0"); //TODO: use keyboard version from kmp.json
                    kbInfo.put(KMManager.KMKey_Font, "NotoSansCanadianAboriginal.ttf");
                    kbInfo.put(KMManager.KMKey_CustomKeyboard, "Y");
                    kbInfo.put(KMManager.KMKey_CustomHelpLink, String.format("%s%s", FVKeyboardHelpLink, keyboard.id));
                    KMManager.addKeyboard(context, kbInfo);
                }
            }
        }

        activeKbList = KMManager.getKeyboardsList(context);
        if (activeKbList != null) {
            if (activeKbList.size() > 0) {
                if (KMManager.getCurrentKeyboardIndex(context) < 0)
                    KMManager.setKeyboard(context, 0);
            }
            else {
                // Add a default keyboard in if none are available
                HashMap<String, String> kbInfo = new HashMap<String, String>();
                kbInfo.put(KMManager.KMKey_KeyboardID, KMManager.KMDefault_KeyboardID);
                kbInfo.put(KMManager.KMKey_LanguageID, KMManager.KMDefault_LanguageID);
                kbInfo.put(KMManager.KMKey_KeyboardName, KMManager.KMDefault_KeyboardName);
                kbInfo.put(KMManager.KMKey_LanguageName, KMManager.KMDefault_LanguageName);
                //kbInfo.put(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(context, KMManager.KMDefault_KeyboardID));
                kbInfo.put(KMManager.KMKey_Font, KMManager.KMDefault_KeyboardFont);
                KMManager.addKeyboard(context, kbInfo);
            }
        }
    }

    protected static String getResourceRoot(Context context) {
        return context.getDir("data", 0).toString() + File.separator;
    }

    protected static String getPackagesDir(Context context) {
        return getResourceRoot(context) + "packages" + File.separator;
    }

    private static void createDir(File dir) throws IOException
    {
        if (dir.exists())
        {
            if (!dir.isDirectory())
            {
                throw new IOException("Can't create directory, a file is in the way");
            }
        } else
        {
            dir.mkdirs();
            if (!dir.isDirectory())
            {
                throw new IOException("Unable to create directory");
            }
        }
    }
    private static void copyAssets(Context context, String assetSource, String dest) throws IOException {
        AssetManager assetManager = context.getAssets();
        String[] items = assetManager.list(assetSource);

        File dest_dir = new File(dest);
        createDir(dest_dir);

        for(String item : items) {
            String assetItem = assetSource + File.separator + item;
            String destItem = dest + File.separator + item;
            if(assetManager.list(assetItem).length == 0) {
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
                copyAssets(context, assetItem, dest + "/" + item);
            }
        }
    }

    public static void preloadPackages(Context context) {
        try {
            copyAssets(context, "packages", getPackagesDir(context));
        }
        catch (Exception e) {
            Log.e("preloadPackages", "Error: " + e);
            // We'll squawk in this situation
        }
    }
}
