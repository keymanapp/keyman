package com.firstvoices.keyboards;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
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
import java.security.MessageDigest;
import java.util.ArrayList;
import java.util.HashMap;

public class FVShared {
    private static ArrayList<ArrayList<HashMap<String, String>>> keyboardList = null;
    private static final String FVKeyboardList = "keyboard_list.dat";
    private static final String FVPreferences = "FVPreferences";
    private static final String FVKeyboardListHashKey = "FVKeyboardListHash";
    private static boolean didCheckHash = false;
    public static final String FVRegionNameKey = "FVRegionName";
    public static final String FVKeyboardNameKey = "FVKeyboardName";
    public static final String FVKeyboardLanguageCodeKey = "FVKeyboardLanguageCode";
    public static final String FVKeyboardFilenameKey = "FVKeyboardFilename";
    public static final String FVKeyboardCheckStateKey = "FVKeyboardCheckState";
    public static final String FVKeyboardHelpLink = "http://help.keyman.com/keyboard/";

    public static ArrayList<ArrayList<HashMap<String, String>>> getKeyboardList(Context context) {
        File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), FVKeyboardList);
        if (keyboardList == null && file.exists()) {
            try {
                ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(file));
                keyboardList = (ArrayList<ArrayList<HashMap<String, String>>>) inputStream.readObject();
                inputStream.close();
            }
            catch (Exception e) {
                Log.e("getKeyboardList", "Error: " + e);
                keyboardList = null;
            }
        }

        boolean shouldLoadFromFile = false;
        if (!didCheckHash) {
            shouldLoadFromFile = shouldLoadFromKeyboardsFile(context);
            didCheckHash = true;
        }

        if (keyboardList == null || shouldLoadFromFile) {
            ArrayList<String> activeKeyboardIdList = null;
            if (keyboardList != null) {
                int len1 = keyboardList.size();
                for (int i = 0; i < len1; i+=2) {
                    ArrayList<HashMap<String, String>>  kbSubList = keyboardList.get(i+1);
                    int len2 = kbSubList.size();
                    for (int j = 0; j < len2; j++) {
                        HashMap<String, String> kbDict = kbSubList.get(j);
                        String checkState = kbDict.get(FVKeyboardCheckStateKey);
                        if (checkState.equals("Y")) {
                            if (activeKeyboardIdList == null)
                                activeKeyboardIdList = new ArrayList<String>();

                            String kbFilename = kbDict.get(FVKeyboardFilenameKey);
                            String kbID = kbFilename.substring(0, kbFilename.lastIndexOf("-"));
                            activeKeyboardIdList.add(kbID);
                        }
                    }
                }
            }

            keyboardList = createKeyboardList(context);
            if (keyboardList != null && activeKeyboardIdList != null) {
                int len1 = keyboardList.size();
                for (int i = 0; i < len1; i+=2) {
                    ArrayList<HashMap<String, String>>  kbSubList = keyboardList.get(i+1);
                    int len2 = kbSubList.size();
                    for (int j = 0; j < len2; j++) {
                        HashMap<String, String> kbDict = kbSubList.get(j);
                        String kbFilename = kbDict.get(FVKeyboardFilenameKey);
                        String kbID = kbFilename.substring(0, kbFilename.lastIndexOf("-"));
                        if (activeKeyboardIdList.contains(kbID))
                            kbDict.put(FVKeyboardCheckStateKey, "Y");
                    }
                }

                saveKeyboardList(context);
            }
        }

        return keyboardList;
    }

    public static boolean saveKeyboardList(Context context) {
        boolean result;
        try {
            File file = new File(context.getDir("userdata", Context.MODE_PRIVATE), FVKeyboardList);
            ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(file));
            outputStream.writeObject(keyboardList);
            outputStream.flush();
            outputStream.close();
            result = true;
        }
        catch (Exception e) {
            Log.e("saveKeyboardList", "Error: " + e);
            result = false;
        }

        updateActiveKeyboardsList(context);

        return result;
    }

    public static int keyboardCount(Context context, int regionIndex) {
        if (keyboardList == null)
            keyboardList = getKeyboardList(context);

        int index = regionIndex*2 + 1;
        if (index < 0 || keyboardList == null || index >= keyboardList.size())
            return 0;

        ArrayList<HashMap<String, String>> kbSubList = keyboardList.get(index);

        return (kbSubList != null?kbSubList.size():0);
    }

    public static int activeKeyboardCount(Context context) {
        if (keyboardList == null)
            keyboardList = getKeyboardList(context);

        if (keyboardList == null)
            return 0;

        int count = 0;
        int len = keyboardList.size();
        for (int i = 0; i < len; i+=2) {
            ArrayList<HashMap<String, String>> kbSubList = keyboardList.get(i+1);
            int len2 = kbSubList.size();
            for (int j = 0; j < len2; j++) {
                HashMap<String, String> kbDict = kbSubList.get(j);
                String checkState = kbDict.get(FVKeyboardCheckStateKey);
                if (checkState.equals("Y"))
                    count++;
            }
        }

        return count;
    }

    public static int activeKeyboardCount(Context context, int regionIndex) {
        if (keyboardList == null)
            keyboardList = getKeyboardList(context);

        int index = regionIndex*2 + 1;
        if (index < 0 || keyboardList == null || index >= keyboardList.size())
            return 0;

        ArrayList<HashMap<String, String>> kbSubList = keyboardList.get(index);
        if (kbSubList == null)
            return 0;

        int count = 0;
        int len = kbSubList.size();
        for (int i = 0; i < len; i++) {
            HashMap<String, String> kbDict = kbSubList.get(i);
            String checkState = kbDict.get(FVKeyboardCheckStateKey);
            if (checkState.equals("Y"))
                count++;
        }

        return count;
    }

    public static boolean checkState(Context context, int regionIndex, int keyboardIndex) {
        if (keyboardList == null)
            keyboardList = getKeyboardList(context);

        int index = regionIndex*2 + 1;
        if (index < 0 || keyboardList == null || index >= keyboardList.size())
            return false;

        ArrayList<HashMap<String, String>> kbSubList = keyboardList.get(index);
        if (keyboardIndex < 0 || kbSubList == null || keyboardIndex >= kbSubList.size())
            return false;

        HashMap<String, String> kbDict = kbSubList.get(keyboardIndex);
        String checkState = kbDict.get(FVKeyboardCheckStateKey);

        return checkState.equals("Y");
    }

    public static boolean setCheckState(Context context, int regionIndex, int keyboardIndex, boolean isChecked) {
        if (keyboardList == null)
            keyboardList = getKeyboardList(context);

        int index = regionIndex*2 + 1;
        if (index < 0 || keyboardList == null || index >= keyboardList.size())
            return false;

        ArrayList<HashMap<String, String>> kbSubList = keyboardList.get(index);
        if (keyboardIndex < 0 || kbSubList == null || keyboardIndex >= kbSubList.size())
            return false;

        HashMap<String, String> kbDict = kbSubList.get(keyboardIndex);
        if (kbDict == null)
            return false;

        kbDict.put(FVKeyboardCheckStateKey, isChecked ? "Y" : "N");
        saveKeyboardList(context);

        return true;
    }

    public static void helpAction(Context context, int regionIndex, int keyboardIndex) {
        if (keyboardList == null)
            keyboardList = getKeyboardList(context);

        int index = regionIndex*2 + 1;
        if (index < 0 || keyboardList == null || index >= keyboardList.size())
            return;

        ArrayList<HashMap<String, String>> kbSubList = keyboardList.get(index);
        if (keyboardIndex < 0 || kbSubList == null || keyboardIndex >= kbSubList.size())
            return;

        HashMap<String, String> kbDict = kbSubList.get(keyboardIndex);
        String kbFilename = kbDict.get(FVKeyboardFilenameKey);
        String kbID = kbFilename.substring(0, kbFilename.lastIndexOf("-"));

        String helpUrl = String.format("%s%s", FVKeyboardHelpLink, kbID);
        Intent i = new Intent(Intent.ACTION_VIEW);
        i.setData(Uri.parse(helpUrl));
        context.startActivity(i);
    }

    private static ArrayList<ArrayList<HashMap<String, String>>> createKeyboardList(Context context) {
        ArrayList<ArrayList<HashMap<String, String>>> list = null;
        try {
            InputStream inputStream = context.getAssets().open("keyboards.csv");
            BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream));

            reader.readLine();
            String line = reader.readLine();
            while (line != null && line.contains(",,"))
                line = line.replace(",,", ", ,");

            while (line != null) {
                String[] values = line.split(",");
                if (values.length > 0) {
                    if (list == null)
                        list = new ArrayList<ArrayList<HashMap<String, String>>>();

                    String kbName = values[0];
                    String regionName = values[3];
                    String kbFilename = values[7].replace(".kmn", "-9.0.js");
                    String langCode = values[9];
                    HashMap<String, String> kbDict = new HashMap<String, String>();
                    kbDict.put(FVKeyboardNameKey, kbName);
                    kbDict.put(FVKeyboardLanguageCodeKey, langCode);
                    kbDict.put(FVKeyboardFilenameKey, kbFilename);
                    kbDict.put(FVKeyboardCheckStateKey, "N");

                    ArrayList<HashMap<String, String>> region = new ArrayList<HashMap<String, String>>();
                    HashMap<String, String> regionDict = new HashMap<String, String>();
                    regionDict.put(FVRegionNameKey, regionName);
                    region.add(regionDict);
                    if (!list.contains(region)) {
                        list.add(region);
                        ArrayList<HashMap<String, String>> kbList = new ArrayList<HashMap<String, String>>();
                        kbList.add(kbDict);
                        list.add(kbList);
                    }
                    else {
                        int index = list.indexOf(region) + 1;
                        ArrayList<HashMap<String, String>> kbList = list.get(index);
                        kbList.add(kbDict);
                    }
                }

                line = reader.readLine();
                while (line != null && line.contains(",,"))
                    line = line.replace(",,", ", ,");
            }

            String hash = md5(inputStream);
            inputStream.close();

            SharedPreferences prefs = context.getSharedPreferences(FVPreferences, Context.MODE_PRIVATE);
            SharedPreferences.Editor editor = prefs.edit();
            editor.putString(FVKeyboardListHashKey, hash);
            editor.commit();
        }
        catch (Exception e) {
            Log.e("createKeyboardList", "Error: " + e);
            list = null;
        }

        if (list != null)
            saveKeyboardList(context);

        return list;
    }

    private static void updateActiveKeyboardsList(Context context) {
        // Clear existing active keyboards list
        ArrayList<HashMap<String, String>> activeKbList = KMManager.getKeyboardsList(context);
        if (activeKbList != null) {
            int len = activeKbList.size();
            for (int i = len-1; i >= 0; i--)
                KMManager.removeKeyboard(context, i);
        }

        // Recreate active keyboards list
        if (keyboardList != null) {
            int len1 = keyboardList.size();
            for (int i = 0; i < len1; i+=2) {
                ArrayList<HashMap<String, String>>  kbSubList = keyboardList.get(i+1);
                int len2 = kbSubList.size();
                for (int j = 0; j < len2; j++) {
                    HashMap<String, String> kbDict = kbSubList.get(j);
                    String checkState = kbDict.get(FVKeyboardCheckStateKey);
                    if (checkState.equals("Y")) {
                        String kbFilename = kbDict.get(FVKeyboardFilenameKey);
                        String kbID = kbFilename.substring(0, kbFilename.lastIndexOf("-"));
                        HashMap<String, String> kbInfo = new HashMap<String, String>();
                        kbInfo.put(KMManager.KMKey_KeyboardID, kbID);
                        kbInfo.put(KMManager.KMKey_LanguageID, kbDict.get(FVKeyboardLanguageCodeKey));
                        kbInfo.put(KMManager.KMKey_KeyboardName, kbDict.get(FVKeyboardNameKey));
                        kbInfo.put(KMManager.KMKey_LanguageName, kbDict.get(FVKeyboardNameKey));
                        //kbInfo.put(KMManager.KMKey_KeyboardVersion, KMManager.getLatestKeyboardFileVersion(context, kbID));
                        kbInfo.put(KMManager.KMKey_Font, "NotoSansCanadianAboriginal.ttf");
                        kbInfo.put(KMManager.KMKey_CustomKeyboard, "Y");
                        kbInfo.put(KMManager.KMKey_CustomHelpLink, String.format("%s%s", FVKeyboardHelpLink, kbID));
                        KMManager.addKeyboard(context, kbInfo);
                    }
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

    private static boolean shouldLoadFromKeyboardsFile(Context context) {
        try {
            SharedPreferences prefs = context.getSharedPreferences(FVPreferences, Context.MODE_PRIVATE);
            String oldHash = prefs.getString(FVKeyboardListHashKey, null);
            if (oldHash == null)
                return true;

            InputStream inputStream = context.getAssets().open("keyboards.csv");
            String newHash = md5(inputStream);
            if (newHash == null)
                return true;

            if (oldHash.equals(newHash))
                return false;
            else
                return true;
        }
        catch (Exception e) {
            return true;
        }
    }

    private static char[] hexDigits = "0123456789abcdef".toCharArray();
    private static String md5(InputStream is) throws IOException {
        byte[] bytes = new byte[4096];
        int read = 0;
        StringBuilder sb = new StringBuilder(32);;

        try {
            MessageDigest digest = MessageDigest.getInstance("MD5");
            while ((read = is.read(bytes)) != -1) {
                digest.update(bytes, 0, read);
            }

            byte[] messageDigest = digest.digest();
            for (byte b : messageDigest) {
                sb.append(hexDigits[(b >> 4) & 0x0f]);
                sb.append(hexDigits[b & 0x0f]);
            }
        }
        catch (Exception e) {
            return null;
        }

        return sb.toString();
    }
}
