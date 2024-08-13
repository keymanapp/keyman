package com.keyman.engine;

import android.util.Log;
import androidx.test.core.app.ApplicationProvider;

import com.keyman.engine.data.Keyboard;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

@RunWith(RobolectricTestRunner.class)
public class KMManagerTest {
  private static final String TAG = "KMManagerTest";
  private static final File TEST_RESOURCE_ROOT = new File("test_resources", "cloud");
  private static final String OLD_KEYBOARDS_LIST = "old_keyboards_list.dat";
  ArrayList<HashMap<String, String>> dat_list;

  // For some keyboard list tests, load an existing keyboard list.
  // Can't use @Before because context is null before running tests.
  public void loadOldKeyboardsList() {
    KMManager.initialize(ApplicationProvider.getApplicationContext(), KMManager.KeyboardType.KEYBOARD_TYPE_INAPP);

    File keyboards_dat = new File(TEST_RESOURCE_ROOT, OLD_KEYBOARDS_LIST);
    if (keyboards_dat == null || !keyboards_dat.exists()) {
      Assert.fail(TAG + ": keyboards list not found");
    }
    try {
      ObjectInputStream inputStream = new ObjectInputStream(new FileInputStream(keyboards_dat));
      dat_list = (ArrayList<HashMap<String, String>>) inputStream.readObject();
      inputStream.close();
    } catch (Exception e) {
      Log.e(TAG, "Exception reading keyboards list");
    }
  }

  @Test
  public void test_getTier() {
    String versionName = "14.0.248-alpha";
    KMManager.Tier tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.ALPHA, tier);

    versionName = "14.0.248-alpha-test";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.ALPHA, tier);

    versionName = "14.0.248-alpha-test-1234";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.ALPHA, tier);

    versionName = "14.0.248-alpha-local";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.ALPHA, tier);

    versionName = "14.0.248-beta";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.BETA, tier);

    versionName = "14.0.248-beta-test";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.BETA, tier);

    versionName = "14.0.248-beta-test-1234";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.BETA, tier);

    versionName = "14.0.248-beta-local";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.BETA, tier);

    versionName = "14.0.248-stable";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.STABLE, tier);

    versionName = "14.0.248-stable-test";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.STABLE, tier);

    versionName = "14.0.248-stable-test-1234";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.STABLE, tier);

    versionName = "14.0.248-stable-local";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.STABLE, tier);

    // If versionName is null or blank, tier based on com.keyman.engine.BuildConfig.KEYMAN_ENGINE_VERSION_NAME
    // But we can't test for it.

    // If regex fails, tier is stable
    versionName = "14.0.248";
    tier = KMManager.getTier(versionName);
    Assert.assertEquals(KMManager.Tier.STABLE, tier);
  }

  /*
  * This test is manually run to edit/regenerate the old keyboards list
  */
  @Test
  public void create_newKeyboardsList() {
    loadOldKeyboardsList();
    dat_list = new ArrayList<HashMap<String, String>>();

    HashMap<String, String> usInfo = new HashMap<String, String>();
    usInfo.put(KMManager.KMKey_PackageID, "cloud");
    usInfo.put(KMManager.KMKey_KeyboardID, "us");
    usInfo.put(KMManager.KMKey_LanguageID, "en");
    usInfo.put(KMManager.KMKey_KeyboardName, "US");
    usInfo.put(KMManager.KMKey_LanguageName, "English");
    usInfo.put(KMManager.KMKey_KeyboardVersion, "1.0");
    dat_list.add(usInfo);

    HashMap<String, String> kmInfo = new HashMap<String, String>();
    kmInfo.put(KMManager.KMKey_PackageID, "khmer_angkor");
    kmInfo.put(KMManager.KMKey_KeyboardID, "khmer_angkor");
    kmInfo.put(KMManager.KMKey_CustomKeyboard, "Y");
    kmInfo.put(KMManager.KMKey_LanguageID, "km");
    kmInfo.put(KMManager.KMKey_KeyboardName, "Khmer Angkor");
    kmInfo.put(KMManager.KMKey_LanguageName, "Central Khmer");
    kmInfo.put(KMManager.KMKey_KeyboardVersion, "1.0.6");
    dat_list.add(kmInfo);

    HashMap<String, String> europeanInfo = new HashMap<String, String>();
    europeanInfo.put(KMManager.KMKey_PackageID, "cloud");
    europeanInfo.put(KMManager.KMKey_KeyboardID, "european");
    europeanInfo.put(KMManager.KMKey_LanguageID, "en");
    europeanInfo.put(KMManager.KMKey_KeyboardName, "EuroLatin keyboard");
    europeanInfo.put(KMManager.KMKey_LanguageName, "English");
    europeanInfo.put(KMManager.KMKey_KeyboardVersion, "1.7");
    dat_list.add(europeanInfo);

    HashMap<String, String> european2Info = new HashMap<String, String>();
    european2Info.put(KMManager.KMKey_PackageID, "cloud");
    european2Info.put(KMManager.KMKey_KeyboardID, "european2");
    european2Info.put(KMManager.KMKey_LanguageID, "en");
    european2Info.put(KMManager.KMKey_KeyboardName, "European2 Keyboard");
    european2Info.put(KMManager.KMKey_LanguageName, "English");
    european2Info.put(KMManager.KMKey_KeyboardVersion, "1.6");
    dat_list.add(european2Info);

    HashMap<String, String> ipaInfo = new HashMap<String, String>();
    // Intentionally leaving package ID undefined
    ipaInfo.put(KMManager.KMKey_KeyboardID, "sil_ipa");
    ipaInfo.put(KMManager.KMKey_LanguageID, "und-fonipa");
    ipaInfo.put(KMManager.KMKey_KeyboardName, "IPA (SIL)");
    ipaInfo.put(KMManager.KMKey_LanguageName, "Undetermined");
    ipaInfo.put(KMManager.KMKey_KeyboardVersion, "1.8.1");
    dat_list.add(ipaInfo);

    HashMap<String, String> eurolatinInfo = new HashMap<String, String>();
    eurolatinInfo.put(KMManager.KMKey_PackageID, "sil_euro_latin");
    eurolatinInfo.put(KMManager.KMKey_KeyboardID, "sil_euro_latin");
    eurolatinInfo.put(KMManager.KMKey_LanguageID, "en");
    eurolatinInfo.put(KMManager.KMKey_KeyboardName, "EuroLatin (SIL)");
    eurolatinInfo.put(KMManager.KMKey_LanguageName, "English");
    eurolatinInfo.put(KMManager.KMKey_KeyboardVersion, "1.9");
    eurolatinInfo.put(KMManager.KMKey_Font, KMManager.KMDefault_KeyboardFont);
    dat_list.add(eurolatinInfo);

    try {
      File file = new File(TEST_RESOURCE_ROOT, OLD_KEYBOARDS_LIST);
      ObjectOutputStream outputStream = new ObjectOutputStream(new FileOutputStream(file));
      outputStream.writeObject(dat_list);
      outputStream.flush();
      outputStream.close();
    } catch (Exception e) {
      Log.e(TAG, "Failed to save " + OLD_KEYBOARDS_LIST, e);
    }
  }

  @Test
  public void test_updateOldKeyboardsList() {
    loadOldKeyboardsList();
    Assert.assertNotNull(dat_list);

    // Verify old keyboards list contains deprecated keyboards
    HashMap<String, String> kbInfo = dat_list.get(0);
    Assert.assertEquals("us", kbInfo.get(KMManager.KMKey_KeyboardID));
    kbInfo = dat_list.get(2);
    Assert.assertEquals("european", kbInfo.get(KMManager.KMKey_KeyboardID));
    kbInfo = dat_list.get(3);
    Assert.assertEquals("european2", kbInfo.get(KMManager.KMKey_KeyboardID));

    // Verify old keyboard list size
    int oldKeyboardListSize = dat_list.size();
    Assert.assertEquals(6, oldKeyboardListSize);

    // Verify old keyboards list contains sil_euro_latin at the end
    kbInfo = dat_list.get(oldKeyboardListSize-1);
    Assert.assertEquals("sil_euro_latin", kbInfo.get(KMManager.KMKey_KeyboardID));

    // Migrate list
    List<Keyboard> migratedList = KMManager.updateOldKeyboardsList(ApplicationProvider.getApplicationContext(), dat_list);
    //shadowOf(getMainLooper()).idle();

    // Verify migrated keyboard list size
    int migratedKeyboardListSize = migratedList.size();
    Assert.assertEquals(migratedKeyboardListSize, 3);

    // Verify deprecated keyboards don't exist in migrated keyboards list
    for (int i=0; i<migratedList.size(); i++) {
      Keyboard k = migratedList.get(i);
      Assert.assertNotEquals("us", k.getKeyboardID());
      Assert.assertNotEquals("european", k.getKeyboardID());
      Assert.assertNotEquals("european2", k.getKeyboardID());
    }

    // Verify first keyboard is now default keyboard
    Keyboard k = migratedList.get(0);
    Assert.assertEquals(KMManager.KMDefault_PackageID, k.getPackageID());
    Assert.assertEquals(KMManager.KMDefault_KeyboardID, k.getKeyboardID());
    Assert.assertEquals(KMManager.KMDefault_LanguageID, k.getLanguageID());

    // Verify last keyboard no longer sil_euro_latin
    k = migratedList.get(migratedKeyboardListSize-1);
    Assert.assertEquals(k.getKeyboardID(), "sil_ipa");

    // Verify sil_ipa now has package ID ("cloud")
    Assert.assertEquals(k.getPackageID(), KMManager.KMDefault_UndefinedPackageID);

    // Can't verify undefined version gets migrated (would need to mock keyboard files)
  }

}
