package com.tavultesoft.kmea.packages;

import com.tavultesoft.kmea.KMManager;

import org.json.JSONObject;
import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
//import org.mockito.junit.MockitoJUnitRunner;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;

@RunWith(RobolectricTestRunner.class)
public class PackageProcessorTest {
  public static final File TEST_RESOURCE_ROOT = new File("test_resources");
  public static final File TEST_EXTRACTION_ROOT = new File(TEST_RESOURCE_ROOT, "temp");

  public static final String TEST_GFF_KMP_NAME = "gff_amh_7_test_json";
  public static final File TEST_GFF_KMP_FILE = new File(TEST_RESOURCE_ROOT, TEST_GFF_KMP_NAME + ".kmp");

  /* TODO:  Create an alternate version with a different package version; perform package overwrite tests
   * in both directions.
   */

  private static File tempPkg;

  @BeforeClass
  public static void extractTestPackages() {
    PackageProcessor.initialize(TEST_EXTRACTION_ROOT);
    try {

      tempPkg = PackageProcessor.unzipKMP(TEST_GFF_KMP_FILE);
    } catch (IOException e) {
      System.err.println(e);
    }
  }

  @Test
  public void tempPackageExtraction() {
    File infoFile = new File(tempPkg, "kmp.json");

    Assert.assertTrue(infoFile.exists());
  }

  @Test
  public void test_loadInfoJSON() throws Exception {
    JSONObject json = PackageProcessor.loadPackageInfo(tempPkg);

    Assert.assertNotNull(json);

    // Test pretty-print, for visual inspection if desired.
    System.out.println();
    System.out.println("Package version: " + json.getJSONObject("system").get("fileVersion"));
    System.out.println();
    System.out.println("System: " + json.getJSONObject("system").toString(2));
    System.out.println();
    System.out.println("Options: " + json.getJSONObject("options").toString(2));
    System.out.println();
    System.out.println("Info: " + json.getJSONObject("info").toString(2));
    System.out.println();
    System.out.println("Files: " + json.getJSONArray("files").toString(2));
    System.out.println();
    System.out.println("Keyboards: " + json.getJSONArray("keyboards").toString(2));
  }

  @Test
  public void test_load_GFF_KMP_keyboards() throws Exception {
    JSONObject json = PackageProcessor.loadPackageInfo(tempPkg);

    Assert.assertNotNull(json);

    HashMap<String, String>[] keyboards = PackageProcessor.processKeyboardsEntry(json.getJSONArray("keyboards").getJSONObject(0));
    Assert.assertEquals(2, keyboards.length);

    HashMap<String, String> amharic = new HashMap<String, String>();
    amharic.put(KMManager.KMKey_KeyboardName, "Amharic");
    amharic.put(KMManager.KMKey_KeyboardID, "gff_amh_7");
    amharic.put(KMManager.KMKey_LanguageID, "am");
    amharic.put(KMManager.KMKey_LanguageName, "Amharic");
    amharic.put(KMManager.KMKey_KeyboardVersion, "1.4");
    amharic.put(KMManager.KMKey_Font, "fantuwua.ttf");
    amharic.put(KMManager.KMKey_OskFont, "wookianos.ttf");

    Assert.assertEquals(amharic, keyboards[0]);

    HashMap<String, String> geez = new HashMap<String, String>();
    geez.put(KMManager.KMKey_KeyboardName, "Amharic");
    geez.put(KMManager.KMKey_KeyboardID, "gff_amh_7");
    geez.put(KMManager.KMKey_LanguageID, "gez");
    geez.put(KMManager.KMKey_LanguageName, "Ge'ez");
    geez.put(KMManager.KMKey_KeyboardVersion, "1.4");
    geez.put(KMManager.KMKey_Font, "fantuwua.ttf");
    geez.put(KMManager.KMKey_OskFont, "wookianos.ttf");

    Assert.assertEquals(geez, keyboards[1]);
  }

  @Test
  public void test_PathConstruction() {
    String permPath = TEST_EXTRACTION_ROOT.toString() + File.separator + "packages" + File.separator + TEST_GFF_KMP_NAME;
    Assert.assertEquals(new File(permPath), PackageProcessor.constructPath(TEST_GFF_KMP_FILE, false));

    Assert.assertNotEquals(new File(permPath), PackageProcessor.constructPath(TEST_GFF_KMP_FILE, true));
  }

  @AfterClass
  public static void eraseTestPackages() {
    PackageProcessor.clearDirectory(tempPkg);
  }
}