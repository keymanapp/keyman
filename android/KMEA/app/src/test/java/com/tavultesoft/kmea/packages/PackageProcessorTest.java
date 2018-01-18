package com.tavultesoft.kmea.packages;

import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.util.ZipUtils;

import org.apache.commons.io.FileUtils;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

@RunWith(RobolectricTestRunner.class)
public class PackageProcessorTest {
  private static final File TEST_RESOURCE_ROOT = new File("test_resources");
  private static final File TEST_EXTRACTION_ROOT = new File(TEST_RESOURCE_ROOT, "temp");

  private static final String TEST_GFF_KMP_NAME = "gff_amh_7_test_json";
  private static final File TEST_GFF_KMP_FILE = new File(TEST_RESOURCE_ROOT, TEST_GFF_KMP_NAME + ".kmp");
  private static final File TEST_GFF_KMP_TARGET = new File(TEST_EXTRACTION_ROOT, "packages" +
    File.separator + TEST_GFF_KMP_NAME);

  private static final String TEST_GFF_KMP_NAME_ALT = TEST_GFF_KMP_NAME;
  private static final File TEST_GFF_KMP_FILE_ALT = new File(TEST_RESOURCE_ROOT, "temp" + File.separator + TEST_GFF_KMP_NAME_ALT + ".kmp");
  private static final File TEST_GFF_KMP_TARGET_ALT = new File(TEST_EXTRACTION_ROOT, "packages" +
    File.separator + TEST_GFF_KMP_NAME_ALT);

  private static final int TEST_GFF_KBD_COUNT = 2;
  private static final String TEST_GFF_KBD_ID = "gff_amh_7";

  private static File tempPkg;

  /**
   * Uses the existing sample KMP's kmp.info file as a base, constructing a second, altered KMP package
   * for use in version tests between same-id packages.  Does not reconstruct the .zip for actual installation
   * tests.
   * @throws Exception
   */
  private static void createAlternateKMP() throws Exception {
    FileUtils.copyFile(TEST_GFF_KMP_FILE, TEST_GFF_KMP_FILE_ALT);
    try {
      File tempPkgAlt = PackageProcessor.unzipKMP(TEST_GFF_KMP_FILE_ALT);

      JSONObject json = PackageProcessor.loadPackageInfo(tempPkgAlt);
      json.getJSONObject("system").put("fileVersion", "8.0"); // Make it look newer!

      // Write out the JSON file.
      File jsonFile = new File(tempPkgAlt, "kmp.json");
      BufferedWriter writer = new BufferedWriter(new FileWriter(jsonFile, false));
      writer.write(json.toString(2));
      writer.close();

      // Write the JSON file.
    } catch (IOException e) {
      System.err.println(e);
    }

    if(!TEST_GFF_KMP_TARGET.equals(TEST_GFF_KMP_TARGET_ALT)) {
      throw new RuntimeException("Generated alternate test KMP not configured properly!");
    }
  }

  // Each test gets a fresh version of the extracted package.
  @Before
  public void extractTestPackages() {
    PackageProcessor.initialize(TEST_EXTRACTION_ROOT);
    try {
      tempPkg = PackageProcessor.unzipKMP(TEST_GFF_KMP_FILE);
    } catch (IOException e) {
      System.err.println(e);
    }
  }

  /**
   * Post-test cleanup.  While the temp/ directory is .gitignore'd, this provides an extra layer
   * of safety from polluting the repo file path.
   * @throws IOException
   */
  @After
  public void eraseTestPackages() throws IOException {
    FileUtils.deleteDirectory(tempPkg);
    FileUtils.deleteDirectory(TEST_GFF_KMP_TARGET);

    FileUtils.deleteQuietly(TEST_GFF_KMP_FILE_ALT);
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
//
//    // Test pretty-print, for visual inspection if desired.
//    System.out.println();
//    System.out.println("Package version: " + json.getJSONObject("system").get("fileVersion"));
//    System.out.println();
//    System.out.println("System: " + json.getJSONObject("system").toString(2));
//    System.out.println();
//    System.out.println("Options: " + json.getJSONObject("options").toString(2));
//    System.out.println();
//    System.out.println("Info: " + json.getJSONObject("info").toString(2));
//    System.out.println();
//    System.out.println("Files: " + json.getJSONArray("files").toString(2));
//    System.out.println();
//    System.out.println("Keyboards: " + json.getJSONArray("keyboards").toString(2));
  }

  @Test
  public void test_load_GFF_KMP_keyboards() throws Exception {
    JSONObject json = PackageProcessor.loadPackageInfo(tempPkg);

    Assert.assertNotNull(json);

    Map<String, String>[] keyboards = PackageProcessor.processKeyboardsEntry(json.getJSONArray("keyboards").getJSONObject(0), "gff_amh_7_test");
    Assert.assertEquals(2, keyboards.length);

    HashMap<String, String> amharic = new HashMap<String, String>();
    amharic.put(KMManager.KMKey_PackageID, "gff_amh_7_test");
    amharic.put(KMManager.KMKey_KeyboardName, "Amharic");
    amharic.put(KMManager.KMKey_KeyboardID, "gff_amh_7");
    amharic.put(KMManager.KMKey_LanguageID, "am");
    amharic.put(KMManager.KMKey_LanguageName, "Amharic");
    amharic.put(KMManager.KMKey_KeyboardVersion, "1.4");
    amharic.put(KMManager.KMKey_Font, "fantuwua.ttf");
    amharic.put(KMManager.KMKey_OskFont, "wookianos.ttf");

    Assert.assertEquals(amharic, keyboards[0]);

    HashMap<String, String> geez = new HashMap<String, String>();
    geez.put(KMManager.KMKey_PackageID, "gff_amh_7_test");
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

  @Test
  public void test_installKMP() throws Exception {
    List<Map<String, String>> installedKbds = PackageProcessor.processKMP(TEST_GFF_KMP_FILE);

    Assert.assertTrue(TEST_GFF_KMP_TARGET.exists());
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
  }

  @Test
  public void test_upgradeInstall() throws Exception {
    File installedKMP = PackageProcessor.constructPath(TEST_GFF_KMP_FILE, false);
    List<Map<String, String>> installedKbds;
    String version;

    installedKbds = PackageProcessor.processKMP(TEST_GFF_KMP_FILE);
    version = PackageProcessor.getPackageVersion(PackageProcessor.loadPackageInfo(installedKMP));
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
    Assert.assertEquals("7.0", version);

    createAlternateKMP();
    installedKbds = PackageProcessor.processKMP(TEST_GFF_KMP_FILE_ALT, false, true);
    version = PackageProcessor.getPackageVersion(PackageProcessor.loadPackageInfo(installedKMP));
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
    Assert.assertEquals("8.0", version);
  }

  @Test
  public void test_downgradeInstall() throws Exception {
    File installedKMP = PackageProcessor.constructPath(TEST_GFF_KMP_FILE, false);
    List<Map<String, String>> installedKbds;
    String version;

    createAlternateKMP();
    installedKbds = PackageProcessor.processKMP(TEST_GFF_KMP_FILE_ALT, false, true);
    version = PackageProcessor.getPackageVersion(PackageProcessor.loadPackageInfo(installedKMP));
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
    Assert.assertEquals("8.0", version);

    // Blocked downgrade attempt.
    installedKbds = PackageProcessor.processKMP(TEST_GFF_KMP_FILE, false);
    version = PackageProcessor.getPackageVersion(PackageProcessor.loadPackageInfo(installedKMP));
    Assert.assertEquals(0, installedKbds.size());
    Assert.assertEquals("8.0", version);

    installedKbds = PackageProcessor.processKMP(TEST_GFF_KMP_FILE, true);
    version = PackageProcessor.getPackageVersion(PackageProcessor.loadPackageInfo(installedKMP));
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
    Assert.assertEquals("7.0", version);
  }

  @Test
  public void test_keyboardVersion() throws Exception {
    JSONObject json = PackageProcessor.loadPackageInfo(tempPkg);

    Assert.assertEquals("1.4", PackageProcessor.getKeyboardVersion(json, TEST_GFF_KBD_ID));
  }

  @Test
  public void test_versionChecks() throws Exception {
    Assert.assertFalse(PackageProcessor.isDowngrade(TEST_GFF_KMP_FILE));
    Assert.assertFalse(PackageProcessor.isSameVersion(TEST_GFF_KMP_FILE));

    createAlternateKMP();
    PackageProcessor.processKMP(TEST_GFF_KMP_FILE_ALT, false, true);

    Assert.assertTrue(PackageProcessor.isDowngrade(TEST_GFF_KMP_FILE));
    Assert.assertFalse(PackageProcessor.isSameVersion(TEST_GFF_KMP_FILE));

    // Test 2 - when it's an equal version.
    PackageProcessor.processKMP(TEST_GFF_KMP_FILE, true);
    Assert.assertFalse(PackageProcessor.isDowngrade(TEST_GFF_KMP_FILE));
    Assert.assertTrue(PackageProcessor.isSameVersion(TEST_GFF_KMP_FILE));

    // Test 3 - when it's an upgrade.
    createAlternateKMP();
    Assert.assertFalse(PackageProcessor.isDowngrade(TEST_GFF_KMP_FILE_ALT, true));
    Assert.assertFalse(PackageProcessor.isSameVersion(TEST_GFF_KMP_FILE_ALT, true));
  }

  @Test
  public void test_kmpVersionCheck() throws Exception {
    createAlternateKMP();
    PackageProcessor.processKMP(TEST_GFF_KMP_FILE_ALT, false, true);

    Assert.assertEquals("8.0", PackageProcessor.getPackageVersion(TEST_GFF_KMP_FILE, true));
    Assert.assertEquals("7.0", PackageProcessor.getPackageVersion(TEST_GFF_KMP_FILE, false));
  }
}