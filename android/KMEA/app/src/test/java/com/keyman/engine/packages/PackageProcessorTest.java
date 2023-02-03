package com.keyman.engine.packages;

import com.keyman.engine.KMManager;

import org.apache.commons.io.FileUtils;
import org.json.JSONObject;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.List;

@RunWith(RobolectricTestRunner.class)
public class PackageProcessorTest {
  private static final File TEST_RESOURCE_ROOT = new File("test_resources");
  private static final File TEST_EXTRACTION_ROOT = new File(TEST_RESOURCE_ROOT, "temp");

  private static final String TEST_GFF_KMP_NAME = "gff_amh_7_test_json";
  private static final File TEST_GFF_KMP_FILE = new File(TEST_RESOURCE_ROOT, "v14" + File.separator + TEST_GFF_KMP_NAME + ".kmp");
  private static final File TEST_GFF_KMP_TARGET = new File(TEST_EXTRACTION_ROOT, "packages" +
    File.separator + TEST_GFF_KMP_NAME);

  private static final String TEST_GFF_KMP_NAME_ALT = TEST_GFF_KMP_NAME;
  private static final File TEST_GFF_KMP_FILE_ALT = new File(TEST_RESOURCE_ROOT, "v15" + File.separator + TEST_GFF_KMP_NAME_ALT + ".kmp");
  private static final File TEST_GFF_KMP_TARGET_ALT = new File(TEST_EXTRACTION_ROOT, "packages" +
    File.separator + TEST_GFF_KMP_NAME_ALT);

  private static final String TEST_GFF_KMP_NAME_UNDEFINED_VER = TEST_GFF_KMP_NAME;
  private static final File TEST_GFF_KMP_FILE_UNDEFINED_VER = new File(TEST_RESOURCE_ROOT, "v" + File.separator + TEST_GFF_KMP_NAME_UNDEFINED_VER + ".kmp");
  private static final File TEST_GFF_KMP_TARGET_UNDEFINED_VER = new File(TEST_EXTRACTION_ROOT, "packages" +
    File.separator + TEST_GFF_KMP_NAME_UNDEFINED_VER);

  private static final int TEST_GFF_KBD_COUNT = 1;
  private static final String TEST_GFF_PACKAGE_NAME = "GFF Amharic Keyboard";
  private static final String TEST_GFF_KBD_ID = "gff_amh_7";
  private static final String TEST_GFF_KBD_NAME = "Amharic";

  private static final String TEST_EN_CUSTOM_MODEL_NAME = "example.en.custom";
  private static final File TEST_EN_CUSTOM_MODEL_KMP_FILE = new File(TEST_RESOURCE_ROOT, "packages" +
    File.separator + "en.custom" + File.separator + TEST_EN_CUSTOM_MODEL_NAME + ".model.kmp");

  private static File tempPkg;
  private static File tempPkgAlt;
  private static File tempPkgUndefinedVer;

  private PackageProcessor PP, PP_ALT, PP_UNDEFINED;

  // Each test gets a fresh version of the extracted package.
  @Before
  public void extractBaseTestPackage() {
    PP = new PackageProcessor(TEST_EXTRACTION_ROOT);
    try {
      tempPkg = PP.unzipKMP(TEST_GFF_KMP_FILE);
    } catch (IOException e) {
      System.err.println(e);
    }
  }

  // Some tests wish to utilize an alternate package..
  public void extractAltTestPackage() {
    PP_ALT = new PackageProcessor(TEST_EXTRACTION_ROOT);
    try {
      tempPkgAlt = PP_ALT.unzipKMP(TEST_GFF_KMP_FILE_ALT);
    } catch (IOException e) {
      System.err.println(e);
    }
  }

  // Keyboard Package Tests

  // Some tests wish to utilize package with undefined version...
  public void extractUndefinedVerTestPackage() {
    PP_UNDEFINED = new PackageProcessor(TEST_EXTRACTION_ROOT);
    try {
      tempPkgUndefinedVer = PP_UNDEFINED.unzipKMP(TEST_GFF_KMP_FILE_UNDEFINED_VER);
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
    FileUtils.deleteQuietly(tempPkgAlt);
    FileUtils.deleteQuietly(tempPkgUndefinedVer);
    FileUtils.deleteDirectory(TEST_GFF_KMP_TARGET);
  }

  @Test
  public void tempPackageExtraction() {
    File infoFile = new File(tempPkg, PackageProcessor.PP_DEFAULT_METADATA);

    Assert.assertTrue(infoFile.exists());
  }

  @Test
  public void test_loadInfoJSON() throws Exception {
    JSONObject json = PP.loadPackageInfo(tempPkg);

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
    JSONObject json = PP.loadPackageInfo(tempPkg);
    FileUtils.moveDirectory(tempPkg, TEST_GFF_KMP_TARGET);

    Assert.assertNotNull(json);
    String pkgVersion = PP.getPackageVersion(json);

    ArrayList<String> languageList = new ArrayList<String>();
    Map<String, String>[] keyboards = PP.processEntry(json.getJSONArray("keyboards").getJSONObject(0), "gff_amh_7_test_json", pkgVersion, languageList);

    HashMap<String, String> amharic = new HashMap<String, String>();
    amharic.put(KMManager.KMKey_PackageID, "gff_amh_7_test_json");
    amharic.put(KMManager.KMKey_KeyboardName, "Amharic");
    amharic.put(KMManager.KMKey_KeyboardID, "gff_amh_7");
    amharic.put(KMManager.KMKey_LanguageID, "am");
    amharic.put(KMManager.KMKey_LanguageName, "Amharic");
    amharic.put(KMManager.KMKey_KeyboardVersion, "1.4");
    amharic.put(KMManager.KMKey_CustomHelpLink, TEST_GFF_KMP_TARGET + File.separator + "welcome.htm");

    // If languageID doesn't match, verify only the first language is installed with the keyboard
    Assert.assertEquals(amharic, keyboards[0]);
    Assert.assertEquals(TEST_GFF_KBD_COUNT, keyboards.length);

    String languageID = "am";
    languageList.add(languageID);
    keyboards = PP.processEntry(json.getJSONArray("keyboards").getJSONObject(0), "gff_amh_7_test_json", pkgVersion, languageList);

    // Verify "am" matched
    Assert.assertEquals(amharic, keyboards[0]);
    Assert.assertEquals(TEST_GFF_KBD_COUNT, keyboards.length);

    languageList.remove(0);
    languageID = "GEZ";
    languageList.add(languageID);
    keyboards = PP.processEntry(json.getJSONArray("keyboards").getJSONObject(0), "gff_amh_7_test_json", pkgVersion, languageList);

    HashMap<String, String> geez = new HashMap<String, String>();
    geez.put(KMManager.KMKey_PackageID, "gff_amh_7_test_json");
    geez.put(KMManager.KMKey_KeyboardName, "Amharic");
    geez.put(KMManager.KMKey_KeyboardID, "gff_amh_7");
    geez.put(KMManager.KMKey_LanguageID, "gez");
    geez.put(KMManager.KMKey_LanguageName, "Ge'ez");
    geez.put(KMManager.KMKey_KeyboardVersion, "1.4");
    geez.put(KMManager.KMKey_CustomHelpLink, TEST_GFF_KMP_TARGET + File.separator + "welcome.htm");

    // Verify "gez" matched
    Assert.assertEquals(geez, keyboards[0]);
    Assert.assertEquals(TEST_GFF_KBD_COUNT, keyboards.length);
  }

  @Test
  public void test_PathConstruction() {
    String permPath = TEST_EXTRACTION_ROOT.toString() + File.separator + "packages" + File.separator + TEST_GFF_KMP_NAME;
    Assert.assertEquals(new File(permPath), PP.constructPath(TEST_GFF_KMP_FILE, false));

    Assert.assertNotEquals(new File(permPath), PP.constructPath(TEST_GFF_KMP_FILE, true));
  }

  @Test
  public void test_installKMP() throws Exception {
    List<Map<String, String>> installedKbds = PP.processKMP(TEST_GFF_KMP_FILE, tempPkg, PackageProcessor.PP_KEYBOARDS_KEY);

    Assert.assertTrue(TEST_GFF_KMP_TARGET.exists());
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
  }

  @Test
  public void test_upgradeInstall() throws Exception {
    File installedKMP = PP.constructPath(TEST_GFF_KMP_FILE, false);
    List<Map<String, String>> installedKbds;
    String version;

    installedKbds = PP.processKMP(TEST_GFF_KMP_FILE, tempPkg, PackageProcessor.PP_KEYBOARDS_KEY);
    version = PP.getPackageVersion(PP.loadPackageInfo(installedKMP));
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
    Assert.assertEquals("1.4", version);

    extractAltTestPackage();
    installedKbds = PP_ALT.processKMP(TEST_GFF_KMP_FILE_ALT, tempPkgAlt, PackageProcessor.PP_KEYBOARDS_KEY);
    version = PP_ALT.getPackageVersion(PP_ALT.loadPackageInfo(installedKMP));
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
    Assert.assertEquals("1.5", version);
  }

  @Test
  public void test_downgradeInstall() throws Exception {
    File installedKMP = PP.constructPath(TEST_GFF_KMP_FILE, false);
    List<Map<String, String>> installedKbds;
    String version;

    extractAltTestPackage();
    installedKbds = PP_ALT.processKMP(TEST_GFF_KMP_FILE_ALT, tempPkgAlt, PackageProcessor.PP_KEYBOARDS_KEY);
    version = PP_ALT.getPackageVersion(PP_ALT.loadPackageInfo(installedKMP));
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
    Assert.assertEquals("1.5", version);

    extractBaseTestPackage();
    installedKbds = PP.processKMP(TEST_GFF_KMP_FILE, tempPkg, PackageProcessor.PP_KEYBOARDS_KEY);
    version = PP.getPackageVersion(PP.loadPackageInfo(installedKMP));
    Assert.assertEquals(TEST_GFF_KBD_COUNT, installedKbds.size());
    Assert.assertEquals("1.4", version);
  }

  @Test
  public void test_keyboardVersion() {
    JSONObject json = PP.loadPackageInfo(tempPkg);

    Assert.assertEquals("1.4", PackageProcessor.getKeyboardVersion(json, TEST_GFF_KBD_ID));
  }

  @Test
  public void test_getPackageID() {
    Assert.assertEquals(TEST_GFF_KMP_NAME, PP.getPackageID(TEST_GFF_KMP_FILE));
    Assert.assertNotEquals(TEST_GFF_KBD_ID, PP.getPackageID(TEST_GFF_KMP_FILE));
    Assert.assertEquals(TEST_EN_CUSTOM_MODEL_NAME, PP.getPackageID(TEST_EN_CUSTOM_MODEL_KMP_FILE));

    // Test trimming " (#)" from kmp name
    Assert.assertEquals("khmer_angkor", PP.getPackageID(new File("khmer_angkor(10).kmp")));
    Assert.assertEquals("khmer_angkor", PP.getPackageID(new File("khmer_angkor (10).kmp")));

    // Test trimming "-#" from kmp name
    Assert.assertEquals("khmer_angkor", PP.getPackageID(new File("khmer_angkor-10.kmp")));
    Assert.assertEquals("khmer_angkor", PP.getPackageID(new File("khmer_angkor -10.kmp")));
  }

  @Test
  public void test_getPackageName() throws Exception {
    JSONObject json = PP.loadPackageInfo(tempPkg);

    Assert.assertEquals(TEST_GFF_PACKAGE_NAME, PP.getPackageName(json));
    Assert.assertNotEquals(TEST_GFF_KMP_NAME, PP.getPackageName(json));
  }

  @Test
  public void test_getPackageVersion() {
    extractUndefinedVerTestPackage();
    JSONObject json = PP_UNDEFINED.loadPackageInfo(tempPkgUndefinedVer);

    Assert.assertEquals(PackageProcessor.PP_DEFAULT_VERSION, PP_UNDEFINED.getPackageVersion(json));
  }

  @Test
  public void test_getPackageTarget() {
    JSONObject json = PP.loadPackageInfo(tempPkg);

    Assert.assertEquals(PackageProcessor.PP_TARGET_KEYBOARDS, PP.getPackageTarget(json));
  }

  @Test
  public void test_versionChecks() throws Exception {
    Assert.assertFalse(PP.isDowngrade(TEST_GFF_KMP_FILE));
    Assert.assertFalse(PP.isSameVersion(TEST_GFF_KMP_FILE));

    extractAltTestPackage();
    PP_ALT.processKMP(TEST_GFF_KMP_FILE_ALT, tempPkgAlt, PackageProcessor.PP_KEYBOARDS_KEY);

    Assert.assertTrue(PP.isDowngrade(TEST_GFF_KMP_FILE));
    Assert.assertFalse(PP.isSameVersion(TEST_GFF_KMP_FILE));

    // Test 2 - when it's an equal version.
    extractBaseTestPackage();
    PP.processKMP(TEST_GFF_KMP_FILE, tempPkg, PackageProcessor.PP_KEYBOARDS_KEY);
    Assert.assertFalse(PP.isDowngrade(TEST_GFF_KMP_FILE));
    Assert.assertTrue(PP.isSameVersion(TEST_GFF_KMP_FILE));

    // Test 3 - when it's an upgrade.
    extractAltTestPackage();
    Assert.assertFalse(PP_ALT.isDowngrade(TEST_GFF_KMP_FILE_ALT));
    Assert.assertFalse(PP_ALT.isSameVersion(TEST_GFF_KMP_FILE_ALT));
  }

  @Test
  public void test_kmpVersionCheck() {
    JSONObject json14 = PP.loadPackageInfo(tempPkg);
    Assert.assertEquals("1.4", PP.getPackageVersion(json14));

    extractAltTestPackage();
    JSONObject json15 = PP_ALT.loadPackageInfo(tempPkgAlt);
    Assert.assertEquals("1.5", PP_ALT.getPackageVersion(json15));
  }
}