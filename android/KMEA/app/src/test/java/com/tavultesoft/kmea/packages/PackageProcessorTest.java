package com.tavultesoft.kmea.packages;

import com.tavultesoft.kmea.KMManager;

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

@RunWith(RobolectricTestRunner.class)
public class PackageProcessorTest {
  public static final File TEST_RESOURCE_ROOT = new File("test_resources");
  public static final File TEST_EXTRACTION_ROOT = new File(TEST_RESOURCE_ROOT, "temp");

  public static final String TEST_GFF_KMP_NAME = "gff_amh_7_test_json";
  public static final File TEST_GFF_KMP_FILE = new File(TEST_RESOURCE_ROOT, TEST_GFF_KMP_NAME + ".kmp");
  public static final File TEST_GFF_KMP_TARGET = new File(TEST_EXTRACTION_ROOT, "packages" +
    File.separator + TEST_GFF_KMP_NAME);

  public static final String TEST_GFF_KMP_NAME_ALT = "gff_amh_7_test_json_alt";
  public static final File TEST_GFF_KMP_FILE_ALT = new File(TEST_RESOURCE_ROOT, TEST_GFF_KMP_NAME_ALT + ".kmp");
  public static final File TEST_GFF_KMP_TARGET_ALT = new File(TEST_EXTRACTION_ROOT, "packages" +
    File.separator + TEST_GFF_KMP_NAME_ALT);

  private static File tempPkg, tempPkgAlt;

  /**
   * Uses the existing sample KMP's kmp.info file as a base, constructing a second, altered KMP package
   * for use in version tests between same-id packages.  Does not reconstruct the .zip for actual installation
   * tests.
   * @throws Exception
   */
  private static void createAlternateKMP() throws Exception {
    FileUtils.copyFile(TEST_GFF_KMP_FILE, TEST_GFF_KMP_FILE_ALT);
    try {
      tempPkgAlt = PackageProcessor.unzipKMP(TEST_GFF_KMP_FILE_ALT);

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

    Map<String, String>[] keyboards = PackageProcessor.processKeyboardsEntry(json.getJSONArray("keyboards").getJSONObject(0));
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

  /**
   * Rather than actually attempt to maintain two identically-named package files, this white-box test
   * simply relies upon an altered kmp.info to test the central version comparison logic used to
   * determine if an old package version should be replaced.
   * @throws Exception
   */
  @Test
  public void test_versionCompare() throws Exception {
    createAlternateKMP();

    Assert.assertEquals(1, PackageProcessor.comparePackageDirectories(tempPkgAlt, tempPkg));
    Assert.assertEquals(-1, PackageProcessor.comparePackageDirectories(tempPkg, tempPkgAlt));

    // Test for when there is no pre-existing package.
    FileUtils.deleteDirectory(tempPkgAlt);
    Assert.assertEquals(1, PackageProcessor.comparePackageDirectories(tempPkg, tempPkgAlt));
  }

  @Test
  public void test_installKMP() throws Exception {
    PackageProcessor.processKMP(TEST_GFF_KMP_FILE);

    Assert.assertTrue(TEST_GFF_KMP_TARGET.exists());
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
}