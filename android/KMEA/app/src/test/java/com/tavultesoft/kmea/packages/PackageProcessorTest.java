package com.tavultesoft.kmea.packages;

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

@RunWith(RobolectricTestRunner.class)
public class PackageProcessorTest {
  public static final File TEST_RESOURCE_ROOT = new File("test_resources");
  public static final File TEST_EXTRACTION_ROOT = new File(TEST_RESOURCE_ROOT, "temp");

  private static File tempPkg;

  @BeforeClass
  public static void extractTestPackages() {
    PackageProcessor.initialize(TEST_EXTRACTION_ROOT);
    try {

      tempPkg = PackageProcessor.unzipKMP(new File(TEST_RESOURCE_ROOT, "gff_amh_7_test_json.kmp"));
    } catch (IOException e) {
      System.err.println(e);
    }
  }

  @Test
  public void tempPackageExtraction() {
    File infoFile = new File(tempPkg, "kmp.json");

    Assert.assertTrue(infoFile.exists());
    System.out.println("File size: " + infoFile.length());
    System.out.println("Absolute path: " + infoFile.getAbsolutePath());
  }

  @Test
  public void test_loadInfoJSON() throws Exception {
    JSONObject json = PackageProcessor.loadPackageInfo(tempPkg);

    Assert.assertNotNull(json);

    // Test pretty-print, for visual inspection if desired.
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

  @AfterClass
  public static void eraseTestPackages() {
    //PackageProcessor.clearDirectory(tempPkg);
  }
}