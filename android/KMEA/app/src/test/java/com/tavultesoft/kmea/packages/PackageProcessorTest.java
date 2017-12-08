package com.tavultesoft.kmea.packages;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import java.io.File;
import java.io.IOException;

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
  public void packageTest() {

  }

  @AfterClass
  public static void eraseTestPackages() {
    PackageProcessor.clearDirectory(tempPkg);
  }
}