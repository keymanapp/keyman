package com.tavultesoft.kmea.packages;

import org.junit.AfterClass;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.Test;
import java.io.File;

public class PackageProcessorTest {
  public static final File TEST_RESOURCE_ROOT = new File("test_resources");
  public static final File TEST_EXTRACTION_ROOT = new File(TEST_RESOURCE_ROOT, "temp");

  @BeforeClass
  public void extractTestPackages() {
    PackageProcessor.processKMP(new File(TEST_RESOURCE_ROOT, "gff_amh_7_test_json.kmp"));
  }

  @Test
  public void packageTest() {

  }

  @AfterClass
  public void eraseTestPackages() {

  }
}