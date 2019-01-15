package com.tavultesoft.kmea.packages;


import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.io.File;
import java.io.IOException;

@RunWith(RobolectricTestRunner.class)
public class JSONUtilsTest {
  private static final File TEST_RESOURCE_ROOT = new File("test_resources", "packages");

  @Before
  public void initializeTestPackage() {
    JSONUtils.initialize(TEST_RESOURCE_ROOT);
  }

  @Test
  public void test_getLanguages() {
    JSONUtils.getLanguages();

  }

}

