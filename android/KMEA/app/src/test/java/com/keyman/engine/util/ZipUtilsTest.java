/**
 * Copyright (C) 2021 SIL International. All rights reserved.
 */

package com.keyman.engine.util;

import com.keyman.engine.util.ZipUtils;

import org.junit.Assert;
import org.junit.Assume;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.io.File;
import java.io.IOException;

public class ZipUtilsTest {
  private static final File TEST_RESOURCE_ROOT = new File("test_resources");
  private static final File TEST_EXTRACTION_ROOT = new File(TEST_RESOURCE_ROOT, "temp");

  // Zip file with Windows paths to test zip slip vulnerability
  private static final File TEST_ZIP_SLIP_FILE = new File(TEST_RESOURCE_ROOT, "zip-slip-win.zip");

  @Rule
  public ExpectedException exceptionRule = ExpectedException.none();

  @Test
  public void test_unzip_throwsSecurityException() throws SecurityException, IOException {
    // Zip file for testing zip slip vulnerability has Windows paths
    Assume.assumeTrue(System.getProperty("os.name").toLowerCase().startsWith("win"));

    exceptionRule.expect(SecurityException.class);
    exceptionRule.expectMessage("Zip traversal error");
    ZipUtils.unzip(TEST_ZIP_SLIP_FILE, TEST_EXTRACTION_ROOT);
  }
}
