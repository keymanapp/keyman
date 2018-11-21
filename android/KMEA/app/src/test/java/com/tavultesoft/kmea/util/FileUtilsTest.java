package com.tavultesoft.kmea.util;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

@RunWith(RobolectricTestRunner.class)
public class FileUtilsTest {

  @Test
  public void test_hasFontExtension() {
    String filename = "test/abc.ttf";
    Assert.assertTrue(FileUtils.hasFontExtension(filename));

    filename = "test/abc.TTF";
    Assert.assertTrue(FileUtils.hasFontExtension(filename));

    filename = "test/abc.otf";
    Assert.assertTrue(FileUtils.hasFontExtension(filename));

    filename = "test/abc.OTF";
    Assert.assertTrue(FileUtils.hasFontExtension(filename));

    filename = "test/abcttf";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));

    filename = "test/abc.font";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));

    filename = "";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));

  }

  @Test
  public void test_hasJavascriptExtension() {
    String filename = "test/abc.js";
    Assert.assertTrue(FileUtils.hasJavaScriptExtension(filename));

    filename = "test/abc.JS";
    Assert.assertTrue(FileUtils.hasJavaScriptExtension(filename));

    filename = "test/abc.sh";
    Assert.assertFalse(FileUtils.hasJavaScriptExtension(filename));

    filename = "test/abcjs";
    Assert.assertFalse(FileUtils.hasJavaScriptExtension(filename));

    filename = "";
    Assert.assertFalse(FileUtils.hasJavaScriptExtension(filename));

  }

  @Test
  public void test_hasKeyboardPackageExtension() {
    String filename = "test/abc.kmp";
    Assert.assertTrue(FileUtils.hasKeyboardPackageExtension(filename));

    filename = "test/abc.KMP";
    Assert.assertTrue(FileUtils.hasKeyboardPackageExtension(filename));

    filename = "test/abc.kmpo";
    Assert.assertFalse(FileUtils.hasKeyboardPackageExtension(filename));

    filename = "";
    Assert.assertFalse(FileUtils.hasKeyboardPackageExtension(filename));
  }

  @Test
  public void test_isWelcomeFile() {
    String filename = "test/welcome.htm";
    Assert.assertTrue(FileUtils.isWelcomeFile(filename));

    filename = "test/welcome.HTM";
    Assert.assertTrue(FileUtils.isWelcomeFile(filename));

    filename = "test/welcome.html";
    Assert.assertFalse(FileUtils.isWelcomeFile(filename));

    filename = "test/welcomehtm";
    Assert.assertFalse(FileUtils.isWelcomeFile(filename));

    filename = "";
    Assert.assertFalse(FileUtils.isWelcomeFile(filename));

  }
}
