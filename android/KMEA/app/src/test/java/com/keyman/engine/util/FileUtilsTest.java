package com.keyman.engine.util;

import androidx.test.core.app.ApplicationProvider;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.shadows.ShadowLog;

import java.io.File;
import java.util.List;

@RunWith(RobolectricTestRunner.class)
public class FileUtilsTest {

  @Test
  public void test_download() {
    // Test invalid download doesn't throw exception
    int ret = FileUtils.download(ApplicationProvider.getApplicationContext(), "invalidURL", "", "");
    Assert.assertEquals(-1, ret);

    List<ShadowLog.LogItem> logs = ShadowLog.getLogs();

    // The logs contain type 4, but we only care about type 6 for connection messages
    Assert.assertEquals(2, logs.size());

    Assert.assertEquals("Connection", logs.get(0).tag);
    Assert.assertEquals("Initialization failed:\njava.net.MalformedURLException: no protocol: invalidURL", logs.get(0).msg);

    Assert.assertEquals("FileUtils", logs.get(1).tag);
    Assert.assertEquals("Could not download filename ", logs.get(1).msg);
  }

  @Test
  public void test_compareVersions() {
    Assert.assertEquals(FileUtils.VERSION_INVALID, FileUtils.compareVersions(null, "1.0"));
    Assert.assertEquals(FileUtils.VERSION_INVALID, FileUtils.compareVersions("" , "1.0"));
    Assert.assertEquals(FileUtils.VERSION_INVALID, FileUtils.compareVersions("1.0", null));
    Assert.assertEquals(FileUtils.VERSION_INVALID, FileUtils.compareVersions("1.0", ""));
    Assert.assertEquals(FileUtils.VERSION_INVALID, FileUtils.compareVersions(null, null));
    Assert.assertEquals(FileUtils.VERSION_INVALID, FileUtils.compareVersions("", ""));

    Assert.assertEquals(FileUtils.VERSION_EQUAL, FileUtils.compareVersions("0.1", "0.1"));
    Assert.assertEquals(FileUtils.VERSION_EQUAL, FileUtils.compareVersions("1.0", "1.0"));
    Assert.assertEquals(FileUtils.VERSION_EQUAL, FileUtils.compareVersions("1.0a", "1.0a"));
    Assert.assertEquals(FileUtils.VERSION_EQUAL, FileUtils.compareVersions("1.0.1", "1.0.1"));

    Assert.assertEquals(FileUtils.VERSION_LOWER, FileUtils.compareVersions("-1.0", "1.0"));
    Assert.assertEquals(FileUtils.VERSION_LOWER, FileUtils.compareVersions("0", "1.0"));
    Assert.assertEquals(FileUtils.VERSION_LOWER, FileUtils.compareVersions("0.1", "1.0"));
    Assert.assertEquals(FileUtils.VERSION_LOWER, FileUtils.compareVersions("1.0", "1.0.1"));

    // 1.0 alpha < 1.0 beta < 1.0
    Assert.assertEquals(FileUtils.VERSION_LOWER, FileUtils.compareVersions("1.0a", "1.0b"));
    Assert.assertEquals(FileUtils.VERSION_LOWER, FileUtils.compareVersions("1.0a", "1.0"));
    Assert.assertEquals(FileUtils.VERSION_LOWER, FileUtils.compareVersions("1.0b", "1.0"));

    Assert.assertEquals(FileUtils.VERSION_GREATER, FileUtils.compareVersions("1.0", "-1.0"));
    Assert.assertEquals(FileUtils.VERSION_GREATER, FileUtils.compareVersions("1.0", "0"));
    Assert.assertEquals(FileUtils.VERSION_GREATER, FileUtils.compareVersions("1.0", "0.1"));
    Assert.assertEquals(FileUtils.VERSION_GREATER, FileUtils.compareVersions("1.0.1", "1.0"));

    // 1.0 > 1.0 beta > 1.0 alpha
    Assert.assertEquals(FileUtils.VERSION_GREATER, FileUtils.compareVersions("1.0", "1.0b"));
    Assert.assertEquals(FileUtils.VERSION_GREATER, FileUtils.compareVersions("1.0", "1.0a"));
    Assert.assertEquals(FileUtils.VERSION_GREATER, FileUtils.compareVersions("1.0b", "1.0a"));
  }

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

    filename = "test/abc.woff";
    Assert.assertTrue(FileUtils.hasFontExtension(filename));

    filename = "test/abc.WOFF";
    Assert.assertTrue(FileUtils.hasFontExtension(filename));

    filename = "test/abc.svg";
    Assert.assertTrue(FileUtils.hasFontExtension(filename));

    filename = "test/abc.SVG";
    Assert.assertTrue(FileUtils.hasFontExtension(filename));

    filename = "test/abcttf";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));

    filename = "test/abc.font";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));

    filename = "test/abcsvg";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));

    filename = "test/abc.svg#";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));

    filename = "test/abcwoff";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));

    filename = "";
    Assert.assertFalse(FileUtils.hasFontExtension(filename));
  }

  @Test
  public void test_hasSVGViewBox() {
    String filename = "test/abc.svg#xyz";
    Assert.assertTrue(FileUtils.hasSVGViewBox(filename));

    filename = "test/abc.SVG#xyz";
    Assert.assertTrue(FileUtils.hasSVGViewBox(filename));

    filename = "test/abc.svg#xyz";
    Assert.assertTrue(FileUtils.hasSVGViewBox(filename));

    filename = "test/abcsvg#xyz";
    Assert.assertFalse(FileUtils.hasSVGViewBox(filename));

    filename = "";
    Assert.assertFalse(FileUtils.hasSVGViewBox(filename));
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
  public void test_hasLexicalModelExtension() {
    String filename = "test/abc.model.js";
    Assert.assertTrue(FileUtils.hasLexicalModelExtension(filename));

    filename = "test/abc.MODEL.JS";
    Assert.assertTrue(FileUtils.hasLexicalModelExtension(filename));

    filename = "test/abc.sh";
    Assert.assertFalse(FileUtils.hasLexicalModelExtension(filename));

    filename = "test/abcmodeljs";
    Assert.assertFalse(FileUtils.hasLexicalModelExtension(filename));

    filename = "test/abc.js";
    Assert.assertFalse(FileUtils.hasLexicalModelExtension(filename));

    filename = "";
    Assert.assertFalse(FileUtils.hasLexicalModelExtension(filename));
  }

  @Test
  public void test_hasLexicalModelPackageExtension() {
    String filename = "test/abc.kmp";
    Assert.assertFalse(FileUtils.hasLexicalModelPackageExtension(filename));

    filename = "test/abc.KMP";
    Assert.assertFalse(FileUtils.hasLexicalModelPackageExtension(filename));

    filename = "test/abc.kmpo";
    Assert.assertFalse(FileUtils.hasLexicalModelPackageExtension(filename));

    filename = "test/abc.model.kmp";
    Assert.assertTrue(FileUtils.hasLexicalModelPackageExtension(filename));

    filename = "test/abc.MODEL.KMP";
    Assert.assertTrue(FileUtils.hasLexicalModelPackageExtension(filename));

    filename = "test/abc.MODEL.KMPO";
    Assert.assertFalse(FileUtils.hasLexicalModelPackageExtension(filename));

    filename = "";
    Assert.assertFalse(FileUtils.hasLexicalModelPackageExtension(filename));
  }

  @Test
  public void test_hasKeymanPackageExtension() {
    String filename = "test/abc.kmp";
    Assert.assertTrue(FileUtils.hasKeymanPackageExtension(filename));

    filename = "test/abc.KMP";
    Assert.assertTrue(FileUtils.hasKeymanPackageExtension(filename));

    filename = "test/abc.kmpo";
    Assert.assertFalse(FileUtils.hasKeymanPackageExtension(filename));

    filename = "test/abc.model.kmp";
    Assert.assertTrue(FileUtils.hasKeymanPackageExtension(filename));

    filename = "test/abc.MODEL.KMP";
    Assert.assertTrue(FileUtils.hasKeymanPackageExtension(filename));

    filename = "test/abc.MODEL.KMPO";
    Assert.assertFalse(FileUtils.hasKeymanPackageExtension(filename));

    filename = "";
    Assert.assertFalse(FileUtils.hasKeymanPackageExtension(filename));
  }

  @Test
  public void test_isTTFFont() {
    String filename = "test/abc.ttf";
    Assert.assertTrue(FileUtils.isTTFFont(filename));

    filename = "test/abc.TTF";
    Assert.assertTrue(FileUtils.isTTFFont(filename));

    filename = "test/abc.ttfo";
    Assert.assertFalse(FileUtils.isTTFFont(filename));

    filename = "";
    Assert.assertFalse(FileUtils.isTTFFont(filename));
  }

  @Test
  public void test_isWelcomeFile() {
    String filename = "test/welcome.htm";
    Assert.assertTrue(FileUtils.isWelcomeFile(filename));

    filename = "test/welcome.HTM";
    Assert.assertTrue(FileUtils.isWelcomeFile(filename));

    filename = "test" + File.separator + "welcome.htm";
    Assert.assertTrue(FileUtils.isWelcomeFile(filename));

    filename = "test/WELCOME.HTM";
    Assert.assertTrue(FileUtils.isWelcomeFile(filename));

    filename = "test/welcome.html";
    Assert.assertFalse(FileUtils.isWelcomeFile(filename));

    filename = "test/zap-welcome.htm";
    Assert.assertFalse(FileUtils.isWelcomeFile(filename));

    filename = "test/welcomehtm";
    Assert.assertFalse(FileUtils.isWelcomeFile(filename));


    filename = "";
    Assert.assertFalse(FileUtils.isWelcomeFile(filename));
  }

  @Test
  public void test_getSVGFilename() {
    String filename = "test/abc.svg#xyz";
    Assert.assertEquals("test/abc.svg#", FileUtils.getSVGFilename(filename));

    filename = "test/abc.SVG#XYZ";
    Assert.assertEquals("test/abc.SVG#", FileUtils.getSVGFilename(filename));

    filename = "";
    Assert.assertEquals("", FileUtils.getSVGFilename(filename));

    filename = "test/abc.notsvg#";
    Assert.assertEquals("", FileUtils.getSVGFilename(filename));

    filename = "test/abc.svg";
    Assert.assertEquals("", FileUtils.getSVGFilename(filename));

  }

  @Test
  public void test_isReadmeFile() {

    Assert.assertTrue(FileUtils.isReadmeFile("test/readme.htm"));

    Assert.assertTrue(FileUtils.isReadmeFile("test/README.HTM"));

    Assert.assertFalse(FileUtils.isReadmeFile("test/WELCOME.HTM"));
  }
}
