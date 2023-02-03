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
import java.util.List;
import java.util.Map;

@RunWith(RobolectricTestRunner.class)
public class LexicalModelPackageProcessorTest {
  private static final File TEST_RESOURCE_ROOT = new File("test_resources");
  private static final File TEST_EXTRACTION_ROOT = new File(TEST_RESOURCE_ROOT, "temp");

  private static final String TEST_EN_CUSTOM_MODEL_NAME = "example.en.custom";
  private static final File TEST_EN_CUSTOM_MODEL_KMP_FILE = new File(TEST_RESOURCE_ROOT, "packages" +
    File.separator + "en.custom" + File.separator + TEST_EN_CUSTOM_MODEL_NAME + ".model.kmp");
  private static final File TEST_EN_CUSTOM_MODEL_KMP_TARGET = new File(TEST_EXTRACTION_ROOT, "models" +
    File.separator + TEST_EN_CUSTOM_MODEL_NAME);

  private static final int TEST_EN_CUSTOM_MODEL_COUNT = 2;

  private static File tempPkg;
  private static LexicalModelPackageProcessor lmPP;

  // Each test gets a fresh version of the extracted package.
  @Before
  public void extractBaseTestPackage() {
    lmPP = new LexicalModelPackageProcessor(TEST_EXTRACTION_ROOT);
    try {
      tempPkg = lmPP.unzipKMP(TEST_EN_CUSTOM_MODEL_KMP_FILE);
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
    FileUtils.deleteDirectory(TEST_EN_CUSTOM_MODEL_KMP_TARGET);
  }

  // Lexical Model Package Tests

  @Test
  public void test_load_EN_CUSTOM_models() throws Exception {
    JSONObject json = lmPP.loadPackageInfo(tempPkg);
    FileUtils.moveDirectory(tempPkg, TEST_EN_CUSTOM_MODEL_KMP_TARGET);

    Assert.assertNotNull(json);
    String pkgVersion = lmPP.getPackageVersion(json);
    ArrayList<String> languageList = new ArrayList<String>();
    languageList.add("en");

    Map<String, String>[] models = lmPP.processEntry(json.getJSONArray("lexicalModels").getJSONObject(0), "example.en.custom", pkgVersion, languageList);

    HashMap<String, String> en_custom = new HashMap<String, String>();
    en_custom.put(KMManager.KMKey_PackageID, "example.en.custom");
    en_custom.put(KMManager.KMKey_LexicalModelName, "Example (English) Template Custom Model");
    en_custom.put(KMManager.KMKey_LexicalModelID, "example.en.custom");
    en_custom.put(KMManager.KMKey_LexicalModelVersion, "1.0");
    en_custom.put(KMManager.KMKey_LanguageID, "en");
    en_custom.put(KMManager.KMKey_LanguageName, "English");
    en_custom.put(KMManager.KMKey_CustomHelpLink, "");

    Assert.assertEquals(en_custom, models[0]);

    // Lexical model same, but different language pairing
    // Verifies en-US from kmp.json gets lower-cased
    en_custom.put(KMManager.KMKey_LanguageID, "en-us");
    en_custom.put(KMManager.KMKey_LanguageName, "English (US)");

    Assert.assertEquals(en_custom, models[1]);
  }

  @Test
  public void test_kmpProcessLexicalModel() throws Exception {
    List<Map<String, String>> installedModels = lmPP.processKMP(TEST_EN_CUSTOM_MODEL_KMP_FILE, tempPkg, PackageProcessor.PP_LEXICAL_MODELS_KEY);

    Assert.assertEquals(TEST_EN_CUSTOM_MODEL_COUNT, installedModels.size());
  }
}
