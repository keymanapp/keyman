package com.tavultesoft.kmea.cloud;

import com.tavultesoft.kmea.JSONParser;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.LexicalModel;
import com.tavultesoft.kmea.packages.JSONUtils;

import org.json.JSONArray;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.io.File;
import java.util.HashMap;
import java.util.List;

@RunWith(RobolectricTestRunner.class)
public class CloudDataJsonUtilTest {
  private static final File TEST_RESOURCE_ROOT = new File("test_resources", "cloud");
  private static final String TAG = "CloudDataJsonUtilTest";

  private final String pkgID = "sil_cameroon_qwerty";
  private final String langID = "bag-Latn";
  private final String langName = "Tuki (Latin)";
  private final String keyboardID = "sil_cameroon_qwerty";
  private final String keyboardName = "Cameroon QWERTY";
  private final String keyboardVersion = "6.0.1";
  private final String customKeyboard = "N";
  private final String  aFont = "AndikaAfr";
  private final String oskFont = aFont;
  private final String customHelpLink = "";

  @Before
  public void initializeTestPackage() {
    JSONUtils.initialize(TEST_RESOURCE_ROOT);
  }

  @Test
  public void shouldLowercaseLanguageID() {
    // Test createKeyboardInfoMap() used by processKeyboardJSON()
    HashMap<String, String> kbInfo = CloudDataJsonUtil.createKeyboardInfoMap(
      pkgID, langID, langName, keyboardID, keyboardName, keyboardVersion, aFont, oskFont, customHelpLink);
    Assert.assertEquals(langID.toLowerCase(), kbInfo.get(KMManager.KMKey_LanguageID));

    // Test processLexicalModelJSON()
    JSONParser parser = new JSONParser();
    JSONArray cloud = parser.getJSONObjectFromFile(new File(TEST_RESOURCE_ROOT, "sencoten.json"), JSONArray.class);
      List<LexicalModel> results = CloudDataJsonUtil.processLexicalModelJSON(cloud, true);

    LexicalModel lmInfo = results.get(0);
    Assert.assertEquals("str-latn", lmInfo.getLanguageID());
  }

}
