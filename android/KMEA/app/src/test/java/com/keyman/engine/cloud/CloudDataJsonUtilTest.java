package com.keyman.engine.cloud;

import com.keyman.engine.JSONParser;
import com.keyman.engine.KMManager;
import com.keyman.engine.data.LexicalModel;
import com.keyman.engine.packages.JSONUtils;

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

  @Test
  public void cloudLinkIsNewer_false() {
    // Cloud KMP link invalid
    Assert.assertFalse(CloudDataJsonUtil.cloudLinkIsNewer(null, null));
    Assert.assertFalse(CloudDataJsonUtil.cloudLinkIsNewer(null, ""));

    // KMPs don't match
    Assert.assertFalse(CloudDataJsonUtil.cloudLinkIsNewer(
      "https://keyman.com/go/package/download/keyboard/sil_cameroon_azerty?version=6.0.4&update=1",
      "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.8&update=1"));

    // Cloud KMP link is older or equal
    Assert.assertFalse(CloudDataJsonUtil.cloudLinkIsNewer(
      "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.8&update=1",
      "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.4&update=1"));

    Assert.assertFalse(CloudDataJsonUtil.cloudLinkIsNewer(
      "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.8&update=1",
      "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.8&update=1"));
  }

  @Test
  public void cloudLinkIsNewer_true() {
    // updateKMP link is null or empty
    Assert.assertTrue(CloudDataJsonUtil.cloudLinkIsNewer(
      null, "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.8&update=1"));
    Assert.assertTrue(CloudDataJsonUtil.cloudLinkIsNewer(
      "", "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.8&update=1"));

    // Cloud KMP link is newer
    Assert.assertTrue(CloudDataJsonUtil.cloudLinkIsNewer(
      "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.4&update=1",
      "https://keyman.com/go/package/download/keyboard/sil_cameroon_qwerty?version=6.0.8&update=1"));
  }
}
