package com.tavultesoft.kmea.data;

import com.tavultesoft.kmea.JSONParser;
import com.tavultesoft.kmea.KMManager;
import com.tavultesoft.kmea.data.LexicalModel;
import com.tavultesoft.kmea.packages.JSONUtils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.io.File;
import java.util.HashMap;
import java.util.List;

// In Keyman 13.0, this becomes CloudDataJsonUtilTest
@RunWith(RobolectricTestRunner.class)
public class CloudRepositoryTest {
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

  @Before
  public void initializeTestPackage() {
    JSONUtils.initialize(TEST_RESOURCE_ROOT);
  }

  @Test
  public void shouldLowercaseLanguageID() {
    JSONParser parser = new JSONParser();

    // Test processKeyboardJSON()
    JSONObject query = parser.getJSONObjectFromFile(new File(TEST_RESOURCE_ROOT, "bag-Latn.json"));
    List<Keyboard> keyboardResults = CloudRepository.processKeyboardJSON(query, false);
    Keyboard kbInfo = keyboardResults.get(0);
    Assert.assertEquals("bag-latn", kbInfo.getLanguageCode());

    // Test processLexicalModelJSON()
    query = parser.getJSONObjectFromFile(new File(TEST_RESOURCE_ROOT, "sencoten.json"));
    JSONArray cloud = new JSONArray();
    cloud.put(query);
    List<LexicalModel> results = CloudRepository.processLexicalModelJSON(cloud);

    LexicalModel lmInfo = results.get(0);
    Assert.assertEquals("str-latn", lmInfo.getLanguageCode());
  }

}
