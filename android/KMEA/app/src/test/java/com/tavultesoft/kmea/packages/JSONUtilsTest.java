package com.tavultesoft.kmea.packages;


import com.tavultesoft.kmea.JSONParser;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
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
    JSONArray result = JSONUtils.getLanguages();

  }

  @Test
  public void test_findLanguagesID() {
    JSONParser parser = new JSONParser();
    JSONObject kmp = parser.getJSONObjectFromFile(new File(TEST_RESOURCE_ROOT, "galaxie_greek_hebrew_mnemonic" + File.separator + "kmp.json"));

    try {
      JSONArray keyboardsArray = kmp.getJSONArray("keyboards");
      JSONArray languagesArray = keyboardsArray.getJSONObject(0).getJSONArray("languages");
      Assert.assertEquals(-1, JSONUtils.findLanguageID(languagesArray, "am"));
      Assert.assertEquals(0, JSONUtils.findLanguageID(languagesArray, "hbo-Hebr"));

    } catch (JSONException e) {

    }
  }

}

