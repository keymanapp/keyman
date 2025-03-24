package com.keyman.engine.packages;

import android.util.Log;

import com.keyman.engine.JSONParser;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import java.io.File;


@RunWith(RobolectricTestRunner.class)
public class JSONUtilsTest {
  private static final File TEST_RESOURCE_ROOT = new File("test_resources", "packages");
  private static final File TEST_INVALID_RESOURCE_ROOT = new File("test_resources", "invalid_packages");
  private static final String TAG = "JSONUtilsTest";

  @Before
  public void initializeTestPackage() {
    JSONUtils.initialize(TEST_RESOURCE_ROOT);
  }

  /**
   * Test parsing galaxie_greek_hebrew_mnemonic/kmp.json and sil_cameroon_qwerty/kmp.json
   * They've been modified so the language Afade (Latin) - "aal-Latn" is associated with keyboards
   * galaxie_hebrew_mnemonic and sil_cameroon_qwerty
   */
  @Test
  public void test_getLanguages() {
    JSONArray result = JSONUtils.getLanguages();
    // galaxie_greek_hebrew_mnemonic: 5
    // sil_cameroon_qwerty: 279
    // intersection of "aal-Latn"
    final int EXPECTED_NUM_LANGUAGES = 283;

    Assert.assertEquals(EXPECTED_NUM_LANGUAGES, result.length());

    // Verify "aal-Latn" has a merge of 2 keyboards
    int index = JSONUtils.findID(result, "aal-Latn");
    try {
      JSONObject alfade = result.getJSONObject(index);
      JSONArray keyboardsArray = alfade.getJSONArray("keyboards");
      Assert.assertEquals(2, keyboardsArray.length());
    } catch (JSONException e) {
      Log.e(TAG, "test_getLanguages error: " + e);
    }
  }

  /**
   * Test parsing null_languages/kmp.json which has no languages
   */
  @Test
  public void test_invalid_getLanguages() {
    JSONUtils.initialize(TEST_INVALID_RESOURCE_ROOT);
    JSONArray result = JSONUtils.getLanguages();
    final int EXPECTED_NUM_LANGUAGES = 0;

    Assert.assertEquals(EXPECTED_NUM_LANGUAGES, result.length());
  }

  @Test
  public void test_findID() {
    JSONParser parser = new JSONParser();
    JSONObject kmp = parser.getJSONObjectFromFile(new File(TEST_RESOURCE_ROOT, "sil_cameroon_qwerty" + File.separator + "kmp.json"));
    final int EXPECTED_NUM_LANGUAGES = 279;

    try {
      JSONArray keyboardsArray = kmp.getJSONArray("keyboards");
      JSONArray languagesArray = keyboardsArray.getJSONObject(0).getJSONArray("languages");
      Assert.assertEquals(EXPECTED_NUM_LANGUAGES, languagesArray.length());

      // Verify Amharic not in the list of languages for sil_cameroon_qwerty
      Assert.assertEquals(-1, JSONUtils.findID(languagesArray, "am"));
      Assert.assertEquals(-1, JSONUtils.findID(null, "aal-Latn"));

      // Verify first and last language for sil_cameroon_qwerty
      Assert.assertEquals(0, JSONUtils.findID(languagesArray, "aal-Latn"));
      Assert.assertEquals(0, JSONUtils.findID(languagesArray, "aal-latn"));
      Assert.assertEquals(EXPECTED_NUM_LANGUAGES-1, JSONUtils.findID(languagesArray, "zuy-Latn"));

    } catch (JSONException e) {
      Log.e(TAG, "test_findID error: " + e);
    }
  }

  @Test
  public void test_findHelp() {
    JSONParser parser = new JSONParser();
    JSONObject kmp = parser.getJSONObjectFromFile(new File(TEST_RESOURCE_ROOT, "sil_cameroon_qwerty" + File.separator + "kmp.json"));

    try {
      JSONArray filesArray = kmp.getJSONArray("files");
      Assert.assertTrue(JSONUtils.findHelp(filesArray));
    } catch (JSONException e) {
      Log.e(TAG, "findHelp error: " + e);
    }
  }
}

