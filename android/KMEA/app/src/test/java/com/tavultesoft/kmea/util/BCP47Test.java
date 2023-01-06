package com.keyman.engine.util;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import com.keyman.engine.util.BCP47;

@RunWith(RobolectricTestRunner.class)
public class BCP47Test {

  private final String LANGUAGE_ID_1 = "str-latn";
  private final String LANGUAGE_ID_2 = "str-Latn";
  private final String LANGUAGE_ID_3 = "str";

  @Test
  public void test_languageEquals() {
    // Test null language ID
    Assert.assertFalse(BCP47.languageEquals(null, LANGUAGE_ID_2));
    Assert.assertFalse(BCP47.languageEquals(LANGUAGE_ID_1, null));
    Assert.assertFalse(BCP47.languageEquals(null, null));

    Assert.assertFalse(BCP47.languageEquals(LANGUAGE_ID_1, LANGUAGE_ID_3));
    Assert.assertFalse(BCP47.languageEquals(LANGUAGE_ID_2, LANGUAGE_ID_3));

    Assert.assertTrue(BCP47.languageEquals(LANGUAGE_ID_1, LANGUAGE_ID_1));
    Assert.assertTrue(BCP47.languageEquals(LANGUAGE_ID_1, LANGUAGE_ID_2));
  }
}