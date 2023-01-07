package com.keyman.engine.util;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;


import java.util.HashMap;

import static android.os.Build.VERSION_CODES.LOLLIPOP;
import static android.os.Build.VERSION_CODES.N;

@RunWith(RobolectricTestRunner.class)
public class MapCompatTest {
  private final HashMap<String, String> pets = new HashMap<String, String>();

  // Each test sets up a HashMap
  @Before
  public void generatePets() {
    pets.put("dog", "Spot");
    pets.put("cat", "Mittens");
  }

  @Rule
  public ExpectedException exceptionRule = ExpectedException.none();

  @Test
  public void test_getOrDefault_nullMapThrows() throws NullPointerException {
    exceptionRule.expect(NullPointerException.class);

    Assert.assertEquals("Peanut", MapCompat.getOrDefault(null, "elephant", "Peanut"));
  }

  @Config(sdk=LOLLIPOP)
  @Test
  public void test_getOrDefault_LollipopKeyExists() {
    Assert.assertEquals("Spot", MapCompat.getOrDefault(pets, "dog", "none"));
  }

  @Config(sdk=N)
  @Test
  public void test_getOrDefault_NougatKeyExists() {
    Assert.assertEquals("Spot", MapCompat.getOrDefault(pets, "dog", "none"));
  }

  @Config(sdk=LOLLIPOP)
  @Test
  public void test_getOrDefault_LollipopReturnDefault() {
    Assert.assertEquals("Peanut", MapCompat.getOrDefault(pets, "elephant", "Peanut"));
  }

  @Config(sdk=N)
  @Test
  public void test_getOrDefault_NougatReturnDefault() {
    Assert.assertEquals("Peanut", MapCompat.getOrDefault(pets, "elephant", "Peanut"));
  }

}
