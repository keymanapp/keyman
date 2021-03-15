package com.tavultesoft.kmea.util;

import org.checkerframework.common.value.qual.StaticallyExecutable;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;
import org.robolectric.shadows.ShadowLog;

import com.tavultesoft.kmea.packages.LexicalModelPackageProcessor;
import com.tavultesoft.kmea.util.MapCompat;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;

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

  @Test
  public void test_getOrDefault_nullMapReturnsDefault() {
    Assert.assertEquals("Peanut", MapCompat.getOrDefault(null, "elephant", "Peanut"));

    // Check shadow log error msg
    List<ShadowLog.LogItem> logs = ShadowLog.getLogsForTag("MapCompat");
    Assert.assertEquals(1, logs.size());
    Assert.assertEquals("MapCompat", logs.get(0).tag);
    Assert.assertEquals("HashMap is null. Returning Peanut", logs.get(0).msg);
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
