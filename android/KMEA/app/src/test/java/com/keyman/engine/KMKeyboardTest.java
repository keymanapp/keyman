package com.keyman.engine;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

@RunWith(RobolectricTestRunner.class)
public class KMKeyboardTest {
  @Test
  public void test_constrain() {
    int[] result;

    // Check "reversed" selection
    Assert.assertArrayEquals(new int[]{4, 8},
      KMKeyboard.constrain(8, 4, 16));

    // Check negative start constrains to 0
    result = KMKeyboard.constrain(-8, 4, 16);
    Assert.assertEquals(0, result[0]);

    // Check negative end constraints to 0
    result = KMKeyboard.constrain(-8, -4, 16);
    Assert.assertEquals(0, result[1]);

    // Check start > length constrains to length
    result = KMKeyboard.constrain(4, 8, 2);
    Assert.assertEquals(2, result[0]);

    // Check end > length constrains to length
    Assert.assertArrayEquals(new int[]{2, 2},
      KMKeyboard.constrain(4, 8, 2));
  }
}
