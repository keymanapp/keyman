package com.tavultesoft.kmea.kmmanager;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import com.tavultesoft.kmea.KMManager;

@RunWith(RobolectricTestRunner.class)
public class CountSurrogatePairsTest {
  // Smiley emoji U+1F600 = D800 DC3D
  private final String SMILEY = "\uD800\uDC3D";

  // Winking emoji U+1F609 = D800 DC3C
  private final String WINK = "\uD800\uDC3C";


  @Test
  public void test_invalid_input() {

    // Test invalid input doesn't throw exception

    // null sequence
    CharSequence sequence = null;
    int numPairs = KMManager.countSurrogatePairs(sequence, 1);
    Assert.assertEquals(0, numPairs);

    // negative dn
    sequence = SMILEY + " Day";
    numPairs = KMManager.countSurrogatePairs(sequence, -1);
    Assert.assertEquals(0, numPairs);

    // dn longer than the sequence length
    numPairs = KMManager.countSurrogatePairs(sequence, sequence.length()+5);
    Assert.assertEquals(1, numPairs);
  }

  @Test
  public void test_zero_pairs() {

    // Test for scenarios that expect 0 surrogate pairs
    CharSequence sequence = "Zero emojis";
    int numPairs = KMManager.countSurrogatePairs(sequence, sequence.length());
    Assert.assertEquals(0, numPairs);

    // dn doesn't reach the surrogate pair
    sequence = "Zero emojis" + SMILEY + "!";
    numPairs = KMManager.countSurrogatePairs(sequence, 0);
    Assert.assertEquals(0, numPairs);

    numPairs = KMManager.countSurrogatePairs(sequence, 1);
    Assert.assertEquals(0, numPairs);
  }

  @Test
  public void test_one_pair() {

    // Test for scenarios that expect 1 surrogate pair
    CharSequence sequence = "Have A " + SMILEY;
    int numPairs = KMManager.countSurrogatePairs(sequence, 1);
    Assert.assertEquals(1, numPairs);

    numPairs = KMManager.countSurrogatePairs(sequence, 2);
    Assert.assertEquals(1, numPairs);

    numPairs = KMManager.countSurrogatePairs(sequence, 6);
    Assert.assertEquals(1, numPairs);

    sequence = "Have A " + SMILEY + WINK + " Day";
    numPairs = KMManager.countSurrogatePairs(sequence, 5);
    Assert.assertEquals(1, numPairs);

    numPairs = KMManager.countSurrogatePairs(sequence, 6);
    Assert.assertEquals(1, numPairs);
  }

  @Test
  public void test_two_pairs() {

    // Test for scenarios that expect 2 surrogate pairs
    CharSequence sequence = "Have A " + SMILEY + WINK + " Day";
    int numPairs = KMManager.countSurrogatePairs(sequence, 7);
    Assert.assertEquals(2, numPairs);

    numPairs = KMManager.countSurrogatePairs(sequence, 8);
    Assert.assertEquals(2, numPairs);
  }
}
