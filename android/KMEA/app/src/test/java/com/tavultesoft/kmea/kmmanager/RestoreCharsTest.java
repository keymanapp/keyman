package com.tavultesoft.kmea.kmmanager;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import com.tavultesoft.kmea.KMManager;

@RunWith(RobolectricTestRunner.class)
public class RestoreCharsTest {
  // Smiley emoji U+1F600 = D800 DC3D
  private final String SMILEY = "\uD800\uDC3D";

  // Winking emoji U+1F609 = D800 DC3C
  private final String WINK = "\uD800\uDC3C";

  private final String COMPOSING_DOT_ABOVE = "\u0307";
  private final String COMPOSING_CIRCUMFLEX_ACCENT = "\u0302";
  private final String P_COMPOSING_CIRCUMFLEX_ACCENT = "p" + COMPOSING_CIRCUMFLEX_ACCENT;

  @Test
  public void test_invalid_input() {

    // Test invalid input doesn't throw exception
    // null sequence
    CharSequence expectedChars = null;
    CharSequence currentContext = "qwerty";
    CharSequence charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals("", charsToRestore);

    expectedChars = "qwerty";
    currentContext = null;
    charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals("", charsToRestore);

    expectedChars = WINK;
    currentContext = "notamatch";
    charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals("", charsToRestore);
  }

  @Test
  public void test_split_surrogate_pair() {
    CharSequence expectedChars = "o" + P_COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT + WINK + "p";
    CharSequence currentContext = P_COMPOSING_CIRCUMFLEX_ACCENT + "\uD800";
    CharSequence charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals("\uDC3C" + "p", charsToRestore);
  }

  @Test
  public void test_chars() {

    CharSequence expectedChars = "qwertyp" + SMILEY + COMPOSING_DOT_ABOVE;
    CharSequence currentContext = "qwertyp";
    CharSequence charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals(SMILEY + COMPOSING_DOT_ABOVE, charsToRestore);

    expectedChars = "qwerty" + P_COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT;
    currentContext = "qwertyp";
    charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals(COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT, charsToRestore);

    expectedChars = "qwerty" + P_COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT;
    currentContext = "qwerty";
    charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals(P_COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT, charsToRestore);
  }
}