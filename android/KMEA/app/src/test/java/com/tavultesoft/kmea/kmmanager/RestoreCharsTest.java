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

    expectedChars = "qwerty";
    currentContext = "notamatch";
    charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals("", charsToRestore);
  }

  @Test
  public void test_chars() {

    CharSequence expectedChars = "qwertyp" + SMILEY + COMPOSING_DOT_ABOVE;
    CharSequence currentContext = "qwertyp";
    CharSequence charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals(SMILEY + COMPOSING_DOT_ABOVE, charsToRestore);

    expectedChars = "qwertyp" + COMPOSING_CIRCUMFLEX_ACCENT + "p" + COMPOSING_CIRCUMFLEX_ACCENT;
    currentContext = "qwertyp";
    charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals(COMPOSING_CIRCUMFLEX_ACCENT + "p" + COMPOSING_CIRCUMFLEX_ACCENT, charsToRestore);

    expectedChars = "qwertyp" + COMPOSING_CIRCUMFLEX_ACCENT + "p" + COMPOSING_CIRCUMFLEX_ACCENT;
    currentContext = "qwerty";
    charsToRestore = KMManager.restoreChars(expectedChars, currentContext);
    Assert.assertEquals("p" + COMPOSING_CIRCUMFLEX_ACCENT + "p" + COMPOSING_CIRCUMFLEX_ACCENT, charsToRestore);
  }
}