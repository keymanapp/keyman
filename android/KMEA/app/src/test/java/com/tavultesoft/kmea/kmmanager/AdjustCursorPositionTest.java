package com.tavultesoft.kmea.kmmanager;

import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;

import com.tavultesoft.kmea.KMManager;

@RunWith(RobolectricTestRunner.class)
public class AdjustCursorPositionTest {
  // Smiley emoji U+1F600 = D800 DC3D
  private final String SMILEY = "\uD800\uDC3D";

  // Winking emoji U+1F609 = D800 DC3C
  private final String WINK = "\uD800\uDC3C";

  private final String COMPOSING_DOT_ABOVE = "\u0307";
  private final String COMPOSING_CIRCUMFLEX_ACCENT = "\u0302";
  private final String P_COMPOSING_CIRCUMFLEX_ACCENT = "p" + COMPOSING_CIRCUMFLEX_ACCENT;

  @Test
  public void test_invalid_input() {
    CharSequence charsBefore = null;
    String s = P_COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT;
    int move = KMManager.adjustCursorPosition(charsBefore, s);
    Assert.assertEquals(0, move);
  }

  @Test
  public void test_move_3() {
    CharSequence charsBefore = "o" + P_COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT +
      SMILEY + "q";
    String s = P_COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT;

    int move = KMManager.adjustCursorPosition(charsBefore, s);
    Assert.assertEquals(3, move);

    charsBefore = "g" + P_COMPOSING_CIRCUMFLEX_ACCENT + P_COMPOSING_CIRCUMFLEX_ACCENT + "lyf";
    move = KMManager.adjustCursorPosition(charsBefore, s);
    Assert.assertEquals(3, move);

  }

}