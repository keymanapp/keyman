package com.keyman.engine.util;

import java.lang.CharSequence;

public final class CharSequenceUtil {
  private final static String TAG = "CharSeqUtil";

  /**
   * Count the number of surrogate pairs starting from the end of a character sequence
   * until we reach dn codepoints or searched the entire sequence.
   * Doing this the hard way because we can't foreach charSequence in reverse.
   * Does not include split-surrogate pairs.
   * @param sequence - the character sequence to analyze
   * @param dn - the number of code points to count up to
   * @return int
   */
  public static int countSurrogatePairs(CharSequence sequence, int dn) {
    if ((sequence == null) || (sequence.length() == 0) || (dn <= 0)) {
      return 0;
    }

    int index = sequence.length()-1, dnx = dn, numPairs = 0;
    while(index > 0 && dnx > 0) {
      if(Character.isLowSurrogate(sequence.charAt(index)) &&
        Character.isHighSurrogate(sequence.charAt(index-1))) {
        numPairs++;
        index--;
      }
      index--;
      dnx--;
    }
    return numPairs;
  }

  /**
   * Determine a character sequence that needs to be re-inserted. If currentContext is a
   * subSequence of expectedChars, returns the character sequence that needs to be restored.
   * @param expectedChars - expected character sequence
   * @param currentContext - current character sequence
   * @return CharSequence - Character sequence that will need to be appended to currentContext.
   *                        Empty string if no characters need to be restored.
   */
  public static CharSequence restoreChars(CharSequence expectedChars, CharSequence currentContext) {
    CharSequence charsToRestore = "";

    try {
      // Now see if we need to re-insert characters
      if (expectedChars.length() != currentContext.length()) {
        String expectedCharsString = expectedChars.toString();
        String currentContextString = currentContext.toString();
        int index = expectedCharsString.lastIndexOf(currentContextString);
        if (currentContextString.length() == 0) {
          index = 0;
        }
        if (index > -1) {
          // subSequence indices are start(inclusive) to end(exclusive)
          charsToRestore = expectedChars.subSequence(index + currentContextString.length(), expectedChars.length());
        }
      }
      return charsToRestore;
    } catch (Exception e) {
      KMLog.LogException(TAG, "Error in restoreChars: ", e);
      return charsToRestore;
    }
  }
}
