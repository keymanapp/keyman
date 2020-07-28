package com.tavultesoft.kmea.util;

import java.lang.CharSequence;

public final class CharSequenceUtil {
  private final static String TAG = "CharSeqUtil";

  /**
   * Count the number of surrogate pairs starting from the end of a character sequence
   * until we reach dn codepoints or searched the entire sequence.
   * Doing this the hard way because we can't foreach charSequence in reverse.
   * @param sequence - the character sequence to analyze
   * @param dn - the number of code points to count up to
   * @return int
   */
  public static int countSurrogatePairs(CharSequence sequence, int dn) {
    if ((sequence == null) || (sequence.length() == 0) || (dn <= 0)) {
      return 0;
    }

    int numPairs = 0, counter = 0;
    int lastIndex = sequence.length()-1;
    try {
      do {
        if (Character.isLowSurrogate(sequence.charAt(lastIndex - counter))) {
          numPairs++;
        }
        counter++;
      } while ((lastIndex - counter >= 0) && (counter < dn) && (numPairs < dn));
      return numPairs;
    } catch (Exception e) {
      KMLog.LogException(TAG, "Error in countSurrogatePairs: ", e);
      return numPairs;
    }
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
        int index = expectedCharsString.indexOf(currentContextString);
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

  /**
   * Adjust cursor position after insert the new text.
   * @param charsBefore the character sequence of the inputconnection (2*s.length)
   * @param s the inserted text
   * @return int - number of characters to adjust the cursor
   */
  public static int adjustCursorPosition(CharSequence charsBefore, String s) {
    if (charsBefore == null || s == null) {
      return 0;
    }

    int numPairs = countSurrogatePairs(charsBefore, s.length());
    int _expected_start_index = charsBefore.length() - s.length();
    int _move = 0;
    while (_move < _expected_start_index) {

      CharSequence _check = charsBefore.subSequence(
        _expected_start_index - _move,charsBefore.length()-_move);
      if(_check.equals(s)) {
        break;
      }
      _move++;
    }
    return _move;
  }

}
