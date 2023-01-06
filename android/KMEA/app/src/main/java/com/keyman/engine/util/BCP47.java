/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.keyman.engine.util;

import java.util.ArrayList;
import java.security.InvalidParameterException;

public final class BCP47 {
  /**
   * Utility to compare two language ID strings
   * @param id1 Language ID 1
   * @param id2 Language ID 2
   * @return true if the two strings are case-insensitive equal
   */
  public static boolean languageEquals(String id1, String id2) {
    if (id1 == null || id2 == null) {
      return false;
    }

    return id1.equalsIgnoreCase(id2);
  }

  /**
   * Utility to modify languageList.
   * If languageID exists in the list, remove it. Otherwise, add languageID to the list.
   * @param languageList
   * @param languageID
   */
  public static void toggleLanguage(ArrayList<String> languageList, String languageID) {
    if (languageList == null) {
      throw new InvalidParameterException("languageList must not be null");
    }
    if (languageID == null) {
      throw new InvalidParameterException("languageID must not be null");
    }

    // See if languageID already exists in the languageList
    for (String l: languageList) {
      if (languageEquals(l, languageID)) {
        languageList.remove(l);
        return;
      }
    }

    languageList.add(languageID.toLowerCase());
  }
}
