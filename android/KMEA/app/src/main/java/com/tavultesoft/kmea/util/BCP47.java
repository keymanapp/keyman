/**
 * Copyright (C) 2020 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea.util;

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
}