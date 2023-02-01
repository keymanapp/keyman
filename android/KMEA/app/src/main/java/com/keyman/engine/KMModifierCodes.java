package com.keyman.engine;

import java.util.HashMap;

public final class KMModifierCodes {

  // Note: Keep this table in sync with web/source/text/codes.ts, Codes.modifierCodes.
  final static HashMap<String, Integer> codes = new HashMap<String, Integer>() {{
    put("LCTRL", 0x0001);
    put("RCTRL", 0x0002);
    put("LALT", 0x0004);
    put("RALT", 0x0008);
    put("SHIFT", 0x0010);
    put("CTRL", 0x0020);
    put("ALT", 0x0040);
    put("CAPS", 0x0100);
    put("NO_CAPS", 0x0200);
    put("NUM_LOCK", 0x0400);
    put("NO_NUM_LOCK", 0x0800);
    put("SCROLL_LOCK", 0x1000);
    put("NO_SCROLL_LOCK", 0x2000);
    put("VIRTUAL_KEY", 0x4000);
  }};

  public static Integer get(String key) {
    Integer bitflag = codes.get(key);
    if (bitflag == null) {
      bitflag = 0x0;
    }
    return bitflag;
  };
}
