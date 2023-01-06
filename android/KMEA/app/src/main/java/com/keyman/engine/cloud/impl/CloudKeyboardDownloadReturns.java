package com.keyman.engine.cloud.impl;

import java.util.List;
import java.util.Map;

/**
 * key board and lexical model download result.
 */
public class CloudKeyboardDownloadReturns {
  public final Integer kbdResult;
  public final List<Map<String, String>> installedResource;

  public CloudKeyboardDownloadReturns(Integer i) {
    this(i, null);
  }

  public CloudKeyboardDownloadReturns(Integer i, List<Map<String, String>> installedResource) {
    this.kbdResult = i;
    this.installedResource = installedResource;
  }
}
