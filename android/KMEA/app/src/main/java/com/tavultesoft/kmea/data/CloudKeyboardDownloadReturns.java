package com.tavultesoft.kmea.data;

import java.util.List;
import java.util.Map;

public class CloudKeyboardDownloadReturns {
  public final Integer kbdResult;
  public final List<Map<String, String>> installedLexicalModels;

  public CloudKeyboardDownloadReturns(Integer i) {
    this(i, null);
  }

  public CloudKeyboardDownloadReturns(Integer i, List<Map<String, String>> installedLexicalModels) {
    this.kbdResult = i;
    this.installedLexicalModels = installedLexicalModels;
  }
}
