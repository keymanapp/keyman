package com.tavultesoft.kmea.cloud.impl;

import com.tavultesoft.kmea.cloud.CloudApiTypes;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.List;

/**
 * Result type for catalogue download.
 */
public class CloudCatalogDownloadReturns {
  public JSONObject keyboardJSON; // TODO: remove
  public JSONArray lexicalModelJSON;
  public JSONObject packagesJSON;

  // Used by the CloudCatalogDownloadTask, as it fits well with doInBackground's param structure.
  public CloudCatalogDownloadReturns(List<CloudApiTypes.CloudApiReturns> returns) {
    JSONObject kbd = null;
    JSONArray lex = null;
    JSONObject pkg = null;

    //TODO: Seems to be wrong because only the last result for each type will be processed
    for(CloudApiTypes.CloudApiReturns ret: returns) {
      switch(ret.target) {
        case Keyboards:
          kbd = ret.jsonObject;
          break;
        case LexicalModels:
          lex = ret.jsonArray;
          break;
        case PackageVersion:
          pkg = ret.jsonObject;
          break;
      }
    }

    // Errors are thrown if we try to do this assignment within the loop.
    this.keyboardJSON = kbd;
    this.lexicalModelJSON = lex;
    this.packagesJSON = pkg;
  }

  public CloudCatalogDownloadReturns(JSONObject keyboardJSON, JSONArray lexicalModelJSON, JSONObject packagesJSON) {
    this.keyboardJSON = keyboardJSON;
    this.lexicalModelJSON = lexicalModelJSON;
    this.packagesJSON = packagesJSON;
  }

  public boolean isEmpty() {
    boolean emptyKbd = keyboardJSON == null || keyboardJSON.length() == 0;
    boolean emptyLex = lexicalModelJSON == null || lexicalModelJSON.length() == 0;
    boolean emptyPkg = packagesJSON == null || packagesJSON.length() == 0;

    return emptyKbd && emptyLex && emptyPkg;
  }
}
