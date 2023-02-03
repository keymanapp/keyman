package com.keyman.engine.cloud.impl;

import com.keyman.engine.cloud.CloudApiTypes;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.List;

/**
 * Result type for catalogue download. Only used for package updates.
 */
public class CloudCatalogDownloadReturns {
  public JSONObject packagesJSON;

  // Used by the CloudCatalogDownloadTask, as it fits well with doInBackground's param structure.
  public CloudCatalogDownloadReturns(List<CloudApiTypes.CloudApiReturns> returns) {
    JSONObject pkg = null;

    //TODO: Seems to be wrong because only the last result for each type will be processed
    for(CloudApiTypes.CloudApiReturns ret: returns) {
      switch(ret.target) {
        case PackageVersion:
          pkg = ret.jsonObject;
          break;
      }
    }

    // Errors are thrown if we try to do this assignment within the loop.
    this.packagesJSON = pkg;
  }

  public CloudCatalogDownloadReturns(JSONObject keyboardJSON, JSONArray lexicalModelJSON, JSONObject packagesJSON) {
    this.packagesJSON = packagesJSON;
  }

  public boolean isEmpty() {
    boolean emptyPkg = packagesJSON == null || packagesJSON.length() == 0;

    return emptyPkg;
  }
}
