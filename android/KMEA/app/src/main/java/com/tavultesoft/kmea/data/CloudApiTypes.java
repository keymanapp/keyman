package com.tavultesoft.kmea.data;

import org.json.JSONArray;
import org.json.JSONObject;

import java.util.List;

public class CloudApiTypes {
  protected static class CloudApiReturns {

    public final ApiTarget target;
    public final JSONArray jsonArray;
    public final JSONObject jsonObject;

    public CloudApiReturns(ApiTarget target, JSONArray jsonArray) {
      this.target = target;
      this.jsonArray = jsonArray;
      this.jsonObject = null;
    }

    public CloudApiReturns(ApiTarget target, JSONObject jsonObject) {
      this.target = target;
      this.jsonArray = null;
      this.jsonObject = jsonObject;
    }

  }

  protected enum ApiTarget {
    Keyboards,
    LexicalModels
  }

  protected enum JSONType {
    Array,
    Object
  }

  protected static class CloudApiParam {
    public final ApiTarget target;
    public final String url;
    public final JSONType type;

    CloudApiParam(ApiTarget target, String url, JSONType type) {
      this.target = target;
      this.url = url;
      this.type = type;
    }
  }

  protected static class CloudDownloadReturns {
    public JSONObject keyboardJSON;
    public JSONArray lexicalModelJSON;

    // Used by the CloudDownloadTask, as it fits well with doInBackground's param structure.
    public CloudDownloadReturns(List<CloudApiReturns> returns) {
      JSONObject kbd = null;
      JSONArray lex = null;

      //TODO: Seems to be wrong because only the last result for each type will be processed
      for(CloudApiTypes.CloudApiReturns ret: returns) {
        switch(ret.target) {
          case Keyboards:
            kbd = ret.jsonObject;
            break;
          case LexicalModels:
            lex = ret.jsonArray;
        }
      }

      // Errors are thrown if we try to do this assignment within the loop.
      this.keyboardJSON = kbd;
      this.lexicalModelJSON = lex;
    }

    public CloudDownloadReturns(JSONObject keyboardJSON, JSONArray lexicalModelJSON) {
      this.keyboardJSON = keyboardJSON;
      this.lexicalModelJSON = lexicalModelJSON;
    }

    public boolean isEmpty() {
      boolean emptyKbd = false;
      boolean emptyLex = false;

      if (keyboardJSON == null) {
        emptyKbd = true;
      } else if (keyboardJSON.length() == 0) {
        emptyKbd = true;
      }

      if(lexicalModelJSON == null) {
        emptyLex = true;
      } else if(lexicalModelJSON.length() == 0) {
        emptyLex = true;
      }

      return emptyKbd && emptyLex;
    }
  }
}
