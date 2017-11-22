/**
 * Copyright (C) 2017 SIL International. All rights reserved.
 */

package com.tavultesoft.kmea;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URL;

import org.json.JSONException;
import org.json.JSONObject;

import com.tavultesoft.kmea.util.Connection;

import android.util.Log;

final class JSONParser {

  private static InputStream inputStream = null;
  private static JSONObject jsonObj = null;
  private static String jsonStr = "";

  public JSONParser() {
  }

  public JSONObject getJSONObjectFromUrl(String urlStr) {

    try {
      if (Connection.initialize(urlStr)) {
        inputStream = Connection.getInputStream();

        // get charset
        String charSet = null;
        String contentType = Connection.getContentType();
        String[] values = contentType.split(";");
        for (String value : values) {
          value = value.trim();
          if (value.toLowerCase().startsWith("charset=")) {
            charSet = value.substring("charset=".length());
          }
        }

        // if cannot get charset, use utf-8
        if (charSet == null)
          charSet = "utf-8";

        BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, charSet), 4096);
        StringBuilder strBuilder = new StringBuilder();
        String line = null;
        while ((line = reader.readLine()) != null)
          strBuilder.append(line + "\n");
        inputStream.close();
        jsonStr = strBuilder.toString();
      }
      jsonObj = new JSONObject(jsonStr);
    } catch (UnsupportedEncodingException e) {
      Log.e("Encoding Error", (e.getMessage() == null) ? "UnsupportedEncodingException" : e.getMessage());
      jsonObj = null;
    } catch (IOException e) {
      Log.e("IO Error", (e.getMessage() == null) ? "IOException" : e.getMessage());
      jsonObj = null;
    } catch (JSONException e) {
      Log.e("JSON Parser Error", (e.getMessage() == null) ? "JSONException" : e.getMessage());
      jsonObj = null;
    } catch (Exception e) {
      Log.e("JSON Parser Error", (e.getMessage() == null) ? "Exception" : e.getMessage());
      jsonObj = null;
    } finally {
      Connection.disconnect();
    }

    return jsonObj;
  }
}